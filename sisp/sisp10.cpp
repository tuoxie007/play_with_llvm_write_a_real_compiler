//
//  sisp10.cpp
//  sisp
//
//  Created by 徐可 on 2020/2/17.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#include "SispJIT04.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>

using namespace std;
using namespace llvm;
#define make_unique std::make_unique

static bool JITEnabled = false;

typedef enum Token {
    tok_eof = -1,
    tok_def = -2,
    tok_extern = -3,
    tok_identifier = -4,
    tok_number = -5,
    tok_if = -6,
    tok_then = -7,
    tok_else = -8,
    tok_for = -9,
    tok_in = -10,
    tok_binary = -11,
    tok_unary = -12,
    tok_var = -13,

    tok_left_paren = '(',
    tok_right_paren = ')',
    tok_equal = '=',
    tok_less = '<',
    tok_great = '>',
    tok_comma = ',',
    tok_colon = ';',
    tok_hash = '#',
    tok_dot = '.',
    tok_space = ' ',
    tok_left_bracket = '{',
    tok_right_bracket = '}',

    tok_add = '+',
    tok_sub = '-',
    tok_mul = '*',
    tok_div = '/',
    tok_not = '!',
    tok_or = '|',
    tok_and = '&',

} Token;


static map<char, int> BinOpPrecedence;

static string IdentifierStr;
static double NumVal;
static string TheCode;

struct SourceLocation {
    int Line;
    int Col;
};
static SourceLocation CurLoc;
static SourceLocation LexLoc = {1, 0};

static int GetChar() {
    static string::size_type Index = 0;
    if (Index >= TheCode.length())
        return EOF;
    char CurChar = TheCode.at(Index++);
    cout << "getchar [" << string(1, CurChar) << "]" << endl;

    if (CurChar == '\n' || CurChar == '\r') {
        LexLoc.Line++;
        LexLoc.Col = 0;
    } else {
        LexLoc.Col++;
    }

    return CurChar;
}

//#define GetChar getchar

static char LastChar = (char)tok_space;

static int gettok() {
    while (isspace(LastChar)) {
        LastChar = GetChar();
    }

    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum(LastChar = GetChar())) {
            IdentifierStr += LastChar;
        }

        if (IdentifierStr == "def") {
            return tok_def;
        }
        if (IdentifierStr == "extern")
            return tok_extern;
        if (IdentifierStr == "exit")
            exit(0);
        if (IdentifierStr == "if")
            return tok_if;
        if (IdentifierStr == "then")
            return tok_then;
        if (IdentifierStr == "else")
            return tok_else;
        if (IdentifierStr == "for")
            return tok_for;
        if (IdentifierStr == "in")
            return tok_in;
        if (IdentifierStr == "unary")
            return tok_unary;
        if (IdentifierStr == "binary")
            return tok_binary;
        if (IdentifierStr == "var")
            return tok_var;

        return tok_identifier;
    }

    if (isdigit(LastChar) || LastChar == tok_dot) {
        string NumStr;
        do {
            NumStr += LastChar;
            LastChar = GetChar();
        } while (isdigit(LastChar) || LastChar == tok_dot);

        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    if (LastChar == tok_hash) {
        do {
            LastChar = GetChar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF) {
            return gettok();
        }
    }

    if (LastChar == EOF) {
        return tok_eof;
    }

    char ThisChar = LastChar;
    LastChar = GetChar();
    return ThisChar;
}

class Scope {
    map<string, unique_ptr<ExprAST>> Vars;
    map<string, Value *> VarVals;

public:
    shared_ptr<Scope> Parent;

    Scope() {}
    Scope(shared_ptr<Scope> parent): Parent(parent) {}

    void append(pair<string, unique_ptr<ExprAST>> V) {
        Vars.insert(move(V));
    }
    void setVal(string var, Value * val) {
        VarVals[var] = val;
    }
    Value *getVal(string var) {
        return VarVals[var] ?: (Parent ? Parent->getVal(var) : nullptr);
    }
};

class ExprAST {
    SourceLocation Loc;

protected:
    shared_ptr<Scope> scope;

public:
    ExprAST(shared_ptr<Scope> scope, SourceLocation Loc = CurLoc) : scope(scope), Loc(Loc) {}
    virtual ~ExprAST() {}
    virtual Value *codegen() = 0;
    int getLine() const { return Loc.Line; };
    int getCol() const { return Loc.Col; }
    virtual raw_ostream &dump(raw_ostream &out, int ind) {
        return out << ":" << getLine() << ":" << getCol() << "\n";
    }
    shared_ptr<Scope> getScope() const { return scope; }
    void setScope(shared_ptr<Scope> newScope) { scope = newScope; }

    virtual void dumpAST() {
        cout << "<<Expr>>" << "\n";
    }
};

static unique_ptr<ExprAST> ParseExpr(shared_ptr<Scope> scope);

class VarExprAST : public ExprAST {
    vector<pair<string, unique_ptr<ExprAST>>> VarNames;
//    unique_ptr<ExprAST> Body;

public:
    VarExprAST(shared_ptr<Scope> scope, vector<pair<string, unique_ptr<ExprAST>>> varNames
//               , unique_ptr<ExprAST> body
               )
        : ExprAST(scope), VarNames(move(varNames)) {}
//    , Body(move(body)) {}

    Value *codegen() override;
    vector<pair<string, unique_ptr<ExprAST>>> getVars() { return move(VarNames); }
    void dumpAST() override {
        cout << "<<VarExprAST>>" << "\n";
    }
};

class CompoundExprAST : public ExprAST {
    vector<unique_ptr<ExprAST>> Exprs;
//    std::map<std::string, std::unique_ptr<ExprAST>> Scope;

public:
    CompoundExprAST(shared_ptr<Scope> scope, vector<unique_ptr<ExprAST>> exprs): ExprAST(scope), Exprs(move(exprs)) {
//        scope = std::make_shared<Scope>();
//        for (auto E = Exprs.begin(); E != Exprs.end(); E++) {
//            if (auto VarE = dynamic_cast<VarExprAST *>((*E).get())) {
//                auto Vars = VarE->getVars();
//                for (auto V = Vars.begin(); V != Vars.end(); V ++) {
//                    scope->append(move(*V));
//                }
//            } else {
//                (*E)->setScope(scope);
//            }
//        }
    }
    Value *codegen() override;
//    void addNameToScope(std::string name, std::unique_ptr<ExprAST> init) ;
    void dumpAST() override {
        cout << "<<CompoundExprAST>>" << "\n";
    }
};

class NumberExprAST : public ExprAST {
    double Val;

public:
    NumberExprAST(shared_ptr<Scope> scope, double val): ExprAST(scope), Val(val) {}
    Value *codegen() override;
    void dumpAST() override {
        cout << "<<NumberExprAST>>" << "\n";
    }
};

class VariableExprAST : public ExprAST {
    string Name;

public:
    VariableExprAST(shared_ptr<Scope> scope, SourceLocation loc, const string &name) : ExprAST(scope, loc), Name(name) {}
    Value *codegen() override;
    string &getName() { return Name; }
    void dumpAST() override {
        cout << "<<VariableExprAST>>" << "\n";
    }
};

class BinaryExprAST : public ExprAST {
    char Op;
    unique_ptr<ExprAST> LHS, RHS;

public:
    BinaryExprAST(shared_ptr<Scope> scope,
                  SourceLocation loc,
                  char op,
                  unique_ptr<ExprAST> lhs,
                  unique_ptr<ExprAST> rhs)
        : ExprAST(scope, loc), Op(op), LHS(move(lhs)), RHS(move(rhs)) {}
    Value *codegen() override;
    void dumpAST() override {
        cout << "<<BinaryExprAST>>" << "\n";
    }
};

class CallExprAST : public ExprAST {
    string Callee;
    vector<unique_ptr<ExprAST>> Args;

public:
    CallExprAST(shared_ptr<Scope> scope,
                SourceLocation loc,
                const string &callee,
                vector<unique_ptr<ExprAST>> args)
        : ExprAST(scope, loc), Callee(callee), Args(move(args)) {}
    Value *codegen() override;
    void dumpAST() override {
        cout << "<<CallExprAST>>" << "\n";
    }
};

class PrototypeAST {
    SourceLocation Loc;
    string Name;
    vector<string> Args;
    bool IsOperator;
    unsigned Precedence;

public:
    PrototypeAST(SourceLocation loc,
                 string &name,
                 vector<string> args,
                 bool isOperator = false,
                 unsigned precedence = 0)
        :
        Loc(loc),
        Name(name),
        Args(move(args)),
        IsOperator(isOperator),
        Precedence(precedence) {}

    Function *codegen();

    const string &getName() const { return Name; }
    bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
    bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

    char getOperatorName() const {
        assert(isUnaryOp() || isBinaryOp());
        return Name[Name.size()-1];
    }

    unsigned getBinaryPrecedence() const { return Precedence; }
    int getLine() const { return Loc.Line; };
    int getCol() const { return Loc.Col; }
    void dumpAST() {
        cout << "<<PrototypeAST>>" << "\n";
    }
};

class IfExprAST : public ExprAST {
    unique_ptr<ExprAST> Cond, Then, Else;

public:
    IfExprAST(shared_ptr<Scope> scope,
              SourceLocation loc,
              unique_ptr<ExprAST> cond,
              unique_ptr<ExprAST> then,
              unique_ptr<ExprAST> elseE)
        : ExprAST(scope, loc), Cond(move(cond)), Then(move(then)), Else(move(elseE)) {}

    Value * codegen() override;
    void dumpAST() override {
        cout << "<<IfExprAST>>" << "\n";
    }
};

class ForExprAST : public ExprAST {
    string VarName;
    unique_ptr<ExprAST> Start, End, Step, Body;

public:
    ForExprAST(shared_ptr<Scope> scope,
               string &varName,
               unique_ptr<ExprAST> start,
               unique_ptr<ExprAST> end,
               unique_ptr<ExprAST> step,
               unique_ptr<ExprAST> body):
        ExprAST(scope), VarName(varName), Start(move(start)), End(move(end)), Step(move(step)), Body(move(body)) {}

    Value * codegen() override;
    void dumpAST() override {
        cout << "<<ForExprAST>>" << "\n";
    }
};

class UnaryExprAST : public ExprAST {
    char Opcode;
    unique_ptr<ExprAST> Operand;

public:
    UnaryExprAST(shared_ptr<Scope> scope, char opcode, unique_ptr<ExprAST> operand)
        : ExprAST(scope), Opcode(opcode), Operand(move(operand)) {}

    Value * codegen() override;
    void dumpAST() override {
        cout << "<<UnaryExprAST>>" << "\n";
    }
};

static Token CurTok;
static int getNextToken() {
    return CurTok = (Token)gettok();
}

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static unique_ptr<Module> TheModule;
static map<string, AllocaInst *> NamedValues;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<llvm::orc::SispJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static std::unique_ptr<DIBuilder> DBuilder;

struct DebugInfo {
    DICompileUnit *TheCU;
    DIType *DblTy;
    std::vector<DIScope *> LexicalBlocks;

    DIType *getDoubleTy();
    void emitLocation(ExprAST *AST);
} SispDbgInfo;

vector<DIScope *> LexicalBlocks;

//static int advance() {
//  int LastChar = getchar();
//
//  if (LastChar == '\n' || LastChar == '\r') {
//    LexLoc.Line++;
//    LexLoc.Col = 0;
//  } else
//    LexLoc.Col++;
//  return LastChar;
//}

DIType *DebugInfo::getDoubleTy() {
    if (DblTy)
        return DblTy;

    DblTy = DBuilder->createBasicType("double", 64, dwarf::DW_ATE_float);
    return DblTy;
}

void DebugInfo::emitLocation(ExprAST *AST) {
    if (!AST)
        return Builder.SetCurrentDebugLocation(DebugLoc());

    DIScope *Scope;
    if (LexicalBlocks.empty())
        Scope = TheCU;
    else
        Scope = LexicalBlocks.back();

    Builder.SetCurrentDebugLocation(DebugLoc::get(AST->getLine(), AST->getCol(), Scope));
}

static DISubroutineType *CreateFunctionType(unsigned long NumArgs, DIFile *Unit) {
  SmallVector<Metadata *, 8> EltTys;
  DIType *DblTy = SispDbgInfo.getDoubleTy();

  // Add the result type.
  EltTys.push_back(DblTy);

  for (unsigned long i = 0, e = NumArgs; i != e; ++i)
      EltTys.push_back(DblTy);

  return DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(EltTys));
}

static unique_ptr<ExprAST> LogError(const char *Str) {
    cerr << "LogError: " << Str << endl;
    assert(false && Str);
    return nullptr;
}

static unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
    LogError(Str);
    return nullptr;
}

Value *LogErrorV(const char *Str) {
    LogError(Str);
    return nullptr;
}

static AllocaInst *CreateEntryBlockAlloca(Function *F,
                                          const string &VarName) {
    IRBuilder<> TmpBlock(&F->getEntryBlock(),
                         F->getEntryBlock().begin());
    return TmpBlock.CreateAlloca(Type::getDoubleTy(TheContext), 0, VarName.c_str());
}

static Function *getFunction(std::string Name) {
    // First, see if the function has already been added to the current module.
    if (auto *F = TheModule->getFunction(Name))
        return F;

    // If not, check whether we can codegen the declaration from some existing
    // prototype.
    auto FI = FunctionProtos.find(Name);
    if (FI != FunctionProtos.end())
        return FI->second->codegen();

    // If no existing prototype exists, return null.
    return nullptr;
}

const std::string& FunctionAST::getName() const {
    return Proto->getName();
}

//void CompoundExprAST::addNameToScope(string name, unique_ptr<ExprAST> init) {
//    if (Scope.find(name) != Scope.end())
//        return;
//    Scope[name] = move(init);
//}

Value *NumberExprAST::codegen() {
    SispDbgInfo.emitLocation(this);
    return ConstantFP::get(TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen() {
    Value *V = scope->getVal(Name);
    if (!V)
        LogError("Unkown variable name");

    SispDbgInfo.emitLocation(this);
    return Builder.CreateLoad(V, Name.c_str());
}

Value *BinaryExprAST::codegen() {
    SispDbgInfo.emitLocation(this);

    if (Op == tok_equal) {
        auto LHSE = static_cast<VariableExprAST *>(LHS.get());
        if (!LHSE)
            return LogErrorV("destination of '=' must be a variable");

        auto Val = RHS->codegen();
        if (!Val)
            return nullptr;

        auto Variable = scope->getVal(LHSE->getName());
        if (!Variable)
            return LogErrorV("Unkown variable name");

        Builder.CreateStore(Val, Variable);
        return Val;
    }

    auto L = LHS->codegen();
    auto R = RHS->codegen();
    if (!L || !R) {
        LogError("BinaryExpr codgen error.");
        return nullptr;
    }

    switch (Op) {
        case tok_add:
            return Builder.CreateFAdd(L, R, "addtmp");
        case tok_sub:
            return Builder.CreateFSub(L, R, "subtmp");
        case tok_mul:
            return Builder.CreateFMul(L, R, "multmp");
        case tok_less:
            L = Builder.CreateFCmpULT(L, R, "cmptmp");
            return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext));
        default:
        {
            auto F = getFunction(string("binary") + Op);
            assert(F && "binary operator not found!");
            auto Ops = { L, R };
            return Builder.CreateCall(F, Ops, "calltmp");
        }
    }
}

Value *IfExprAST::codegen() {
    SispDbgInfo.emitLocation(this);
    auto CondV = Cond->codegen();
    if (!CondV)
        return nullptr;

    CondV = Builder.CreateFCmpONE(CondV, ConstantFP::get(TheContext, APFloat(0.0)), "ifcond");

    auto F = Builder.GetInsertBlock()->getParent();

    auto ThenBlock = BasicBlock::Create(TheContext, "then", F);
    auto ElseBlock = BasicBlock::Create(TheContext, "else");
    auto MergeBlock = BasicBlock::Create(TheContext, "ifcont");

    Builder.CreateCondBr(CondV, ThenBlock, ElseBlock);

    Builder.SetInsertPoint(ThenBlock);

    auto ThenV = Then->codegen();
    if (!ThenV)
        return nullptr;

    Builder.CreateBr(MergeBlock);

    ThenBlock = Builder.GetInsertBlock();

    F->getBasicBlockList().push_back(ElseBlock);
    Builder.SetInsertPoint(ElseBlock);

    auto ElseV = Else->codegen();
    if (!ElseV)
        return nullptr;

    Builder.CreateBr(MergeBlock);

    ElseBlock = Builder.GetInsertBlock();

    F->getBasicBlockList().push_back(MergeBlock);
    Builder.SetInsertPoint(MergeBlock);

    auto PN = Builder.CreatePHI(Type::getDoubleTy(TheContext), 2, "iftmp");

    PN->addIncoming(ThenV, ThenBlock);
    PN->addIncoming(ElseV, ElseBlock);

    return PN;
}

Value *ForExprAST::codegen() {
    auto StartValue = Start->codegen();
    if (!StartValue)
        return nullptr;

    auto F = Builder.GetInsertBlock()->getParent();

    auto Alloca = CreateEntryBlockAlloca(F, VarName);

    SispDbgInfo.emitLocation(this);

    auto StartVal = Start->codegen();
    if (!Start)
        return nullptr;

    Builder.CreateStore(StartVal, Alloca);

    auto LoopBlock = BasicBlock::Create(TheContext, "loop", F);
    Builder.CreateBr(LoopBlock);
    Builder.SetInsertPoint(LoopBlock);

//    auto OldVal = NamedValues[VarName];
//    NamedValues[VarName] = Alloca;
    getScope()->setVal(VarName, Alloca);

    if (!Body->codegen())
        return nullptr;

    Value *StepVal = nullptr;
    if (Step) {
        StepVal = Step->codegen();
        if (!StepVal)
            return nullptr;
    } else {
        StepVal = ConstantFP::get(TheContext, APFloat(1.0));
    }

    auto EndCond = End->codegen();
    if (!EndCond)
        return nullptr;

    auto CurVar = Builder.CreateLoad(Alloca, VarName.c_str());
    auto NextVar = Builder.CreateFAdd(CurVar, StepVal, "nextvar");
    Builder.CreateStore(NextVar, Alloca);

    EndCond = Builder.CreateFCmpONE(EndCond, ConstantFP::get(TheContext, APFloat(0.0)), "loopcond");

    auto AfterBlock = BasicBlock::Create(TheContext, "afterloop", F);
    Builder.CreateCondBr(EndCond, LoopBlock, AfterBlock);
    Builder.SetInsertPoint(AfterBlock);

//    if (OldVal)
//        NamedValues[VarName] = OldVal;
//    else
//        NamedValues.erase(VarName);

    return Constant::getNullValue(Type::getDoubleTy(TheContext));
}

Value *VarExprAST::codegen() {
//    vector<AllocaInst *> OldBindings;

    auto F = Builder.GetInsertBlock()->getParent();

    for (unsigned long i = 0, e = VarNames.size(); i != e; i++) {
        string &VarName = VarNames[i].first;
        auto Init = VarNames[i].second.get();
        Value *InitVal;
        if (Init) {
            InitVal = Init->codegen();
            if (!InitVal)
                return nullptr;
        } else {
            InitVal = ConstantFP::get(TheContext, APFloat(0.0));
        }

        auto Alloca = CreateEntryBlockAlloca(F, VarName);
        Builder.CreateStore(InitVal, Alloca);

        scope->setVal(VarName, Alloca);

//        OldBindings.push_back(NamedValues[VarName]);
//        NamedValues[VarName] = Alloca;
    }

    SispDbgInfo.emitLocation(this);
//    auto BodyVal = Body->codegen();
//    if (!BodyVal)
//        return nullptr;

//    for (unsigned long i = 0, e = VarNames.size(); i != e; i++) {
//        NamedValues[VarNames[i].first] = OldBindings[i];
//    }

    return Constant::getNullValue(Type::getDoubleTy(TheContext));
}

Value *UnaryExprAST::codegen() {
    auto OperandV = Operand->codegen();
    if (!OperandV)
        return nullptr;

    auto F = getFunction(string("unary") + Opcode);
    if (!F)
        return LogErrorV("Unkown unary operator");

    SispDbgInfo.emitLocation(this);
    return Builder.CreateCall(F, OperandV, "unop");
}

Value *CompoundExprAST::codegen() {
    int i = 0;
    for (auto Expr = Exprs.begin(); Expr != Exprs.end(); Expr ++) {
        cout << "subExpr " << to_string(i++) << endl;
        if (!(*Expr)->codegen())
            return nullptr;
    }
    return Constant::getNullValue(Type::getDoubleTy(TheContext));
}

Value *CallExprAST::codegen() {
    SispDbgInfo.emitLocation(this);
    // Look up the name in the global module table.
    Function *CalleeF = getFunction(Callee);
    if (!CalleeF)
        return LogErrorV((string("Unknown function referenced ") + Callee).c_str());

    // If argument mismatch error.
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");

    std::vector<Value *> ArgsV;
    for (unsigned long i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
        return nullptr;
    }

    return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen() {
    vector<Type *> Doubles(Args.size(), Type::getDoubleTy(TheContext));
    FunctionType *FT = FunctionType::get(Type::getDoubleTy(TheContext), Doubles, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());
    unsigned long Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(Args[Idx++]);
    return F;
}

Function *FunctionAST::codegen() {
    // Transfer ownership of the prototype to the FunctionProtos map, but keep a
    // reference to it for use below.
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    Function *F = getFunction(P.getName());
    if (!F)
        return nullptr;

    if (P.isBinaryOp())
        BinOpPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();

    // Create a new basic block to start insertion into.
    auto BB = BasicBlock::Create(TheContext, "entry", F);
    Builder.SetInsertPoint(BB);

    // Create a subprogram DIE for this function.
    DIFile *Unit = DBuilder->createFile(SispDbgInfo.TheCU->getFilename(),
                                        SispDbgInfo.TheCU->getDirectory());
    DIScope *FContext = Unit;
    unsigned LineNo = P.getLine();
    unsigned ScopeLine = LineNo;
    DISubprogram *SP =
    DBuilder->createFunction(FContext,
                             P.getName(),
                             StringRef(),
                             Unit,
                             LineNo,
                             CreateFunctionType(F->arg_size(), Unit),
                             ScopeLine,
                             DINode::FlagPrototyped,
                             DISubprogram::SPFlagDefinition);
    F->setSubprogram(SP);

    // Push the current scope.
    SispDbgInfo.LexicalBlocks.push_back(SP);

    // Unset the location for the prologue emission (leading instructions with no
    // location in a function are considered part of the prologue and the debugger
    // will run past them when breaking on a function)
    SispDbgInfo.emitLocation(nullptr);

    // Record the function arguments in the NamedValues map.
//    NamedValues.clear();

    unsigned ArgIdx = 0;
    for (auto &Arg : F->args()) {
        auto Alloca = CreateEntryBlockAlloca(F, Arg.getName());

        // Create a debug descriptor for the variable.
        DILocalVariable *D = DBuilder->createParameterVariable(
            SP, Arg.getName(), ++ArgIdx, Unit, LineNo, SispDbgInfo.getDoubleTy(),
            true);

        DBuilder->insertDeclare(Alloca, D, DBuilder->createExpression(),
                                DebugLoc::get(LineNo, 0, SP),
                                Builder.GetInsertBlock());

        Builder.CreateStore(&Arg, Alloca);
//        NamedValues[Arg.getName()] = Alloca;
        Body->getScope()->setVal(Arg.getName(), Alloca);
    }

    SispDbgInfo.emitLocation(Body.get());

    if (Value *RetVal = Body->codegen()) {
        // Finish off the function.
        Builder.CreateRet(RetVal);

        SispDbgInfo.LexicalBlocks.pop_back();

        // Validate the generated code, checking for consistency.
        verifyFunction(*F);

        if (JITEnabled) {
            // Run the optimizer on the function.
            TheFPM->run(*F);
        }

        return F;
    }

    // Error reading body, remove function.
    F->eraseFromParent();

    if (P.isBinaryOp())
        BinOpPrecedence.erase(Proto->getOperatorName());

    SispDbgInfo.LexicalBlocks.pop_back();

    return nullptr;
}


static unique_ptr<ExprAST> ParseIfExpr(shared_ptr<Scope> scope);
static unique_ptr<ExprAST> ParseForExpr(shared_ptr<Scope> scope);

static unique_ptr<ExprAST> ParseNumberExpr(shared_ptr<Scope> scope) {
    auto Result = make_unique<NumberExprAST>(scope, NumVal);
    getNextToken();

    if (CurTok == tok_colon) {
        getNextToken();
    }
    return move(Result);
}

static unique_ptr<ExprAST> ParseParenExpr(shared_ptr<Scope> scope) {
    getNextToken();
    auto V = ParseExpr(scope);
    if (!V)
        return nullptr;

    if (CurTok != tok_right_paren)
        return LogError("expected ')'");

    getNextToken();

    if (CurTok == tok_colon) {
        getNextToken();
    }
    return V;
}

static std::unique_ptr<ExprAST> ParseIdentifierExpr(shared_ptr<Scope> scope) {
    std::string IdName = IdentifierStr;

    SourceLocation LitLoc = CurLoc;

    getNextToken(); // eat identifier.

    if (CurTok != tok_left_paren) // Simple variable ref.
        return make_unique<VariableExprAST>(scope, LitLoc, IdName);

    // Call.
    getNextToken(); // eat (
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (CurTok != tok_right_paren) {
        while (true) {
            if (auto Arg = ParseExpr(scope))
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (CurTok == tok_right_paren)
                break;

            if (CurTok != tok_comma)
                return LogError("Expected ')' or ',' in argument list");

                getNextToken();
        }
    }

    // Eat the ')'.
    getNextToken();

    if (CurTok == tok_colon) {
        getNextToken();
    }

    return make_unique<CallExprAST>(scope, CurLoc, IdName, std::move(Args));
}

static unique_ptr<ExprAST> ParseVarExpr(shared_ptr<Scope> scope);

static unique_ptr<ExprAST> ParsePrimary(shared_ptr<Scope> scope) {
    switch (CurTok) {
        case tok_identifier:
            return ParseIdentifierExpr(scope);
        case tok_number:
            return ParseNumberExpr(scope);
        case tok_left_paren:
            return ParseParenExpr(scope);
        case tok_if:
            return ParseIfExpr(scope);
        case tok_for:
            return ParseForExpr(scope);
        case tok_var:
            return ParseVarExpr(scope);
        default:
            return LogError("unkown token when execepting an expression");
    }
}

static int GetTokenPrecedence() {
    if (!isascii(CurTok)) {
        return -1;
    }

    int TokPrec = BinOpPrecedence[CurTok];
    if (TokPrec <= 0) {
        return -1;
    }

    return TokPrec;
}

static unique_ptr<ExprAST> ParseUnary(shared_ptr<Scope> scope);
static unique_ptr<ExprAST> ParseBinOpRHS(shared_ptr<Scope> scope, int ExprPrec, unique_ptr<ExprAST> LHS);

static unique_ptr<ExprAST> ParseExpr(shared_ptr<Scope> scope) {
    if (CurTok == tok_left_bracket) {
        cout << "## {" << endl;

        getNextToken();

        vector<unique_ptr<ExprAST>> Exprs;
        auto localScope = make_shared<Scope>(scope);
        while (true) {
            if (CurTok == tok_right_bracket) {
                cout << "## }" << endl;
                getNextToken();
                break;
            }
            auto Expr = ParseExpr(localScope);
            if (!Expr)
                return nullptr;

            Expr->dumpAST();
            Exprs.push_back(move(Expr));
        }

        return make_unique<CompoundExprAST>(localScope, move(Exprs));
    }

    auto LHS = ParseUnary(scope);
    if (!LHS)
        return nullptr;

    return ParseBinOpRHS(scope, 0, move(LHS));
}

static unique_ptr<ExprAST> ParseBinOpRHS(shared_ptr<Scope> scope,
                                         int ExprPrec,
                                         unique_ptr<ExprAST> LHS) {
    while (true) {
        int TokPrec = GetTokenPrecedence();

        if (TokPrec < ExprPrec) {
            return LHS;
        }

        char BinOp = CurTok;
        SourceLocation BinLoc = CurLoc;
        getNextToken();

        auto RHS = ParseUnary(scope);
        if (!RHS) {
            return nullptr;
        }

        int NextPrec = GetTokenPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(scope, TokPrec + 1, move(RHS));
            if (!RHS) {
                return nullptr;
            }
        }

        if (BinOp == tok_equal) {
            cout << "assign" << endl;
        }

        LHS = make_unique<BinaryExprAST>(scope, BinLoc, BinOp, move(LHS), move(RHS));
    }
}

static unique_ptr<ExprAST> ParseIfExpr(shared_ptr<Scope> scope) {
    SourceLocation IfLoc = CurLoc;

    cout << "## if" << endl;

    getNextToken();

    auto IfScope = make_shared<Scope>(scope);

    auto Cond = ParseExpr(IfScope);
    if (!Cond)
        return nullptr;

    if (CurTok != tok_then)
        return LogError("expected then");
    getNextToken();

    cout << "## then" << endl;

    auto Then = ParseExpr(IfScope);
    if (!Then)
        return nullptr;

    if (CurTok != tok_else)
        return LogError("expected else");
    getNextToken();

    cout << "## else" << endl;

    auto Else = ParseExpr(IfScope);
    if (!Else)
        return nullptr;


    if (CurTok == tok_colon) {
        getNextToken();
    }
    return make_unique<IfExprAST>(IfScope, IfLoc, move(Cond), move(Then), move(Else));
}

static unique_ptr<ExprAST> ParseForExpr(shared_ptr<Scope> scope) {
    getNextToken();

    cout << "## for" << endl;

    if (CurTok != tok_left_paren)
        return LogError("expected '(' after for");
    getNextToken();

    if (CurTok != tok_identifier)
        return LogError("expected identifier after for");

    shared_ptr<Scope> ForScope = make_shared<Scope>(scope);

    auto IdName = IdentifierStr;
    getNextToken();

    if (CurTok != tok_equal)
        return LogError("expected '=', after identifier");
    getNextToken();

    cout << "## start" << endl;

    auto Start = ParseExpr(ForScope);
    if (!Start)
        return nullptr;

    if (CurTok == tok_colon)
//        return LogError("expected ';' after start value");
        getNextToken();

    cout << "## end" << endl;

    auto End = ParseExpr(ForScope);
    if (!End)
        return nullptr;

    cout << "## step" << endl;

    unique_ptr<ExprAST> Step;
    if (CurTok == tok_colon) {
        getNextToken();
        Step = ParseExpr(ForScope);
        if (!Step)
            return nullptr;
    }

    if (CurTok != tok_right_paren)
        return LogError("expected ')' in for");
    getNextToken();

//    if (CurTok != tok_in)
//        return LogError("expected 'in' after for");
//    getNextToken();

    auto Body = ParseExpr(ForScope);
    if (!Body)
        return nullptr;


    if (CurTok == tok_colon) {
        getNextToken();
    }
    return make_unique<ForExprAST>(ForScope,
                                   IdName,
                                   move(Start),
                                   move(End),
                                   move(Step),
                                   move(Body));
}

static unique_ptr<ExprAST> ParseUnary(shared_ptr<Scope> scope) {
    if (!isascii(CurTok) ||
        CurTok == tok_left_paren ||
        CurTok == tok_comma)
        return ParsePrimary(scope);

    char Opc = CurTok;
    getNextToken();

    if (auto Operand = ParseUnary(scope))
        return make_unique<UnaryExprAST>(scope, Opc, move(Operand));

    return nullptr;
}

static unique_ptr<ExprAST> ParseVarExpr(shared_ptr<Scope> scope) {
    getNextToken();

    vector<pair<string, unique_ptr<ExprAST>>> VarNames;

    if (CurTok != tok_identifier)
        return LogError("expected identifier after var");

    while (true) {
        auto Name = IdentifierStr;
        getNextToken();

        unique_ptr<ExprAST> Init;
        if (CurTok == '=') {
            getNextToken();

            Init = ParseExpr(scope);
            if (!Init)
                return nullptr;
        }

        VarNames.push_back(make_pair(Name, move(Init)));

        if (CurTok != ',')
            break;
        getNextToken();

        if (CurTok != tok_identifier)
            return LogError("expected identifier list after var");
    }


    if (CurTok == tok_colon) {
        getNextToken();
    }
    return make_unique<VarExprAST>(scope, move(VarNames)/*, move(Body)*/);
}

static unique_ptr<PrototypeAST> ParsePrototype() {

    SourceLocation FnLoc = CurLoc;
    string FnName = IdentifierStr;
    unsigned Kind = 0;
    unsigned BinaryPrecedence = 30;

    switch (CurTok) {
        case tok_identifier:
            FnName = IdentifierStr;
            Kind = 0;
            getNextToken();
            break;
        case tok_unary:
            getNextToken();
            if (!isascii(CurTok))
                return LogErrorP("Excpeted unary operator");
            FnName = string("unary") + (char)CurTok;
            Kind = 1;
            getNextToken();
            break;
        case tok_binary:
            getNextToken();
            if (!isascii(CurTok))
                return LogErrorP("Expected binary operator");
            FnName = "binary";
            FnName += (char)CurTok;
            Kind = 2;
            getNextToken();

            if (CurTok == tok_number) {
                if (NumVal < 1 || NumVal > 100)
                    return LogErrorP("Invalid precedence: must be 1..100");
                BinaryPrecedence = (unsigned)NumVal;
                getNextToken();
            }
            break;
        default:
            return LogErrorP("Expected function name in prototype");
    }

    if (CurTok != tok_left_paren) {
        return LogErrorP("Expected '(' in prototype");
    }

    vector<string> ArgNames;
    while (getNextToken() == tok_identifier) {
        ArgNames.push_back(IdentifierStr);
    }
    if (isspace(CurTok)) {
        getNextToken();
    }
    if (CurTok != tok_right_paren) {
        return LogErrorP("Expected ')' in prototype");
    }

    getNextToken();

    if (Kind && ArgNames.size() != Kind)
        return LogErrorP("Invalid number of operands for operator");

    return make_unique<PrototypeAST>(FnLoc, FnName, move(ArgNames), Kind != 0, BinaryPrecedence);
}

static unique_ptr<FunctionAST> ParseDefinition(shared_ptr<Scope> scope) {
    getNextToken();
    auto Proto = ParsePrototype();
    if (!Proto) {
        return nullptr;
    }

    if (auto E = ParseExpr(scope))
        return make_unique<FunctionAST>(move(Proto), move(E));

    return nullptr;
}

static unique_ptr<PrototypeAST> ParseExtern() {
    getNextToken();
    return ParsePrototype();
}

static unique_ptr<FunctionAST> ParseTopLevelExpr(shared_ptr<Scope> scope) {
    SourceLocation FnLoc = CurLoc;
    if (auto E = ParseExpr(scope)) {
        string Name = "__anon_expr";
//        string Name = "main";
        auto Proto = make_unique<PrototypeAST>(FnLoc, Name, vector<string>());
        return make_unique<FunctionAST>(move(Proto), move(E));
    }
    return nullptr;
}

static void InitializeModuleAndPassManager() {
    // Open a new module.
    TheModule = make_unique<Module>("Sisp Demo 10", TheContext);
    TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

    // Create a new pass manager attached to it.
    TheFPM = make_unique<legacy::FunctionPassManager>(TheModule.get());

    TheFPM->add(createPromoteMemoryToRegisterPass());

    // Do simple "peephole" optimizations and bit-twiddling optzns.
    TheFPM->add(createInstructionCombiningPass());
    // Reassociate expressions.
    TheFPM->add(createReassociatePass());
    // Eliminate Common SubExpressions.
    TheFPM->add(createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    TheFPM->add(createCFGSimplificationPass());

    TheFPM->doInitialization();
}

static void HandleDefinition(shared_ptr<Scope> scope) {
    if (auto FnAST = ParseDefinition(scope)) {
        cout << "FnAST: " << FnAST->getName() << endl;
        if (auto *FnIR = FnAST->codegen()) {
//            cout << "Read function definition:";
//            FnIR->print(outs());
//            cout << endl;
            if (JITEnabled) {
                TheJIT->addModule(std::move(TheModule));
                InitializeModuleAndPassManager();
            }
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern() {
    if (auto ProtoAST = ParseExtern()) {
        if (auto *FnIR = ProtoAST->codegen()) {
//            cout << "Read extern: ";
//            FnIR->print(outs());
//            cout << endl;
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression(shared_ptr<Scope> scope) {
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr(scope)) {
        if (FnAST->codegen()) {
            if (JITEnabled) {
                // JIT the module containing the anonymous expression, keeping a handle so
                // we can free it later.
                auto H = TheJIT->addModule(std::move(TheModule));
                InitializeModuleAndPassManager();

                // Search the JIT for the __anon_expr symbol.
                auto ExprSymbol = TheJIT->findSymbol("__anon_expr");
                assert(ExprSymbol && "Function not found");

                // Get the symbol's address and cast it to the right type (takes no
                // arguments, returns a double) so we can call it as a native function.
                double (*FP)() = (double (*)())(intptr_t)cantFail(ExprSymbol.getAddress());
                cout << "JIT Evaluate:" << endl;
                double R = FP();
                cout << endl << "Evaluate Result:" << to_string(R) << endl;

                // Delete the anonymous expression module from the JIT.
                TheJIT->removeModule(H);
            }
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static int MainLoop() {
    auto scope = make_shared<Scope>();
    while (true) {
        switch (CurTok) {
            case tok_eof:
                return 0;
            case tok_colon:
            case tok_right_bracket:
                getNextToken();
                break;
            case tok_def:
                HandleDefinition(scope);
                break;
            case tok_extern:
                HandleExtern();
                break;
            default:
                HandleTopLevelExpression(scope);
                break;
        }
    }
}


#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT double putchard(double X) {
    cout << (char)X;
//    cout << "putchar [" << string(1, (char)X) << "]" << endl;
    return 0;
}

extern "C" DLLEXPORT double printd(double X) {
    cout << to_string(X);
//    cout << "print [" << to_string(X) << "]" << endl;
    return 0;
}

int main(int argc, const char * argv[]) {

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    BinOpPrecedence[tok_equal] = 2;
    BinOpPrecedence[tok_less] = 10;
    BinOpPrecedence[tok_add] = 20;
    BinOpPrecedence[tok_sub] = 20;
    BinOpPrecedence[tok_mul] = 40;

//    cout << "Simple Lisp 1.0" << endl;
//    cout << "sisp> ";

    const char *test_path = "/Users/xuke/mywork/play_with_llvm/sisp/sisp/test10.sisp";
    std::ifstream t(test_path);
    std::string str;

    t.seekg(0, std::ios::end);
    str.reserve(t.tellg());
    t.seekg(0, std::ios::beg);

    str.assign((std::istreambuf_iterator<char>(t)),
                std::istreambuf_iterator<char>());

    TheCode = str;

//    cout << TheCode << endl;

    getNextToken();

    JITEnabled = true;

    TheJIT = make_unique<llvm::orc::SispJIT>();
    InitializeModuleAndPassManager();

    TheModule->addModuleFlag(Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);
    if (Triple(sys::getProcessTriple()).isOSDarwin())
        TheModule->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

    DBuilder = make_unique<DIBuilder>(*TheModule);
    SispDbgInfo.TheCU = DBuilder->createCompileUnit(dwarf::DW_LANG_C, DBuilder->createFile("ch10.sisp", "."), "Sisp Compiler", 0, "", 0);

    MainLoop();

    DBuilder->finalize();

    cout << "### Module Define ###" << endl;
    TheModule->print(outs(), nullptr);

    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    auto TargetTriple = sys::getDefaultTargetTriple();
    TheModule->setTargetTriple(TargetTriple);

    string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

    if (!Target) {
        LogError(Error.c_str());
        return 1;
    }

    auto CPU = "generic";
    auto Features = "";

    TargetOptions opt;
    auto RM = Optional<Reloc::Model>();
    auto TheTargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);
    TheModule->setDataLayout(TheTargetMachine->createDataLayout());

    auto Filename = "output.o";
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

    legacy::PassManager Pass;
    auto FileType = llvm::TargetMachine::CGFT_ObjectFile;

    if (TheTargetMachine->addPassesToEmitFile(Pass, dest, nullptr, FileType)) {
        LogError("TheTargetMachine can't emit a file of this type");
        return 1;
    }

    Pass.run(*TheModule);
    dest.flush();

//    cout << "Wrote " << Filename << endl;

    return 0;
}
