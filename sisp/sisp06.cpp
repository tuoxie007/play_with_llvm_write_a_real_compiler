//
//  sisp06.cpp
//  sisp
//
//  Created by 徐可 on 2020/2/15.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#include "SispJIT04.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
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

static int GetChar() {
    static string::size_type Index = 0;
    if (Index >= TheCode.length())
        return EOF;
    char CurChar = TheCode.at(Index++);
    cout << "getchar [" << string(1, CurChar) << "]" << endl;
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

class ExprAST {
public:
    ExprAST() {}
    virtual ~ExprAST() {}
    virtual Value *codegen() = 0;
};

static unique_ptr<ExprAST> ParseExpr();

class NumberExprAST : public ExprAST {
    double Val;

public:
    NumberExprAST(double Val): Val(Val) {}
    Value *codegen() override;
};

class VariableExprAST : public ExprAST {
    string Name;

public:
    VariableExprAST(const string &name) : Name(name) {}
    Value *codegen() override;
};

class BinaryExprAST : public ExprAST {
    char Op;
    unique_ptr<ExprAST> LHS, RHS;

public:
    BinaryExprAST(char op, unique_ptr<ExprAST> lhs, unique_ptr<ExprAST> rhs)
        : Op(op), LHS(move(lhs)), RHS(move(rhs)) {}
    Value *codegen() override;
};

class CallExprAST : public ExprAST {
    string Callee;
    vector<unique_ptr<ExprAST>> Args;

public:
    CallExprAST(const string &callee, vector<unique_ptr<ExprAST>> args)
        : Callee(callee), Args(move(args)) {}
    Value *codegen() override;
};

class PrototypeAST {
    string Name;
    vector<string> Args;
    bool IsOperator;
    unsigned Precedence;

public:
    PrototypeAST(string &name,
                 vector<string> args,
                 bool isOperator = false,
                 unsigned precedence = 0)
        : Name(name),
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
};

class IfExprAST : public ExprAST {
    unique_ptr<ExprAST> Cond, Then, Else;

public:
    IfExprAST(unique_ptr<ExprAST> cond,
              unique_ptr<ExprAST> then,
              unique_ptr<ExprAST> elseE)
        : Cond(move(cond)), Then(move(then)), Else(move(elseE)) {}

    Value * codegen() override;
};

class ForExprAST : public ExprAST {
    string VarName;
    unique_ptr<ExprAST> Start, End, Step, Body;

public:
    ForExprAST(string &varName,
               unique_ptr<ExprAST> start,
               unique_ptr<ExprAST> end,
               unique_ptr<ExprAST> step,
               unique_ptr<ExprAST> body):
        VarName(varName), Start(move(start)), End(move(end)), Step(move(step)), Body(move(body)) {}

    Value * codegen() override;
};

class UnaryExprAST : public ExprAST {
    char Opcode;
    unique_ptr<ExprAST> Operand;

public:
    UnaryExprAST(char opcode, unique_ptr<ExprAST> operand)
        : Opcode(opcode), Operand(move(operand)) {}

    Value * codegen() override;
};

static Token CurTok;
static int getNextToken() {
    CurTok = (Token)gettok();
    if (CurTok == tok_or)
        cout << "CurTok 124" << endl;
    return CurTok;
}

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static unique_ptr<Module> TheModule;
static map<string, Value *> NamedValues;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<llvm::orc::SispJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

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

Value *NumberExprAST::codegen() {
    return ConstantFP::get(TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen() {
    Value *V = NamedValues[Name];
    if (!V)
        LogError("Unkown variable name");
    return V;
}

Value *BinaryExprAST::codegen() {
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

    // entry:
    //   br loop
    auto PreheaderBlock = Builder.GetInsertBlock();
    auto LoopBlock = BasicBlock::Create(TheContext, "loop", F);
    Builder.CreateBr(LoopBlock);

    // loop:
    Builder.SetInsertPoint(LoopBlock);

    // https://www.cnblogs.com/ilocker/p/4892439.html
    // phi [StartValue, entry]
    auto Variable = Builder.CreatePHI(Type::getDoubleTy(TheContext), 2, VarName);
    Variable->addIncoming(StartValue, PreheaderBlock);

    auto OldVal = NamedValues[VarName];
    NamedValues[VarName] = Variable;

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

    // fadd Variable, StepValue
    auto NextVar = Builder.CreateFAdd(Variable, StepVal, "nextval");

    auto EndCond = End->codegen();
    if (!EndCond)
        return nullptr;

    EndCond = Builder.CreateFCmpONE(EndCond, ConstantFP::get(TheContext, APFloat(0.0)), "loopcond");

    auto LoopEndBlock = Builder.GetInsertBlock();
    auto AfterBlock = BasicBlock::Create(TheContext, "afterloop", F);
    Builder.CreateCondBr(EndCond, LoopBlock, AfterBlock);
    Builder.SetInsertPoint(AfterBlock);

    // phi [StartValue, entry], [NextVar, end]
    Variable->addIncoming(NextVar, LoopEndBlock);

    if (OldVal)
        NamedValues[VarName] = OldVal;
    else
        NamedValues.erase(VarName);

    return Constant::getNullValue(Type::getDoubleTy(TheContext));
}

Value *UnaryExprAST::codegen() {
    auto OperandV = Operand->codegen();
    if (!OperandV)
        return nullptr;

    auto F = getFunction(string("unary") + Opcode);
    if (!F)
        return LogErrorV("Unkown unary operator");

    return Builder.CreateCall(F, OperandV, "unop");
}

Value *CallExprAST::codegen() {
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

    // Record the function arguments in the NamedValues map.
    NamedValues.clear();
    for (auto &Arg : F->args())
        NamedValues[std::string(Arg.getName())] = &Arg;

    if (Value *RetVal = Body->codegen()) {
        // Finish off the function.
        Builder.CreateRet(RetVal);

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
    return nullptr;
}


static unique_ptr<ExprAST> ParseIfExpr();
static unique_ptr<ExprAST> ParseForExpr();

static unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = make_unique<NumberExprAST>(NumVal);
    getNextToken();
    return move(Result);
}

static unique_ptr<ExprAST> ParseParenExpr() {
    getNextToken();
    auto V = ParseExpr();
    if (!V)
        return nullptr;

    if (CurTok != tok_right_paren)
        return LogError("expected ')'");

    getNextToken();
    return V;
}

static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier.

  if (CurTok != tok_left_paren) // Simple variable ref.
    return make_unique<VariableExprAST>(IdName);

  // Call.
  getNextToken(); // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != tok_right_paren) {
    while (true) {
      if (auto Arg = ParseExpr())
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

  return make_unique<CallExprAST>(IdName, std::move(Args));
}

static unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
        case tok_identifier:
            return ParseIdentifierExpr();
        case tok_number:
            return ParseNumberExpr();
        case tok_left_paren:
            return ParseParenExpr();
        case tok_if:
            return ParseIfExpr();
        case tok_for:
            return ParseForExpr();
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

static unique_ptr<ExprAST> ParseUnary();
static unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, unique_ptr<ExprAST> LHS);

static unique_ptr<ExprAST> ParseExpr() {
    auto LHS = ParseUnary();
    if (!LHS){
        return nullptr;
    }

    return ParseBinOpRHS(0, move(LHS));
}

static unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                         unique_ptr<ExprAST> LHS) {
    while (true) {
        int TokPrec = GetTokenPrecedence();

        if (TokPrec < ExprPrec) {
            return LHS;
        }

        char BinOp = CurTok;
        getNextToken();

        auto RHS = ParseUnary();
        if (!RHS) {
            return nullptr;
        }

        int NextPrec = GetTokenPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, move(RHS));
            if (!RHS) {
                return nullptr;
            }
        }

        LHS = make_unique<BinaryExprAST>(BinOp, move(LHS), move(RHS));
    }
}

static unique_ptr<ExprAST> ParseIfExpr() {
    getNextToken();

    auto Cond = ParseExpr();
    if (!Cond)
        return nullptr;

    if (CurTok != tok_then)
        return LogError("expected then");
    getNextToken();

    auto Then = ParseExpr();
    if (!Then)
        return nullptr;

    if (CurTok != tok_else)
        return LogError("expected else");
    getNextToken();

    auto Else = ParseExpr();
    if (!Else)
        return nullptr;

    return make_unique<IfExprAST>(move(Cond), move(Then), move(Else));
}

static unique_ptr<ExprAST> ParseForExpr() {
    getNextToken();

    if (CurTok != tok_identifier)
        return LogError("expected identifier after for");

    auto IdName = IdentifierStr;
    getNextToken();

    if (CurTok != tok_equal)
        return LogError("expected '=', after identifier");
    getNextToken();

    auto Start = ParseExpr();
    if (!Start)
        return nullptr;
    if (CurTok != tok_comma)
        return LogError("expected ',' after start value");
    getNextToken();

    auto End = ParseExpr();
    if (!End)
        return nullptr;

    unique_ptr<ExprAST> Step;
    if (CurTok == tok_comma) {
        getNextToken();
        Step = ParseExpr();
        if (!Step)
            return nullptr;
    }

    if (CurTok != tok_in)
        return LogError("expected 'in' after for");
    getNextToken();

    auto Body = ParseExpr();
    if (!Body)
        return nullptr;

    return make_unique<ForExprAST>(IdName,
                                   move(Start),
                                   move(End),
                                   move(Step),
                                   move(Body));
}

static unique_ptr<ExprAST> ParseUnary() {
    if (!isascii(CurTok) ||
        CurTok == tok_left_paren ||
        CurTok == tok_comma)
        return ParsePrimary();

    char Opc = CurTok;
    getNextToken();

    if (auto Operand = ParseUnary())
        return make_unique<UnaryExprAST>(Opc, move(Operand));

    return nullptr;
}

static unique_ptr<PrototypeAST> ParsePrototype() {

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

    return make_unique<PrototypeAST>(FnName, move(ArgNames), Kind != 0, BinaryPrecedence);
}

static unique_ptr<FunctionAST> ParseDefinition() {
    getNextToken();
    auto Proto = ParsePrototype();
    if (!Proto) {
        return nullptr;
    }

    if (auto E = ParseExpr())
        return make_unique<FunctionAST>(move(Proto), move(E));

    return nullptr;
}

static unique_ptr<PrototypeAST> ParseExtern() {
    getNextToken();
    return ParsePrototype();
}

static unique_ptr<FunctionAST> ParseTopLevelExpr() {
    if (auto E = ParseExpr()) {
        string Name = "__anon_expr";
        auto Proto = make_unique<PrototypeAST>(Name, vector<string>());
        return make_unique<FunctionAST>(move(Proto), move(E));
    }
    return nullptr;
}

static void InitializeModuleAndPassManager() {
    // Open a new module.
    TheModule = make_unique<Module>("Sisp Demo 06", TheContext);
    TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

    // Create a new pass manager attached to it.
    TheFPM = make_unique<legacy::FunctionPassManager>(TheModule.get());

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

static void HandleDefinition() {
    if (auto FnAST = ParseDefinition()) {
        if (auto *FnIR = FnAST->codegen()) {
            cout << "Read function definition:";
            FnIR->print(outs());
            cout << endl;
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
            cout << "Read extern: ";
            FnIR->print(outs());
            cout << endl;
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression() {
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr()) {
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
                cout << "Evaluated to " << to_string(FP()) << endl;

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
    while (true) {
        switch (CurTok) {
            case tok_eof:
                return 0;
            case tok_colon:
                getNextToken();
                break;
            case tok_def:
                HandleDefinition();
                break;
            case tok_extern:
                HandleExtern();
                break;
            default:
                HandleTopLevelExpression();
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
    fputc((char)X, stderr);
    return 0;
}

extern "C" DLLEXPORT double printd(double X) {
    cout << to_string(X) << endl;
    return 0;
}

int main(int argc, const char * argv[]) {

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    BinOpPrecedence[tok_less] = 10;
    BinOpPrecedence[tok_add] = 20;
    BinOpPrecedence[tok_sub] = 20;
    BinOpPrecedence[tok_mul] = 40;

    cout << "Simple List 1.0" << endl;
//    cout << "sisp> ";
//    TheCode =
//    "extern foo();"
//    "extern bar();"
//    "def baz(x) if x then foo() else bar();"
//    ;
    FILE *StdFile = fopen("/Users/jason/mywork/sisp/sisp/std.sisp", "r");
    fseek(StdFile, 0L, SEEK_END);
    unsigned long sz = ftell(StdFile);
    rewind(StdFile);
    char *StdContent = (char *)malloc(sz);
    fread(StdContent, sz, sizeof(char), StdFile);
//    cout << StdContent;

    FILE *TestFile = fopen("/Users/jason/mywork/sisp/sisp/test06.sisp", "r");
    fseek(TestFile, 0L, SEEK_END);
    sz = ftell(TestFile);
    rewind(TestFile);
    char *TestContent = (char *)malloc(sz);
    fread(TestContent, sz, sizeof(char), TestFile);

    TheCode =
    string(StdContent) + string(TestContent);

    cout << TheCode << endl;

    getNextToken();

    JITEnabled = true;

    TheJIT = make_unique<llvm::orc::SispJIT>();
    InitializeModuleAndPassManager();

    int R = MainLoop();

    cout << "### Module Define ###" << endl;
    TheModule->print(outs(), nullptr);

    return R;
}
