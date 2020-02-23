//
//  Parser.hpp
//  sisp
//
//  Created by 徐可 on 2020/2/19.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#ifndef Parser_hpp
#define Parser_hpp

#include "SispJIT.h"
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

#include <string>
#include <iostream>

#include "Lexer.hpp"

using namespace std;
using namespace llvm;

class ExprAST;

static unsigned gId = 0;
class Scope {
    map<string, unique_ptr<ExprAST>> Vars;
    map<string, AllocaInst *> VarVals;

public:
    shared_ptr<Scope> Parent;
    unsigned Id;

    Scope() { Id = ++ gId; }
    Scope(shared_ptr<Scope> parent): Parent(parent) { Id = ++ gId; }

    void append(pair<string, unique_ptr<ExprAST>> V) {
        Vars.insert(move(V));
    }
    void setVal(string var, AllocaInst *val) {
        VarVals[var] = val;
    }
    AllocaInst *getVal(string var) {
        return VarVals[var] ?: (Parent->getVal(var) ?: nullptr);
    }
};

class ExprAST {
    SourceLocation Loc;

protected:
    shared_ptr<Scope> scope;

public:
    ExprAST(shared_ptr<Scope> scope);
    ExprAST(shared_ptr<Scope> scope, SourceLocation Loc) : scope(scope), Loc(Loc) {}
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
    CompoundExprAST(shared_ptr<Scope> scope, vector<unique_ptr<ExprAST>> exprs): ExprAST(scope), Exprs(move(exprs)) {}
    Value *codegen() override;
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

static unique_ptr<ExprAST> LogError(std::string Str) {
    cerr << "LogError: " << Str << endl;
    assert(false && Str.c_str());
    return nullptr;
}

static unique_ptr<PrototypeAST> LogErrorP(std::string Str) {
    LogError(Str);
    return nullptr;
}

static Value *LogErrorV(std::string Str) {
    LogError(Str);
    return nullptr;
}

class DebugInfo {
public:
    DebugInfo() {};
    DICompileUnit *TheCU;
    DIType *DblTy;
    std::vector<DIScope *> LexicalBlocks;
    DIType *getDoubleTy();
    void emitLocation(ExprAST *AST);
};

//extern LLVMContext TheContext;
extern std::unique_ptr<DIBuilder> DBuilder;
extern DebugInfo SispDbgInfo;

class Parser {
    bool JITEnabled;
    LLVMContext LLContext;
    IRBuilder<> *Builder;
    unique_ptr<Module> TheModule;
    std::unique_ptr<llvm::orc::SispJIT> TheJIT = std::make_unique<llvm::orc::SispJIT>();;
    std::unique_ptr<legacy::FunctionPassManager> TheFPM;
    std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
    map<char, int> BinOpPrecedence;
    std::unique_ptr<Lexer> TheLexer;

    unique_ptr<ExprAST> ParseNumberExpr(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseParenExpr(shared_ptr<Scope> scope);
    std::unique_ptr<ExprAST> ParseIdentifierExpr(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParsePrimary(shared_ptr<Scope> scope);
    int GetTokenPrecedence();
    unique_ptr<ExprAST> ParseExpr(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseBinOpRHS(shared_ptr<Scope> scope, int ExprPrec, unique_ptr<ExprAST> LHS);
    unique_ptr<ExprAST> ParseIfExpr(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseForExpr(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseUnary(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseVarExpr(shared_ptr<Scope> scope);
    unique_ptr<PrototypeAST> ParsePrototype();
    unique_ptr<FunctionAST> ParseDefinition(shared_ptr<Scope> scope);
    unique_ptr<PrototypeAST> ParseExtern();
    unique_ptr<FunctionAST> ParseTopLevelExpr(shared_ptr<Scope> scope);

public:
    Parser(bool jitEnabled, std::string src)
    : JITEnabled(jitEnabled), TheLexer(std::make_unique<Lexer>(src)) {

        Builder = new IRBuilder<>(LLContext);

        BinOpPrecedence[tok_equal] = 2;
        BinOpPrecedence[tok_less] = 10;
        BinOpPrecedence[tok_add] = 20;
        BinOpPrecedence[tok_sub] = 20;
        BinOpPrecedence[tok_mul] = 40;

        getNextToken();

        InitializeModuleAndPassManager();

        TheModule->addModuleFlag(Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);
        if (Triple(sys::getProcessTriple()).isOSDarwin())
            TheModule->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

        DBuilder = std::make_unique<DIBuilder>(*TheModule);
        SispDbgInfo.TheCU = DBuilder->createCompileUnit(dwarf::DW_LANG_C, DBuilder->createFile("ch10.sisp", "."), "Sisp Compiler", 0, "", 0);
    }

    Token getNextToken();
    Token getCurToken() { return TheLexer->getCurToken(); }
    SourceLocation getCurLoc() { return TheLexer->CurLoc; }
    void InitializeModuleAndPassManager();
    void HandleDefinition(shared_ptr<Scope> scope);
    void HandleExtern();
    void HandleTopLevelExpression(shared_ptr<Scope> scope);

    static AllocaInst *CreateEntryBlockAlloca(Function *F, const string &VarName);
    void SetBinOpPrecedence(char Op, int Prec) {
        if (Prec >= 0) {
            BinOpPrecedence[Op] = Prec;
        } else {
            BinOpPrecedence.erase(Op);
        }
    };
    void AddFunctionProtos(std::unique_ptr<PrototypeAST> Proto) {
        FunctionProtos[Proto->getName()] = std::move(Proto);
    }
    Function *getFunction(std::string Name);
    Module &getModule() const { return *TheModule.get(); };
    const bool isJITEnabled() const { return JITEnabled; }
    LLVMContext &getContext() { return this->LLContext; };
    IRBuilder<> *getBuilder() { return Builder; };
    void RunFunction(Function *F) { TheFPM->run(*F); };
};

extern unique_ptr<Parser> TheParser;

#endif /* Parser_hpp */
