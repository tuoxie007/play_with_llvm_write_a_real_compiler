//
//  Parser.hpp
//  play
//
//  Created by Jason Hsu on 2020/2/19.
//  Copyright © 2020 Jason Hsu<tuoxie007@gmail.com>. All rights reserved.
//

#ifndef Parser_hpp
#define Parser_hpp

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

static inline std::string FormatString(const char *fmt,...)
{
#define FORMAT_MSG_BUFFER_SIZE (1024)
    char szBuffer[FORMAT_MSG_BUFFER_SIZE + 1] = { 0 };
    va_list args;
    va_start(args, fmt);
    vsnprintf(szBuffer, FORMAT_MSG_BUFFER_SIZE, fmt, args);
    va_end(args);
    std::string strRet  = szBuffer;
    return strRet;
}

static Type * getType(Token type, LLVMContext &contxt) {
    Type *ArgType;
    switch (type) {
        case tok_type_bool:
            ArgType = llvm::Type::getInt1Ty(contxt);
            break;
        case tok_type_int:
            ArgType = llvm::Type::getInt64Ty(contxt);
            break;
        case tok_type_float:
            ArgType = llvm::Type::getDoubleTy(contxt);
            break;
        case tok_type_void:
            ArgType = llvm::Type::getVoidTy(contxt);
            break;
        default:
            assert(false && "not implemented type");
            break;
    }
    return ArgType;
}

enum VarTypeID {
    VarTypeUnkown = 0,
    VarTypeBool,
    VarTypeInt,
    VarTypeFloat,
    VarTypeString,
    VarTypeArray,
    VarTypeClass,
    VarTypeObject,
    VarTypeStar,
    VarTypeVoid,
};

class VarType;

class VarType {
public:
    VarTypeID TypeID;
    string ClassName;
    VarType *PointedType;

    VarType() {}
    VarType(VarTypeID T, string C = ""): TypeID(T), ClassName(C), PointedType(nullptr) {}
    VarType(Token T, string C = ""): ClassName(C), PointedType(nullptr) {
        switch (T) {
            case tok_type_void: TypeID = VarTypeVoid; break;
            case tok_type_bool: TypeID = VarTypeBool; break;
            case tok_type_int: TypeID = VarTypeInt; break;
            case tok_type_float: TypeID = VarTypeFloat; break;
            case tok_type_object: TypeID = VarTypeObject; break;
            default: TypeID = VarTypeUnkown; break;
        }
    }
    static VarType getPointerType(VarType Ty) {
        VarType VT;
        VT.TypeID = VarTypeStar;
        VT.PointedType = new VarType(Ty.TypeID, Ty.ClassName);
        return VT;
    }
    const bool isPointer() const { return TypeID == VarTypeStar; }
    VarType &pointerElement() { return *PointedType; }
    Value *getDefaultValue(LLVMContext &context) {
        switch (this->TypeID) {
            case VarTypeFloat:
                return ConstantFP::get(context, APFloat(0.0));
            case VarTypeBool:
                return ConstantInt::get(context, APInt(1, 0));
            case VarTypeInt:
            case VarTypeObject:
            case VarTypeStar:
                return ConstantInt::get(context, APInt(64, 0));
            default:
                assert(false && "not implemented type");
                return ConstantInt::get(context, APInt(64, 0));
        }
    }
    unsigned getMemoryBytes() {
        if (TypeID == VarTypeBool) {
            return 1;
        } else if (TypeID == VarTypeInt) {
            return 8;
        } else if (TypeID == VarTypeFloat) {
            return 8;
        } else if (TypeID == VarTypeStar) {
            return 8;
        } else {
            assert(false && "unkown type");
            return 0;
        }
    }

    llvm::Type * getType(LLVMContext &contxt) {
        switch (TypeID) {
            case VarTypeVoid:
                return llvm::Type::getVoidTy(contxt);
            case VarTypeBool:
                return llvm::Type::getInt1Ty(contxt);
            case VarTypeInt:
                return llvm::Type::getInt64Ty(contxt);
            case VarTypeFloat:
                return llvm::Type::getDoubleTy(contxt);
            case VarTypeStar:
                return PointedType->getType(contxt)->getPointerTo();
            default:
                assert(false && "not implemented type");
                break;
        }
        return nullptr;
    }
    string dumpJSON() {
        int TypeIdValue = (int)TypeID;
        const char *ClassNameStr = ClassName.length() > 0 ? ClassName.c_str() : "";
        const char *PointedTypeStr = PointedType ? PointedType->dumpJSON().c_str() : "null";
        return FormatString("{`type`: `VarType`, `TypeID`: %d, `ClassName`: `%s`, `PointedType`: %s}",
                            TypeIdValue,
                            ClassNameStr,
                            PointedTypeStr);
    }
};

class ExprAST;
class ClassDeclAST;

static unsigned gId = 0;
class Scope {
    map<string, VarType> VarTypes;
    map<string, Value *> VarVals;
    map<string, unique_ptr<ClassDeclAST>> Classes;
    map<string, StructType *> ClassTypes;

public:
    shared_ptr<Scope> Parent;
    unsigned Id;

    Scope() { Id = ++ gId; }
    Scope(shared_ptr<Scope> parent): Parent(parent) { Id = ++ gId; }

    void setValType(string Name, VarType Type) {
        VarTypes[Name] = Type;
    }
    const VarType getValType(const string &name) {
        if (VarTypes[name].TypeID) return VarTypes[name];
        if (Parent) return Parent->getValType(name);
        return VarType(VarTypeUnkown);
    }
    void setVal(string var, Value *val) {
        if (var=="tall")
            cout << "setVal " << var << " => " << val->getType()->getTypeID() << endl;

        VarVals[var] = val;
    }
    Value *getVal(const string name) {
        return VarVals[name] ?: (Parent ? Parent->getVal(name) : nullptr);
    }
    void appendClass(string name, unique_ptr<ClassDeclAST> C) {
        Classes[name] = std::move(C);
    }
    ClassDeclAST *getClass(const string &name) {
        if (Classes[name]) return Classes[name].get();
        if (Parent) return Parent->getClass(name);
        return nullptr;
    }
    void setClassType(string ClsName, StructType * ClsType) {
        ClassTypes[ClsName] = ClsType;
    }
    StructType * getClassType(const string ClsName) {
        if (ClassTypes[ClsName])
            return ClassTypes[ClsName];
        if (Parent)
            return Parent->getClassType(ClsName);
        return nullptr;
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

    virtual string dumpJSON() = 0;

    static string listDumpJSON(vector<unique_ptr<ExprAST>> &Exprs) {
        string JSON = "[";
        for (auto E = Exprs.begin(); E != Exprs.end(); E ++) {
            JSON += (*E)->dumpJSON();
            if (E != Exprs.end() - 1) {
                JSON += ",";
            }
        }
        JSON += "]";
        return JSON;
    }
};

static unique_ptr<ExprAST> ParseExpr(shared_ptr<Scope> scope);

class VarExprAST : public ExprAST {
    string Name;
    VarType Type;
    unique_ptr<ExprAST> Init;


    static llvm::Type *getIRType(LLVMContext &context, shared_ptr<Scope> scope, VarType &VT) {
        switch (VT.TypeID) {
            case VarTypeVoid: return Type::getVoidTy(context);
            case VarTypeBool: return Type::getInt1Ty(context);
            case VarTypeInt: return Type::getInt64Ty(context);
            case VarTypeFloat: return Type::getDoubleTy(context);
            case VarTypeObject: return scope->getClassType(VT.ClassName)->getPointerTo();
            case VarTypeStar: return getIRType(context, scope, VT.pointerElement())->getPointerTo();
            default: return nullptr;
        }
    }

public:
    VarExprAST(shared_ptr<Scope> scope, VarType type, string name, unique_ptr<ExprAST> init)
        : ExprAST(scope), Type(type), Name(name), Init(std::move(init)) {
            scope->setValType(name, type);
        }

    Value *codegen() override;
    const string &getName() const { return Name; }
    const VarType &getType() const { return Type; }
    llvm::Type *getIRType(LLVMContext &Context) {
        return VarExprAST::getIRType(Context, scope, Type);
    }
    ExprAST * getInit() const { return Init.get(); }

    string dumpJSON() override {
        return FormatString("{`type`: `Var`, `Name`: `%s`, `Type`: %d}", Name.c_str(), (int)Type.TypeID);
    }
};

class CompoundExprAST : public ExprAST {
    vector<unique_ptr<ExprAST>> Exprs;

public:
    CompoundExprAST(shared_ptr<Scope> scope, vector<unique_ptr<ExprAST>> exprs): ExprAST(scope), Exprs(std::move(exprs)) {}
    Value *codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `Compound`, `Exprs`: %s]}", ExprAST::listDumpJSON(Exprs).c_str());
    }
};

class IntegerLiteralAST : public ExprAST {
    long Val;

public:
    IntegerLiteralAST(shared_ptr<Scope> scope, long val): ExprAST(scope), Val(val) {}
    Value *codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `IntegerLiteral`, `Val`: `%s`}", to_string(Val).c_str());
    }
};

class FloatLiteralAST : public ExprAST {
    double Val;

public:
    FloatLiteralAST(shared_ptr<Scope> scope, double val): ExprAST(scope), Val(val) {}
    Value *codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `FloatLiteral`, `Val`: `%s`}", to_string(Val).c_str());
    }
};

class VariableExprAST : public ExprAST {
    string Name;

public:
    VariableExprAST(shared_ptr<Scope> scope, SourceLocation loc, const string &name) : ExprAST(scope, loc), Name(name) {}
    Value *codegen() override;
    string &getName() { return Name; }
    string dumpJSON() override {
        return FormatString("{`type`: `Variable`, `Name`: `%s`}", Name.c_str());
    }
};

class RightValueAST : public ExprAST {
    unique_ptr<ExprAST> Expr;

public:
    RightValueAST(shared_ptr<Scope> scope, unique_ptr<ExprAST> expr) : ExprAST(scope), Expr(std::move(expr)) {}
    Value *codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `RightValue`, `Expr`: %s}", Expr->dumpJSON().c_str());
    }
    ExprAST *getExpr() {
        return Expr.get();
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
        : ExprAST(scope, loc), Op(op), LHS(std::move(lhs)), RHS(std::move(rhs)) {}
    Value *codegen() override;
    string dumpJSON() override {
        string lhs = LHS->dumpJSON();
        string rhs = RHS->dumpJSON();
        return FormatString("{`type`: `Binary`, `Operator`: `%c`, `LHS`: %s, `RHS`: %s}", Op, LHS->dumpJSON().c_str(), RHS->dumpJSON().c_str());
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
        : ExprAST(scope, loc), Callee(callee), Args(std::move(args)) {}
    Value *codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `Call`, `Callee`: `%s`, `Args`: %s}", Callee.c_str(), ExprAST::listDumpJSON(Args).c_str());
    }
};

class MethodCallAST : public ExprAST {
    // TODO
    // SourceLocation Loc;

    unique_ptr<ExprAST> Var;
    string Callee;
    vector<unique_ptr<ExprAST>> Args;

public:
    MethodCallAST(shared_ptr<Scope> scope,
//                  SourceLocation loc,
                  unique_ptr<ExprAST> var,
                  const string &callee,
                  vector<unique_ptr<ExprAST>> args)
        : ExprAST(scope), Var(std::move(var)), Callee(callee), Args(std::move(args)) {}
    Value *codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `MethodCall`, `Var`: %s, `Callee`: `%s`, `Args`: %s}", Var->dumpJSON().c_str(), Callee.c_str(), ExprAST::listDumpJSON(Args).c_str());
    }
};

class MemberAccessAST : public ExprAST {
    // TODO
    // SourceLocation Loc;

    unique_ptr<ExprAST> Var;
    string Member;
    unique_ptr<ExprAST> RHS;

public:
    MemberAccessAST(shared_ptr<Scope> scope, unique_ptr<ExprAST> var, string member)
        : ExprAST(scope), Var(std::move(var)), Member(member) {}
    MemberAccessAST(shared_ptr<Scope> scope, unique_ptr<ExprAST> var, string member, unique_ptr<ExprAST> RHS)
        : ExprAST(scope), Var(std::move(var)), Member(member), RHS(std::move(RHS)) {}

    Value *codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `MemberAccess`, `Var`: %s, `Member`: `%s`, `RHS`: %s}",
                            Var->dumpJSON().c_str(), Member.c_str(), RHS ? RHS->dumpJSON().c_str() : "null");
    }
};

class IndexerAST : public ExprAST {

    unique_ptr<ExprAST> Var;
    unique_ptr<ExprAST> Index;
    unique_ptr<ExprAST> RHS;

public:
    IndexerAST(shared_ptr<Scope> scope,
               unique_ptr<ExprAST> var,
               unique_ptr<ExprAST> index)
        : ExprAST(scope), Var(std::move(var)), Index(std::move(index)) {}

    IndexerAST(shared_ptr<Scope> scope,
               unique_ptr<ExprAST> var,
               unique_ptr<ExprAST> index,
               unique_ptr<ExprAST> RHS)
        : ExprAST(scope), Var(std::move(var)), Index(std::move(index)), RHS(std::move(RHS)) {}

    Value *codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `IndexSubscribe`, `Var`: %s, `Index`: %s, `RHS`: %s}",
                            Var->dumpJSON().c_str(), Index->dumpJSON().c_str(), RHS ? RHS->dumpJSON().c_str() : "null");
    }
};

class PrototypeAST {
    SourceLocation Loc;
//    Token RetType;
    VarType RetType;
    string Name;
    vector<unique_ptr<VarExprAST>> Args;
    bool IsOperator;
    unsigned Precedence;

public:
    PrototypeAST(SourceLocation loc,
                 VarType &type,
                 string &name,
                 vector<unique_ptr<VarExprAST>> args,
                 bool isOperator = false,
                 unsigned precedence = 0)
        :
        Loc(loc),
        RetType(type),
        Name(name),
        Args(std::move(args)),
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
    string dumpJSON() {
        string ArgsJSON = "[";
        for (auto E = Args.begin(); E != Args.end(); E ++) {
            ArgsJSON += (*E)->dumpJSON();
            if (E != Args.end() - 1) {
                ArgsJSON += ",";
            }
        }
        ArgsJSON += "]";
        return FormatString("{`type`: `Prototype`, `Name`: `%s`, `Args`: %s}", Name.c_str(), ArgsJSON.c_str());
    }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}

    const PrototypeAST& getProto() const;
    const std::string& getName() const;
    llvm::Function *codegen();
    std::string dumpJSON();
};

class IfExprAST : public ExprAST {
    unique_ptr<ExprAST> Cond, Then, Else;

public:
    IfExprAST(shared_ptr<Scope> scope,
              SourceLocation loc,
              unique_ptr<ExprAST> cond,
              unique_ptr<ExprAST> then,
              unique_ptr<ExprAST> elseE)
        : ExprAST(scope, loc), Cond(std::move(cond)), Then(std::move(then)), Else(std::move(elseE)) {}

    Value * codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `If`, `Cond`: %s, `Then`: %s, `Else`: %s}", Cond->dumpJSON().c_str(), Then->dumpJSON().c_str(), Else->dumpJSON().c_str());
    }
};

class ForExprAST : public ExprAST {
    unique_ptr<VarExprAST> Var;
    unique_ptr<ExprAST> Start, End, Step, Body;

public:
    ForExprAST(shared_ptr<Scope> scope,
               unique_ptr<VarExprAST> var,
               unique_ptr<ExprAST> end,
               unique_ptr<ExprAST> step,
               unique_ptr<ExprAST> body):
        ExprAST(scope),
        Var(std::move(var)),
        End(std::move(end)),
        Step(std::move(step)),
        Body(std::move(body)) {}

    Value * codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `For`, `Var`: %s, `End`: %s, `Step`: %s, `Body`: %s}", Var->dumpJSON().c_str(), End->dumpJSON().c_str(), Step->dumpJSON().c_str(), Body->dumpJSON().c_str());
    }
};

class UnaryExprAST : public ExprAST {
    char Opcode;
    unique_ptr<ExprAST> Operand;

public:
    UnaryExprAST(shared_ptr<Scope> scope, char opcode, unique_ptr<ExprAST> operand)
        : ExprAST(scope), Opcode(opcode), Operand(std::move(operand)) {}

    Value * codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `Unary`, `Operand`: `%s`}", Operand->dumpJSON().c_str());
    }
};

class NewAST : public ExprAST {
    VarType Type;
    unique_ptr<ExprAST> Size;

public:
    NewAST(shared_ptr<Scope> scope, VarType type, unique_ptr<ExprAST> size)
        : ExprAST(scope), Type(type), Size(std::move(size)) {}

    Value * codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `New`, `Type`: `%s`, `Size`: %s}", Type.dumpJSON().c_str(), Size->dumpJSON().c_str());
    }
};

class DeleteAST : public ExprAST {
    unique_ptr<ExprAST> Var;

public:
    DeleteAST(shared_ptr<Scope> scope, unique_ptr<ExprAST> var)
        : ExprAST(scope), Var(std::move(var)) {}

    Value * codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `Delete`, `Var`: %s}", Var->dumpJSON().c_str());
    }
};

class ReturnAST : public ExprAST {
    unique_ptr<ExprAST> Var;

public:
    ReturnAST(shared_ptr<Scope> scope) : ExprAST(scope) {}
    ReturnAST(shared_ptr<Scope> scope, unique_ptr<ExprAST> var)
        : ExprAST(scope), Var(std::move(var)) {}

    Value * codegen() override;
    string dumpJSON() override {
        return FormatString("{`type`: `Return`, `Var`: %s}", Var->dumpJSON().c_str());
    }
};

class MemberAST {
public:
    VarType VType;
    string Name;

    MemberAST(VarType type, string &name) : VType(type), Name(name) {}

    string dumpJSON()  {
        return FormatString("{`type`: `Member`, `Type`: %s, `Name`: `%s`}", VType.dumpJSON().c_str(), Name.c_str());
    };
};

class ClassDeclAST {
    SourceLocation Loc;
    shared_ptr<Scope> scope;
    string Name;
    vector<unique_ptr<MemberAST>> Members;
    vector<unique_ptr<FunctionAST>> Methods;

public:
  ClassDeclAST(shared_ptr<Scope> scope,
               SourceLocation loc,
               string name,
               vector<unique_ptr<MemberAST>> members,
               vector<unique_ptr<FunctionAST>> methods)
      : Loc(loc), scope(scope), Name(name),
        Members(std::move(members)), Methods(std::move(methods)) {}

    string& getName() { return Name; }
    const size_t getMemberSize() const { return Members.size(); }
    const MemberAST *getMember(size_t i) const { return Members[i].get(); }
    const unsigned indexOfMember(const string &MemName) const {
        unsigned idx = 0;
        for (auto E = Members.begin(); E != Members.end(); E ++, idx ++) {
            if ((*E)->Name == MemName) break;
        }
        return idx;
    }
    unsigned getMemoryBytes() {
        unsigned bytes = 0;
        for (auto E = Members.begin(); E != Members.end(); E ++) {
            bytes += (*E)->VType.getMemoryBytes();
        }
        return bytes;
    }
    StructType *codegen();
    string dumpJSON()  {
        return FormatString("{`Type`: `ClassDecl`, `Name`: `%s`}", Name.c_str());
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

class Parser {
    LLVMContext LLContext;
    IRBuilder<> *Builder;
    unique_ptr<Module> TheModule;
    std::unique_ptr<legacy::FunctionPassManager> TheFPM;
    std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
    map<char, int> BinOpPrecedence;
    std::unique_ptr<Lexer> TheLexer;
    std::string TopFuncName;
    std::string Filename;

    Token getCurTok() {
        return TheLexer->CurTok;
    }

    Token SkipColon() {
        if (getCurTok() == tok_colon) {
            return getNextToken();
        }
        return getCurTok();
    }

    int GetTokenPrecedence();

    VarType ParseType(shared_ptr<Scope> scope);

    unique_ptr<ExprAST> ParseIntegerLiteral(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseFloatLiteral(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseParenExpr(shared_ptr<Scope> scope);
    std::unique_ptr<ExprAST> ParseIdentifierExpr(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParsePrimary(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseExpr(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseBinOpRHS(shared_ptr<Scope> scope, int ExprPrec, unique_ptr<ExprAST> LHS);
    unique_ptr<ExprAST> ParseIfExpr(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseForExpr(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseUnary(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseVarExpr(shared_ptr<Scope> scope);
    unique_ptr<PrototypeAST> ParsePrototype(shared_ptr<Scope> scope, string &ClassName);
    unique_ptr<FunctionAST> ParseDefinition(shared_ptr<Scope> scope);
    unique_ptr<FunctionAST> ParseMethod(shared_ptr<Scope> scope, string &ClassName);
    unique_ptr<PrototypeAST> ParseExtern(shared_ptr<Scope> scope);
    unique_ptr<FunctionAST> ParseTopLevelExpr(shared_ptr<Scope> scope);
    unique_ptr<MemberAST> ParseMemberAST(shared_ptr<Scope> scope);
    unique_ptr<ClassDeclAST> ParseClassDecl(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseNew(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseDelete(shared_ptr<Scope> scope);
    unique_ptr<ExprAST> ParseReturn(shared_ptr<Scope> scope);

public:
    Parser(std::string src, std::string filename)
    : TheLexer(std::make_unique<Lexer>(src)), Filename(filename) {

        Builder = new IRBuilder<>(LLContext);

        BinOpPrecedence[tok_equal] = 2;
        BinOpPrecedence[tok_less] = 10;
        BinOpPrecedence[tok_greater] = 10;
        BinOpPrecedence[tok_add] = 20;
        BinOpPrecedence[tok_sub] = 20;
        BinOpPrecedence[tok_mul] = 40;
        BinOpPrecedence[tok_dot] = 50;

        getNextToken();

        InitializeModuleAndPassManager();

    }

    Token getNextToken();
    Token getCurToken() { return TheLexer->getCurToken(); }
    SourceLocation getCurLoc() { return TheLexer->CurLoc; }
    void InitializeModuleAndPassManager();
    void HandleDefinition(shared_ptr<Scope> scope);
    void HandleExtern(shared_ptr<Scope> scope);
    void HandleTopLevelExpression(shared_ptr<Scope> scope);

    static AllocaInst *CreateEntryBlockAlloca(Function *F, Type *T, const string &VarName);
    static AllocaInst *CreateEntryBlockAlloca(Function *F, VarExprAST *Var);
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
    LLVMContext &getContext() { return this->LLContext; };
    IRBuilder<> *getBuilder() { return Builder; };
    void RunFunction(Function *F) { TheFPM->run(*F); };
    void SetTopFuncName(std::string &FuncName) { TopFuncName = FuncName; };
    VarType getVarType(Token Tok) {
        switch (Tok) {
            case tok_type_void: return VarType(VarTypeVoid);
            case tok_type_bool: return VarType(VarTypeBool);
            case tok_type_int: return VarType(VarTypeInt);
            case tok_type_float: return VarType(VarTypeFloat);
            case tok_type_string: return VarType(VarTypeString);
            case tok_type_object: return VarType(VarTypeObject, TheLexer->IdentifierStr);
            default: return VarType(VarTypeUnkown);
        }
    }
};

extern unique_ptr<Parser> TheParser;

inline LLVMContext &getContext() { return TheParser->getContext(); }
inline IRBuilder<> *getBuilder() { return TheParser->getBuilder(); }

#endif /* Parser_hpp */
