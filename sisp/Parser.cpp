//
//  Parser.cpp
//  sisp
//
//  Created by 徐可 on 2020/2/19.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#include "Parser.hpp"
#include "GlobalVars.hpp"

using namespace llvm;
using namespace std;

#define make_unique std::make_unique

std::unique_ptr<DIBuilder> DBuilder;
DebugInfo SispDbgInfo;
unique_ptr<Parser> TheParser;

string FunctionAST::dumpJSON() {
    return FormatString("{`type`: `Function`, `Prototype`: %s, `Body`: %s}", Proto->dumpJSON().c_str(), Body->dumpJSON().c_str());
}

ExprAST::ExprAST(shared_ptr<Scope> scope) {
    this->scope = scope;
    this->Loc = TheParser->getCurLoc();
}

Token Parser::getNextToken() {
    return TheLexer->getNextToken();
}

unique_ptr<ExprAST> Parser::ParseIntegerLiteral(shared_ptr<Scope> scope) {
    auto Result = make_unique<IntegerLiteralAST>(scope, (long)TheLexer->IntegerVal);
    getNextToken();

    SkipColon();
    return std::move(Result);
}

unique_ptr<ExprAST> Parser::ParseFloatLiteral(shared_ptr<Scope> scope) {
    auto Result = make_unique<FloatLiteralAST>(scope, TheLexer->FloatVal);
    getNextToken();

    SkipColon();
    return std::move(Result);
}

unique_ptr<ExprAST> Parser::ParseParenExpr(shared_ptr<Scope> scope) {
    getNextToken();
    auto V = ParseExpr(scope);
    if (!V)
        return nullptr;

    if (getCurTok() != tok_right_paren)
        return LogError("expected ')'");

    getNextToken();

    SkipColon();

    return V;
}

std::unique_ptr<ExprAST> Parser::ParseIdentifierExpr(shared_ptr<Scope> scope) {
    std::string IdName = TheLexer->IdentifierStr;

    SourceLocation LitLoc = TheLexer->CurLoc;

    getNextToken(); // eat identifier.

    if (getCurTok() != tok_left_paren) {// Simple variable ref.
        SkipColon();
        return make_unique<VariableExprAST>(scope, LitLoc, IdName);
    }

    // Call.
    getNextToken(); // eat (
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (getCurTok() != tok_right_paren) {
        while (true) {
            if (auto Arg = ParseExpr(scope))
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (getCurTok() == tok_right_paren)
                break;

            if (getCurTok() != tok_comma)
                return LogError("Expected ')' or ',' in argument list");

            getNextToken();
        }
    }

    // Eat the ')'.
    getNextToken();

    SkipColon();

    return make_unique<CallExprAST>(scope, TheLexer->CurLoc, IdName, std::move(Args));
}

unique_ptr<ExprAST> Parser::ParsePrimary(shared_ptr<Scope> scope) {
    switch (getCurTok()) {
        case tok_identifier:
            if (scope->getClass(TheLexer->IdentifierStr)) {
                if (TheLexer->getNextToken(1) == tok_left_paren) { // constructor
                    return ParseIdentifierExpr(scope);
                }
                return ParseVarExpr(scope);
            }
            return ParseIdentifierExpr(scope);
        case tok_integer_literal:
            return ParseIntegerLiteral(scope);
        case tok_float_literal:
            return ParseFloatLiteral(scope);
        case tok_left_paren:
            return ParseParenExpr(scope);
        case tok_if:
            return ParseIfExpr(scope);
        case tok_for:
            return ParseForExpr(scope);
        case tok_new:
            return TheParser->ParseNew(scope);
        case tok_del:
            return TheParser->ParseDelete(scope);
        case tok_ret:
            return TheParser->ParseReturn(scope);
        case tok_type_void:
        case tok_type_bool:
        case tok_type_int:
        case tok_type_float:
        case tok_type_string:
        case tok_type_object:
            return ParseVarExpr(scope);
        default:
            return LogError(std::string("unkown token when execepting an expression: ") + tok_tos(getCurTok()));
    }
}

int Parser::GetTokenPrecedence() {
//    cout << "GetTokenPrecedence" << tok_tos(getCurTok()) << endl;
    if (!isascii(getCurTok())) {
        return -1;
    }

    int TokPrec = BinOpPrecedence[getCurTok()];
    if (TokPrec <= 0) {
        return -1;
    }

    return TokPrec;
}

unique_ptr<ExprAST> Parser::ParseExpr(shared_ptr<Scope> scope) {
    if (getCurTok() == tok_left_bracket) {

        getNextToken();

        vector<unique_ptr<ExprAST>> Exprs;
        auto localScope = make_shared<Scope>(scope);
        while (true) {
            if (getCurTok() == tok_right_bracket) {
                getNextToken();
                break;
            }
            auto Expr = ParseExpr(localScope);
            if (!Expr)
                return nullptr;

            DLog(DLT_AST, Expr->dumpJSON());

            Exprs.push_back(std::move(Expr));
        }

        return make_unique<CompoundExprAST>(localScope, std::move(Exprs));
    }

    auto LHS = ParseUnary(scope);
    if (!LHS)
        return nullptr;

    auto Bin = ParseBinOpRHS(scope, 0, std::move(LHS));
    SkipColon();
    return Bin;
}

unique_ptr<ExprAST> Parser::ParseBinOpRHS(shared_ptr<Scope> scope,
                                         int ExprPrec,
                                         unique_ptr<ExprAST> LHS) {
    while (true) {
        int TokPrec = GetTokenPrecedence();

        if (TokPrec < ExprPrec) {
            return LHS;
        }

        char BinOp = getCurTok();
        SourceLocation BinLoc = TheLexer->CurLoc;

        if (getCurTok() == tok_dot) {
            getNextToken();

            if (getCurTok() != tok_identifier) {
                return LogError("expected identifier after '.'");
            }

            string MemName = TheLexer->IdentifierStr;
            getNextToken();

            if (getCurTok() == tok_equal) {
                getNextToken();

                auto RHS = ParseExpr(scope);
                if (!RHS)
                    LogError("expected expression after member assignment");

                auto RV = make_unique<RightValueAST>(scope, std::move(RHS));
//                auto LV = make_unique<RightValueAST>(scope, std::move(LHS));

                return make_unique<MemberAccessAST>(scope, std::move(LHS), MemName, std::move(RV));
            }

            if (getCurTok() == tok_left_paren) { // method call
                getNextToken();
                vector<unique_ptr<ExprAST>> Args;
//                Args.push_back(std::move(LHS));
                if (getCurTok() != tok_right_paren) {
                    while (true) {
                        if (auto Arg = ParseExpr(scope))
                            Args.push_back(std::move(Arg));
                        else
                            return nullptr;

                        if (getCurTok() == tok_right_paren)
                            break;

                        if (getCurTok() != tok_comma)
                            return LogError("Expected ')' or ',' in argument list");

                        getNextToken();
                    }

                    getNextToken();

                    return make_unique<MethodCallAST>(scope, std::move(LHS), MemName, std::move(Args));
                }
            }

            SkipColon();

//            auto LV = make_unique<RightValueAST>(scope, std::move(LHS));
            return make_unique<MemberAccessAST>(scope, std::move(LHS), MemName);
        }

        getNextToken();

        auto RHS = ParseUnary(scope);
        if (!RHS) {
            return nullptr;
        }

        int NextPrec = GetTokenPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(scope, TokPrec + 1, std::move(RHS));
            if (!RHS) {
                return nullptr;
            }
        }

        LHS = make_unique<BinaryExprAST>(scope, BinLoc, BinOp, std::move(LHS), std::move(RHS));
    }
}

unique_ptr<ExprAST> Parser::ParseIfExpr(shared_ptr<Scope> scope) {
    SourceLocation IfLoc = TheLexer->CurLoc;

    getNextToken();

    auto IfScope = make_shared<Scope>(scope);

    auto Cond = ParseExpr(IfScope);
    if (!Cond)
        return nullptr;

    if (getCurTok() == tok_then)
        getNextToken();

    auto Then = ParseExpr(IfScope);
    if (!Then)
        return nullptr;

    if (getCurTok() != tok_else)
        return LogError("expected else");
    getNextToken();

    auto Else = ParseExpr(IfScope);
    if (!Else)
        return nullptr;

    SkipColon();

    return make_unique<IfExprAST>(IfScope, IfLoc, std::move(Cond), std::move(Then), std::move(Else));
}

unique_ptr<ExprAST> Parser::ParseForExpr(shared_ptr<Scope> scope) {
    getNextToken();

    if (getCurTok() != tok_left_paren)
        return LogError("expected '(' after for");
    getNextToken();

    if (getCurTok() != tok_type_int)
        return LogError("expected int");

    shared_ptr<Scope> ForScope = make_shared<Scope>(scope);

    auto Var = unique_ptr<VarExprAST>(static_cast<VarExprAST *>(ParseVarExpr(scope).release()));

    SkipColon();

    auto End = ParseExpr(ForScope);
    if (!End)
        return nullptr;

    unique_ptr<ExprAST> Step;
    SkipColon();

    if (getCurTok() == tok_integer_literal) {
        Step = ParseExpr(ForScope);
        if (!Step)
            return nullptr;
    }

    if (getCurTok() != tok_right_paren)
        return LogError("expected ')' in for");
    getNextToken();

    auto Body = ParseExpr(ForScope);
    if (!Body)
        return nullptr;


    SkipColon();
    return make_unique<ForExprAST>(ForScope,
                                   std::move(Var),
                                   std::move(End),
                                   std::move(Step),
                                   std::move(Body));
}

unique_ptr<ExprAST> Parser::ParseUnary(shared_ptr<Scope> scope) {
    if (!isascii(getCurTok()) || // literal
        getCurTok() == tok_left_paren || // ()
        getCurTok() == tok_comma) {// ,

        auto LHS = ParsePrimary(scope);

        if (getCurTok() == tok_left_square) { // '['
            getNextToken(); // eat '['

            auto Idx = ParseExpr(scope);
            if (!Idx) {
                return LogError("expected index expr after '['");
            }

            if (getCurTok() != tok_right_square) { // ']'
                return LogError("expected '[' after index expr");
            }
            getNextToken();

            if (getCurTok() == tok_equal) {
                getNextToken();
                auto Value = ParseExpr(scope);
                SkipColon();
                auto RV = make_unique<RightValueAST>(scope, std::move(Value));
                return make_unique<IndexerAST>(scope, std::move(LHS), std::move(Idx), std::move(RV));
            } else {
                SkipColon();
                return make_unique<IndexerAST>(scope, std::move(LHS), std::move(Idx));
            }
        }

        return LHS;
    }

    Token Opc = getCurTok();
    getNextToken();

    if (auto Operand = ParseUnary(scope))
        return make_unique<UnaryExprAST>(scope, Opc, std::move(Operand));

    return nullptr;
}

VarType Parser::ParseType(shared_ptr<Scope> scope) {
    Token Tok = getCurTok();
    VarType Type = getVarType(Tok);
    if (Type.TypeID == VarTypeUnkown) {
        if (scope->getClass(TheLexer->IdentifierStr)) {
            Type = VarType(VarTypeObject, TheLexer->IdentifierStr);
        } else {
            LogError("unkown var type");
            return Type;
        }
    }
    getNextToken();

    if (getCurTok() == tok_star) {
        Type = VarType::getPointerType(Type);
        getNextToken();
    }

    return Type;
}

unique_ptr<ExprAST> Parser::ParseVarExpr(shared_ptr<Scope> scope) {
    VarType Type = ParseType(scope);

    string Name;
    if (getCurTok() == tok_identifier) {
        Name = TheLexer->IdentifierStr;
        getNextToken();
    }

    unique_ptr<ExprAST> Init;
    if (getCurTok() == tok_equal) {
        getNextToken();

        Init = ParseExpr(scope);
        if (!Init)
            return nullptr;
    }
    SkipColon();

    return make_unique<VarExprAST>(scope, Type, Name, std::move(Init));
}

unique_ptr<PrototypeAST> Parser::ParsePrototype(shared_ptr<Scope> scope, string &ClassName) {

    SourceLocation FnLoc = TheLexer->CurLoc;
//    Token Type = getCurTok();
    VarType RetType = ParseType(scope);
    string FnName;

    unsigned Kind = 0;
    unsigned BinaryPrecedence = 30;

    switch (getCurTok()) {
        case tok_identifier:
            FnName = TheLexer->IdentifierStr;
            if (ClassName.length()) {
                FnName = ClassName + "$" + FnName;
            }
            Kind = 0;
            getNextToken();
            break;
        case tok_unary:
            getNextToken();
            if (!isascii(getCurTok()))
                return LogErrorP("Excpeted unary operator");
            FnName = string("unary") + (char)getCurTok();
            Kind = 1;
            getNextToken();
            break;
        case tok_binary:
            getNextToken();
            if (!isascii(getCurTok()))
                return LogErrorP("Expected binary operator");
            FnName = "binary";
            FnName += (char)getCurTok();
            Kind = 2;
            getNextToken();

            if (getCurTok() == tok_integer_literal) {
                if (TheLexer->IntegerVal < 1 || TheLexer->IntegerVal > 100)
                    return LogErrorP("Invalid precedence: must be 1..100");
                BinaryPrecedence = (unsigned)TheLexer->IntegerVal;
                getNextToken();
            }
            break;
        default:
            return LogErrorP("Expected function name in prototype");
    }

    if (getCurTok() != tok_left_paren) {
        return LogErrorP("Expected '(' in prototype");
    }

    if (!scope) {
        scope = make_shared<Scope>();
    }

    vector<unique_ptr<VarExprAST>> Args;
    if (ClassName.length()) {
        VarType Type = VarType(VarTypeObject, ClassName);
        auto ThisArg = make_unique<VarExprAST>(scope, Type, "this", unique_ptr<ExprAST>());
        Args.push_back(std::move(ThisArg));
    }
    getNextToken();

    while (TheLexer->getVarType()) {
        auto ArgE = ParseVarExpr(scope);
        auto Arg = unique_ptr<VarExprAST>(static_cast<VarExprAST *>(ArgE.release()));
        Args.push_back(std::move(Arg));
        if (getCurTok() == tok_comma)
            getNextToken();
    }
    if (isspace(getCurTok())) {
        getNextToken();
    }
    if (getCurTok() != tok_right_paren) {
        return LogErrorP(string("Expected ')' in prototype but ") + (char)getCurTok());
    }

    getNextToken();

    if (Kind && Args.size() != Kind)
        return LogErrorP("Invalid number of operands for operator");

    return make_unique<PrototypeAST>(FnLoc, RetType, FnName, std::move(Args), Kind != 0, BinaryPrecedence);
}

unique_ptr<FunctionAST> Parser::ParseDefinition(shared_ptr<Scope> scope) {
    string Empty;
    auto Proto = ParsePrototype(scope, Empty);
    if (!Proto) {
        return nullptr;
    }

    if (auto E = ParseExpr(scope))
        return make_unique<FunctionAST>(std::move(Proto), std::move(E));

    return nullptr;
}

unique_ptr<FunctionAST> Parser::ParseMethod(shared_ptr<Scope> scope, string &ClassName) {
    auto Proto = ParsePrototype(scope, ClassName);
    if (!Proto) {
        return nullptr;
    }

    if (auto E = ParseExpr(scope))
        return make_unique<FunctionAST>(std::move(Proto), std::move(E));

    return nullptr;
}

unique_ptr<PrototypeAST> Parser::ParseExtern(shared_ptr<Scope> scope) {
    getNextToken();
    string Empty;
    return ParsePrototype(scope, Empty);
}

unique_ptr<FunctionAST> Parser::ParseTopLevelExpr(shared_ptr<Scope> scope) {
    SourceLocation FnLoc = TheLexer->CurLoc;
    if (auto E = ParseExpr(scope)) {
        VarType RetType(VarTypeInt);
        auto Proto = make_unique<PrototypeAST>(FnLoc, RetType, TopFuncName, vector<unique_ptr<VarExprAST>>());
        return make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

unique_ptr<MemberAST> Parser::ParseMemberAST(shared_ptr<Scope> scope) {
    Token type = getCurTok();
    // TODO support all types
    if (type != tok_type_int &&
        type != tok_type_bool &&
        type != tok_type_float) {
        LogError("unimplemented member type");
        return nullptr;
    }
    getNextToken();

    if (getCurTok() != tok_identifier) {
        LogError("expected identifier after member type");
        return nullptr;
    }

    string name = TheLexer->IdentifierStr;
    getNextToken();

    if (getCurTok() != tok_colon) {
        LogError("expected ';' after member name");
        return nullptr;
    }
    getNextToken();

    return make_unique<MemberAST>(VarType(type), name);
}

unique_ptr<ClassDeclAST> Parser::ParseClassDecl(shared_ptr<Scope> scope) {
    SourceLocation ClsLoc = TheLexer->CurLoc;

    getNextToken();
    string Name = TheLexer->IdentifierStr;

    getNextToken();
    if (getCurTok() != tok_left_bracket) {
        LogError("expected '{' after class name");
        return nullptr;
    }
    getNextToken();

    vector<unique_ptr<MemberAST>> Members;
    vector<unique_ptr<FunctionAST>> Methods;
    while (getCurTok() != tok_right_bracket) {
        if (TheLexer->getNextToken(2) == tok_left_paren) {
            if (auto Method = ParseMethod(scope, Name)) {
                DLog(DLT_AST, Method->dumpJSON());
                Methods.push_back(std::move(Method));
            } else {
                LogError("Parse Method failed");
            }
        } else {
            auto Member = ParseMemberAST(scope);
            Members.push_back(std::move(Member));
        }
    }

    getNextToken();
    return make_unique<ClassDeclAST>(scope, ClsLoc, Name, std::move(Members), std::move(Methods));
}

unique_ptr<ExprAST> Parser::ParseNew(shared_ptr<Scope> scope) {
    getNextToken(); // eat "new"
    VarType Type = ParseType(scope);

    unique_ptr<ExprAST> Size = make_unique<IntegerLiteralAST>(scope, 1);
    if (getCurTok() == tok_left_paren) {
        getNextToken();
        Size = ParseExpr(scope);

        if (getCurTok() != tok_right_paren) {
            return LogError("expected ')' after new type");
        }
        getNextToken();

        SkipColon();
    }
    return make_unique<NewAST>(scope, Type, std::move(Size));
}

unique_ptr<ExprAST> Parser::ParseDelete(shared_ptr<Scope> scope) {
    getNextToken();

    if (getCurTok() != tok_identifier)
        return LogError("expected variable");

    auto LitLoc = TheLexer->CurLoc;
    auto VarName = TheLexer->getIdentifier();
    getNextToken();
    SkipColon();
    auto Var = make_unique<VariableExprAST>(scope, LitLoc, VarName);
    return make_unique<DeleteAST>(scope, std::move(Var));
}

unique_ptr<ExprAST> Parser::ParseReturn(shared_ptr<Scope> scope) {
    getNextToken();
    auto Var = ParseExpr(scope);
    SkipColon();
    return make_unique<ReturnAST>(scope, std::move(Var));
}

void Parser::InitializeModuleAndPassManager() {
    TheModule = make_unique<Module>(Filename, LLContext);

    TheModule->addModuleFlag(Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);
    if (Triple(sys::getProcessTriple()).isOSDarwin())
        TheModule->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

    DBuilder = make_unique<DIBuilder>(*TheModule);
    SispDbgInfo.TheCU = DBuilder->createCompileUnit(dwarf::DW_LANG_C, DBuilder->createFile(Filename, "."), "Sisp Compiler", 0, "", 0);

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

void Parser::HandleDefinition(shared_ptr<Scope> scope) {
    if (getCurTok() == tok_class) {
        if (auto ClsDecl = ParseClassDecl(scope)) {
            DLog(DLT_AST, ClsDecl->dumpJSON());
            auto C = ClsDecl.get();
            scope->appendClass(ClsDecl->getName(), std::move(ClsDecl));
            C->codegen();
        } else {
            LogError("Parse ClassDecl failed");
        }
    } else if (TheLexer->getNextToken(2) == tok_left_paren) {
        if (auto FnAST = ParseDefinition(scope)) {
            DLog(DLT_AST, FnAST->dumpJSON());
            FnAST->codegen();
        } else {
            LogError("Parse Function failed");
        }
    } else {
        HandleTopLevelExpression(scope);
    }
}

void Parser::HandleExtern(shared_ptr<Scope> scope) {
    if (auto ProtoAST = ParseExtern(scope)) {
        DLog(DLT_AST, ProtoAST->dumpJSON());
        if (auto *FnIR = ProtoAST->codegen()) {
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    } else {
        LogError("parse extern failed");
    }
}

void Parser::HandleTopLevelExpression(shared_ptr<Scope> scope) {
    if (auto FnAST = ParseTopLevelExpr(scope)) {
        DLog(DLT_AST, FnAST->dumpJSON());
        FnAST->codegen();
    } else {
        LogError("parse top level expr failed");
    }
}

AllocaInst * Parser::CreateEntryBlockAlloca(Function *F, Type *T, const string &VarName) {
    IRBuilder<> TmpBlock(&F->getEntryBlock(), F->getEntryBlock().begin());
    return TmpBlock.CreateAlloca(T, 0, VarName);
}

AllocaInst * Parser::CreateEntryBlockAlloca(Function *F, VarExprAST *Var) {
    return Parser::CreateEntryBlockAlloca(F, Var->getIRType(F->getContext()), Var->getName());
}

Function * Parser::getFunction(std::string Name) {
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
