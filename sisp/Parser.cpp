//
//  Parser.cpp
//  sisp
//
//  Created by 徐可 on 2020/2/19.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#include "Parser.hpp"

using namespace llvm;
using namespace std;

#define make_unique std::make_unique

std::unique_ptr<DIBuilder> DBuilder;
DebugInfo SispDbgInfo;
unique_ptr<Parser> TheParser;

string FunctionAST::dumpJSON() {
    return FormatString("{`type`: `Function`, `Prototype`: %s, `Body`: %s", Proto->dumpJSON().c_str(), Body->dumpJSON().c_str());
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

    if (TheLexer->CurTok == tok_colon) {
        getNextToken();
    }
    return move(Result);
}

unique_ptr<ExprAST> Parser::ParseFloatLiteral(shared_ptr<Scope> scope) {
    auto Result = make_unique<FloatLiteralAST>(scope, TheLexer->FloatVal);
    getNextToken();

    if (TheLexer->CurTok == tok_colon) {
        getNextToken();
    }
    return move(Result);
}

unique_ptr<ExprAST> Parser::ParseParenExpr(shared_ptr<Scope> scope) {
    getNextToken();
    auto V = ParseExpr(scope);
    if (!V)
        return nullptr;

    if (TheLexer->CurTok != tok_right_paren)
        return LogError("expected ')'");

    getNextToken();

    if (TheLexer->CurTok == tok_colon) {
        getNextToken();
    }
    return V;
}

std::unique_ptr<ExprAST> Parser::ParseIdentifierExpr(shared_ptr<Scope> scope) {
    std::string IdName = TheLexer->IdentifierStr;

    SourceLocation LitLoc = TheLexer->CurLoc;

    getNextToken(); // eat identifier.

    if (TheLexer->CurTok != tok_left_paren) {// Simple variable ref.
        if (TheLexer->CurTok == tok_colon)
            getNextToken();
        return make_unique<VariableExprAST>(scope, LitLoc, IdName);
    }

    // Call.
    getNextToken(); // eat (
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (TheLexer->CurTok != tok_right_paren) {
        while (true) {
            if (auto Arg = ParseExpr(scope))
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (TheLexer->CurTok == tok_right_paren)
                break;

            if (TheLexer->CurTok != tok_comma)
                return LogError("Expected ')' or ',' in argument list");

                getNextToken();
        }
    }

    // Eat the ')'.
    getNextToken();

    if (TheLexer->CurTok == tok_colon) {
        getNextToken();
    }

    return make_unique<CallExprAST>(scope, TheLexer->CurLoc, IdName, std::move(Args));
}

unique_ptr<ExprAST> Parser::ParsePrimary(shared_ptr<Scope> scope) {
    switch (TheLexer->CurTok) {
        case tok_identifier:
            // TODO find the user-defined type
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
        case tok_type_bool:
        case tok_type_int:
        case tok_type_float:
        case tok_type_string:
        case tok_type_object:
            return ParseVarExpr(scope);
        default:
            return LogError(std::string("unkown token when execepting an expression: ") + tok_tos(TheLexer->CurTok));
    }
}

int Parser::GetTokenPrecedence() {
//    cout << "GetTokenPrecedence" << tok_tos(TheLexer->CurTok) << endl;
    if (!isascii(TheLexer->CurTok)) {
        return -1;
    }

    int TokPrec = BinOpPrecedence[TheLexer->CurTok];
    if (TokPrec <= 0) {
        return -1;
    }

    return TokPrec;
}

unique_ptr<ExprAST> Parser::ParseExpr(shared_ptr<Scope> scope) {
    if (TheLexer->CurTok == tok_left_bracket) {

        getNextToken();

        vector<unique_ptr<ExprAST>> Exprs;
        auto localScope = make_shared<Scope>(scope);
        while (true) {
            if (TheLexer->CurTok == tok_right_bracket) {
                getNextToken();
                break;
            }
            auto Expr = ParseExpr(localScope);
            if (!Expr)
                return nullptr;

            Exprs.push_back(move(Expr));
        }

        return make_unique<CompoundExprAST>(localScope, move(Exprs));
    }

    auto LHS = ParseUnary(scope);
    if (!LHS)
        return nullptr;

    return ParseBinOpRHS(scope, 0, move(LHS));
}

unique_ptr<ExprAST> Parser::ParseBinOpRHS(shared_ptr<Scope> scope,
                                         int ExprPrec,
                                         unique_ptr<ExprAST> LHS) {
    while (true) {
        int TokPrec = GetTokenPrecedence();

        if (TokPrec < ExprPrec) {
            return LHS;
        }

        char BinOp = TheLexer->CurTok;
        SourceLocation BinLoc = TheLexer->CurLoc;
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

        LHS = make_unique<BinaryExprAST>(scope, BinLoc, BinOp, move(LHS), move(RHS));
    }
}

unique_ptr<ExprAST> Parser::ParseIfExpr(shared_ptr<Scope> scope) {
    SourceLocation IfLoc = TheLexer->CurLoc;

    getNextToken();

    auto IfScope = make_shared<Scope>(scope);

    auto Cond = ParseExpr(IfScope);
    if (!Cond)
        return nullptr;

    if (TheLexer->CurTok == tok_then)
        getNextToken();

    auto Then = ParseExpr(IfScope);
    if (!Then)
        return nullptr;

    if (TheLexer->CurTok != tok_else)
        return LogError("expected else");
    getNextToken();

    auto Else = ParseExpr(IfScope);
    if (!Else)
        return nullptr;

    if (TheLexer->CurTok == tok_colon) {
        getNextToken();
    }
    return make_unique<IfExprAST>(IfScope, IfLoc, move(Cond), move(Then), move(Else));
}

unique_ptr<ExprAST> Parser::ParseForExpr(shared_ptr<Scope> scope) {
    getNextToken();

    if (TheLexer->CurTok != tok_left_paren)
        return LogError("expected '(' after for");
    getNextToken();

    if (TheLexer->CurTok != tok_type_int)
        return LogError("expected int");

    shared_ptr<Scope> ForScope = make_shared<Scope>(scope);

    auto Var = unique_ptr<VarExprAST>(static_cast<VarExprAST *>(ParseVarExpr(scope).release()));

    if (TheLexer->CurTok == tok_colon)
        getNextToken();

    auto End = ParseExpr(ForScope);
    if (!End)
        return nullptr;

    unique_ptr<ExprAST> Step;
    if (TheLexer->CurTok == tok_colon) {
        getNextToken();
    }

    if (TheLexer->CurTok == tok_integer_literal) {
        Step = ParseExpr(ForScope);
        if (!Step)
            return nullptr;
    }

    if (TheLexer->CurTok != tok_right_paren)
        return LogError("expected ')' in for");
    getNextToken();

    auto Body = ParseExpr(ForScope);
    if (!Body)
        return nullptr;


    if (TheLexer->CurTok == tok_colon) {
        getNextToken();
    }
    return make_unique<ForExprAST>(ForScope,
                                   move(Var),
//                                   move(Start),
                                   move(End),
                                   move(Step),
                                   move(Body));
}

unique_ptr<ExprAST> Parser::ParseUnary(shared_ptr<Scope> scope) {
    if (!isascii(TheLexer->CurTok) ||
        TheLexer->CurTok == tok_left_paren ||
        TheLexer->CurTok == tok_comma)
        return ParsePrimary(scope);

    char Opc = TheLexer->CurTok;
    getNextToken();

    if (auto Operand = ParseUnary(scope))
        return make_unique<UnaryExprAST>(scope, Opc, move(Operand));

    return nullptr;
}

unique_ptr<ExprAST> Parser::ParseVarExpr(shared_ptr<Scope> scope) {

    Token Type = TheLexer->CurTok;
    getNextToken();

    if (TheLexer->CurTok != tok_identifier) {
        return LogError("expected id after type");
    }
    string Name = TheLexer->IdentifierStr;
    getNextToken();

    unique_ptr<ExprAST> Init;
    if (TheLexer->CurTok == tok_equal) {
        getNextToken();

        Init = ParseExpr(scope);
        if (!Init)
            return nullptr;
    }
    if (TheLexer->CurTok == tok_colon) {
        getNextToken();
    }

    return make_unique<VarExprAST>(scope, Type, Name, move(Init));
}

unique_ptr<PrototypeAST> Parser::ParsePrototype() {

    SourceLocation FnLoc = TheLexer->CurLoc;
    Token Type = TheLexer->CurTok;
    getNextToken();
    string FnName;

    unsigned Kind = 0;
    unsigned BinaryPrecedence = 30;

    switch (TheLexer->CurTok) {
        case tok_identifier:
            FnName = TheLexer->IdentifierStr;
            Kind = 0;
            getNextToken();
            break;
        case tok_unary:
            getNextToken();
            if (!isascii(TheLexer->CurTok))
                return LogErrorP("Excpeted unary operator");
            FnName = string("unary") + (char)TheLexer->CurTok;
            Kind = 1;
            getNextToken();
            break;
        case tok_binary:
            getNextToken();
            if (!isascii(TheLexer->CurTok))
                return LogErrorP("Expected binary operator");
            FnName = "binary";
            FnName += (char)TheLexer->CurTok;
            Kind = 2;
            getNextToken();

            if (TheLexer->CurTok == tok_integer_literal) {
                if (TheLexer->IntegerVal < 1 || TheLexer->IntegerVal > 100)
                    return LogErrorP("Invalid precedence: must be 1..100");
                BinaryPrecedence = (unsigned)TheLexer->IntegerVal;
                getNextToken();
            }
            break;
        default:
            return LogErrorP("Expected function name in prototype");
    }

    if (TheLexer->CurTok != tok_left_paren) {
        return LogErrorP("Expected '(' in prototype");
    }

    vector<unique_ptr<VarExprAST>> Args;
    getNextToken();
    auto scope = make_shared<Scope>();
    while (TheLexer->getVarType()) {
        auto ArgE = ParseVarExpr(scope);
        auto Arg = unique_ptr<VarExprAST>(static_cast<VarExprAST *>(ArgE.release()));
        Args.push_back(move(Arg));
        if (TheLexer->CurTok == tok_comma)
            getNextToken();
    }
    if (isspace(TheLexer->CurTok)) {
        getNextToken();
    }
    if (TheLexer->CurTok != tok_right_paren) {
        return LogErrorP(string("Expected ')' in prototype but ") + (char)TheLexer->CurTok);
    }

    getNextToken();

    if (Kind && Args.size() != Kind)
        return LogErrorP("Invalid number of operands for operator");

    return make_unique<PrototypeAST>(FnLoc, Type, FnName, move(Args), Kind != 0, BinaryPrecedence);
}

unique_ptr<FunctionAST> Parser::ParseDefinition(shared_ptr<Scope> scope) {
    auto Proto = ParsePrototype();
    if (!Proto) {
        return nullptr;
    }

    if (auto E = ParseExpr(scope))
        return make_unique<FunctionAST>(move(Proto), move(E));

    return nullptr;
}

unique_ptr<PrototypeAST> Parser::ParseExtern() {
    getNextToken();
    return ParsePrototype();
}

unique_ptr<FunctionAST> Parser::ParseTopLevelExpr(shared_ptr<Scope> scope) {
    SourceLocation FnLoc = TheLexer->CurLoc;
    if (auto E = ParseExpr(scope)) {
        auto Proto = make_unique<PrototypeAST>(FnLoc, tok_type_int, TopFuncName, vector<unique_ptr<VarExprAST>>());
        return make_unique<FunctionAST>(move(Proto), move(E));
    }
    return nullptr;
}

unique_ptr<MemberAST> Parser::ParseMemberAST(shared_ptr<Scope> scope) {
    Token type = TheLexer->CurTok;
    // TODO support all types
    if (type != tok_type_int && type != tok_type_bool && type != tok_type_float) {
        LogError("unimplemented member type");
        return nullptr;
    }
    getNextToken();

    if (TheLexer->CurTok != tok_identifier) {
        LogError("expected identifier after member type");
        return nullptr;
    }

    string name = TheLexer->IdentifierStr;
    getNextToken();

    if (TheLexer->CurTok != tok_colon) {
        LogError("expected ';' after member name");
        return nullptr;
    }
    getNextToken();

    return make_unique<MemberAST>(type, name);
}

unique_ptr<ClassDeclAST> Parser::ParseClassDecl(shared_ptr<Scope> scope) {
    SourceLocation ClsLoc = TheLexer->CurLoc;

    getNextToken();
    string Name = TheLexer->IdentifierStr;

    getNextToken();
    if (TheLexer->CurTok != tok_left_bracket) {
        LogError("expected '{' after class name");
        return nullptr;
    }

    getNextToken();
    vector<unique_ptr<MemberAST>> Members;
    while (TheLexer->CurTok != tok_right_bracket) {
        auto Member = ParseMemberAST(scope);
        Members.push_back(move(Member));
    }

    getNextToken();
    return make_unique<ClassDeclAST>(scope, ClsLoc, Name, move(Members));
}

void Parser::InitializeModuleAndPassManager() {
    // Open a new module.
    TheModule = make_unique<Module>("Sisp Demo", LLContext);
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

void Parser::HandleDefinition(shared_ptr<Scope> scope) {
    if (TheLexer->CurTok == tok_class) {
        if (auto ClsDecl = ParseClassDecl(scope)) {
            cout << ClsDecl->dumpJSON() << endl;
            ClsDecl->codegen();
            scope->appendClass(move(ClsDecl));
        } else {
            LogError("Parse ClassDecl failed");
        }
    } else if (TheLexer->getNextToken(2) == tok_left_paren) {
        if (auto FnAST = ParseDefinition(scope)) {
            cout << FnAST->dumpJSON() << endl;
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
            LogError("Parse Function failed");
        }
    } else {
        HandleTopLevelExpression(scope);
    }
}

void Parser::HandleExtern() {
    if (auto ProtoAST = ParseExtern()) {
        cout << ProtoAST->dumpJSON() << endl;
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

void Parser::HandleTopLevelExpression(shared_ptr<Scope> scope) {
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr(scope)) {
        cout << FnAST->dumpJSON() << endl;
        if (FnAST->codegen()) {
            if (JITEnabled) {
                // JIT the module containing the anonymous expression, keeping a handle so
                // we can free it later.
                auto H = TheJIT->addModule(std::move(TheModule));
                InitializeModuleAndPassManager();

                // Search the JIT for the __anon_expr symbol.
                auto ExprSymbol = TheJIT->findSymbol(TopFuncName);
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

AllocaInst * Parser::CreateEntryBlockAlloca(Function *F, Type *T, const string &VarName) {
    IRBuilder<> TmpBlock(&F->getEntryBlock(), F->getEntryBlock().begin());
    return TmpBlock.CreateAlloca(T, 0, VarName);
}

AllocaInst * Parser::CreateEntryBlockAlloca(Function *F, VarExprAST *Var) {
    return Parser::CreateEntryBlockAlloca(F, getType(Var->getType(), F->getContext()), Var->getName());
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
