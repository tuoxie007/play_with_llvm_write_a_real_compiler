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

bool JITEnabled = false;
LLVMContext TheContext;
unique_ptr<Module> TheModule;
std::unique_ptr<llvm::orc::SispJIT> TheJIT;
std::unique_ptr<legacy::FunctionPassManager> TheFPM;
std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

map<char, int> BinOpPrecedence;

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

void InitializeModuleAndPassManager() {
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

void HandleDefinition(shared_ptr<Scope> scope) {
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

void HandleExtern() {
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

void HandleTopLevelExpression(shared_ptr<Scope> scope) {
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
