//
//  Driver.cpp
//  sisp
//
//  Created by 徐可 on 2020/2/19.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#include <iostream>
#include <fstream>

#include "Lexer.hpp"
#include "Parser.hpp"
#include "Codegen.hpp"
#include "Driver.hpp"

using namespace std;
using namespace llvm;

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

int compile(std::string &src, std::map<string, string> *opts) {

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    BinOpPrecedence[tok_equal] = 2;
    BinOpPrecedence[tok_less] = 10;
    BinOpPrecedence[tok_add] = 20;
    BinOpPrecedence[tok_sub] = 20;
    BinOpPrecedence[tok_mul] = 40;

    TheCode = src;

//    cout << TheCode << endl;

    getNextToken();

    std::string jit = (*opts)["jit"];
    if (jit == "1")
        JITEnabled = true;

    TheJIT = std::make_unique<llvm::orc::SispJIT>();
    InitializeModuleAndPassManager();

    TheModule->addModuleFlag(Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);
    if (Triple(sys::getProcessTriple()).isOSDarwin())
        TheModule->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

    DBuilder = std::make_unique<DIBuilder>(*TheModule);
    SispDbgInfo.TheCU = DBuilder->createCompileUnit(dwarf::DW_LANG_C, DBuilder->createFile("ch10.sisp", "."), "Sisp Compiler", 0, "", 0);

    MainLoop();

    DBuilder->finalize();

//    cout << "### Module Define ###" << endl;
//    TheModule->print(outs(), nullptr);

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
