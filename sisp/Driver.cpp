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

static int MainLoop() {
    auto scope = make_shared<Scope>();
    while (true) {
//        cout << "CurTok: " << tok_tos(TheParser->getCurToken()) << endl;
        switch (TheParser->getCurToken()) {
            case tok_eof:
                return 0;
            case tok_colon:
            case tok_right_bracket:
                TheParser->getNextToken();
                break;
//            case tok_def:
            case tok_type_bool:
            case tok_type_int:
            case tok_type_float:
            case tok_type_string:
            case tok_type_object:
                TheParser->HandleDefinition(scope);
                break;
            case tok_extern:
                TheParser->HandleExtern();
                break;
            default:
                TheParser->HandleTopLevelExpression(scope);
                break;
        }
    }
}

int compile(std::string &src, std::map<string, string> &opts) {

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    cout << src << endl;

    std::string jit = opts["jit"];
    TheParser = std::make_unique<Parser>(jit == "1", src);

    MainLoop();

    DBuilder->finalize();

    cout << "### Module Define ###" << endl;
    TheParser->getModule().print(outs(), nullptr);

    if (jit == "1")
        return 0;

    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    auto TargetTriple = sys::getDefaultTargetTriple();
    TheParser->getModule().setTargetTriple(TargetTriple);

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
    TheParser->getModule().setDataLayout(TheTargetMachine->createDataLayout());

    auto Filename = opts.find("out") != opts.end() ? opts["out"] : "output.o";
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

    legacy::PassManager Pass;
    auto FileType = llvm::TargetMachine::CGFT_ObjectFile;

    if (TheTargetMachine->addPassesToEmitFile(Pass, dest, nullptr, FileType)) {
        LogError("TheTargetMachine can't emit a file of this type");
        return 1;
    }

    Pass.run(TheParser->getModule());
    dest.flush();

    cout << "Wrote " << Filename << endl;

    return 0;
}
