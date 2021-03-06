//
//  cli.cpp
//  play
//
//  Created by Jason Hsu on 2020/2/20.
//  Copyright © 2020 Jason Hsu<tuoxie007@gmail.com>. All rights reserved.
//

#include "Driver.hpp"
#include <iostream>
#include <fstream>
#include <string>
#include <filesystem>

//#define TEST "def"
//#define TEST "extern"
//#define TEST "constant"
//#define TEST "math"
//#define TEST "less"
//#define TEST "less_float"
//#define TEST "greater"
//#define TEST "greater_float"
//#define TEST "call"
//#define TEST "var"
//#define TEST "var_init"
//#define TEST "var_assign"
#define TEST "ifcond"
//#define TEST "forloop"
//#define TEST "formula"
//#define TEST "compound"
//#define TEST "link"
//#define TEST "class_member"
//#define TEST "class_method"
//#define TEST "new_int"
//#define TEST "int_pointer"
//#define TEST "int_indexer"
//#define TEST "int_pointer_arg"
//#define TEST "delete_ptr"

#ifdef TEST

int main(int argc, const char * argv[]) {

    std::string testsDir = std::string(PROJECT_DIR) + "/play/tests";
    for (const auto & entry : std::__fs::filesystem::directory_iterator(testsDir)) {

        auto filename = entry.path().filename();
        if (filename != std::string(TEST) + ".play") continue;

        std::cout << "📟 start building " << entry.path().filename() << std::endl;

        std::string src;
        std::ifstream t(entry.path());

        t.seekg(0, std::ios::end);
        src.reserve(t.tellg());
        t.seekg(0, std::ios::beg);

        src.assign((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());

        std::map<std::string, std::string> opts;
        opts["jit"] = "0";
        opts["out"] = testsDir + "/" + TEST + ".o";
        opts["obj"] = std::string(TEST) != "exec" ? "1" : "0";

        std::string module = std::string(filename);
        compile(module, src, opts);
    }
    return 0;
}

#else

int main(int argc, const char * argv[]) {
    std::string src;
    if (argc == 0) {
        while(true) {
            char c = getchar();
            if (c == EOF)
                break;
            src += c;
        }
    } else {
        std::ifstream t(argv[0]);

        t.seekg(0, std::ios::end);
        src.reserve(t.tellg());
        t.seekg(0, std::ios::beg);

        src.assign((std::istreambuf_iterator<char>(t)),
                    std::istreambuf_iterator<char>());
    }
    compile("stdin", src);
    return 0;
}

#endif
