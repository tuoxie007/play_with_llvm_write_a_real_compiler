//
//  cli.cpp
//  sisp
//
//  Created by 徐可 on 2020/2/20.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#include "Driver.hpp"
#include <iostream>
#include <fstream>
#include <string>
#include <filesystem>

#define TEST "test_extern.sisp"

#ifdef TEST

int main(int argc, const char * argv[]) {
    
    std::string testsDir = std::string(PROJECT_DIR) + "/sisp/sisp11/tests";
    for (const auto & entry : std::__fs::filesystem::directory_iterator(testsDir)) {

        if (entry.path().filename() != TEST) continue;

        std::cout << "📟 start building " << entry.path().filename() << std::endl;

        std::string src;
        std::ifstream t(entry.path());

        t.seekg(0, std::ios::end);
        src.reserve(t.tellg());
        t.seekg(0, std::ios::beg);

        src.assign((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());

        std::map<std::string, std::string> opts;
        opts["jit"] = "0";
        compile(src, &opts);
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
    compile(src);
    return 0;
}

#endif
