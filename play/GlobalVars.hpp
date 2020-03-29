//
//  GlobalVars.hpp
//  play
//
//  Created by Jason Hsu on 2020/2/21.
//  Copyright © 2020 Jason Hsu<tuoxie007@gmail.com>. All rights reserved.
//

#ifndef GlobalVars_h
#define GlobalVars_h

#include <string>
#include <iostream>

enum DLogTag {
    DLT_SRC,
    DLT_TOK,
    DLT_AST,
    DLT_IR,
    DLT_OTH,
};

inline void DLog(DLogTag Tag, std::string Msg) {
    std::cout << Msg << std::endl;
}

#endif /* GlobalVars_h */
