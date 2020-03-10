//
//  Dirver.hpp
//  sisp
//
//  Created by 徐可 on 2020/2/20.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#ifndef Dirver_h
#define Dirver_h

#include <map>
#include <string>

extern int compile(std::string &filename, std::string &src, std::map<std::string, std::string> &opts);

#endif /* Dirver_h */
