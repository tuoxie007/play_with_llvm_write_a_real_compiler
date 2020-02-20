//
//  Codegen.hpp
//  sisp
//
//  Created by 徐可 on 2020/2/19.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#ifndef Codegen_hpp
#define Codegen_hpp

#include "Parser.hpp"

struct DebugInfo {
    DICompileUnit *TheCU;
    DIType *DblTy;
    std::vector<DIScope *> LexicalBlocks;

    DIType *getDoubleTy();
    void emitLocation(ExprAST *AST);
};

extern DebugInfo SispDbgInfo;

extern std::unique_ptr<DIBuilder> DBuilder;

#endif /* Codegen_hpp */
