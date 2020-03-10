//
//  Codegen.cpp
//  sisp
//
//  Created by 徐可 on 2020/2/19.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#include "Codegen.hpp"
#include "llvm/IR/DerivedTypes.h"

#include <vector>

static vector<DIScope *> LexicalBlocks;

DIType *DebugInfo::getDoubleTy() {
    if (DblTy)
        return DblTy;

    DblTy = DBuilder->createBasicType("double", 64, dwarf::DW_ATE_float);
    return DblTy;
}

void DebugInfo::emitLocation(ExprAST *AST) {
    if (!AST)
        return TheParser->getBuilder()->SetCurrentDebugLocation(DebugLoc());

    DIScope *Scope;
    if (LexicalBlocks.empty())
        Scope = TheCU;
    else
        Scope = LexicalBlocks.back();

    TheParser->getBuilder()->SetCurrentDebugLocation(DebugLoc::get(AST->getLine(), AST->getCol(), Scope));
}

static DISubroutineType *CreateFunctionType(unsigned long NumArgs, DIFile *Unit) {
  SmallVector<Metadata *, 8> EltTys;
  DIType *DblTy = SispDbgInfo.getDoubleTy();

  // Add the result type.
  EltTys.push_back(DblTy);

  for (unsigned long i = 0, e = NumArgs; i != e; ++i)
      EltTys.push_back(DblTy);

  return DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(EltTys));
}

const std::string& FunctionAST::getName() const {
    return Proto->getName();
}

Value *IntegerLiteralAST::codegen() {
    SispDbgInfo.emitLocation(this);
    return ConstantInt::get(TheParser->getContext(), APInt(64, Val));
}

Value *FloatLiteralAST::codegen() {
    SispDbgInfo.emitLocation(this);
    return ConstantFP::get(TheParser->getContext(), APFloat(Val));
}

Value *VariableExprAST::codegen() {
    Value *V = scope->getVal(Name);
    if (!V)
        LogError("Unkown variable name");

//    cout << "VariableExprAST::codegen()" << V << endl;
    SispDbgInfo.emitLocation(this);

    // var
    return V;
}

Value *RightValueAST::codegen() {
    auto V = Expr->codegen();
    if (V->getType()->isPointerTy() && V->getType()->getPointerElementType()->isStructTy())
        return V;
    else if (V->getType()->isPointerTy())
        return TheParser->getBuilder()->CreateLoad(V, "rv");
    return V;
}

Value *BinaryExprAST::codegen() {
    SispDbgInfo.emitLocation(this);

    if (Op == tok_equal) {
        auto LHSE = static_cast<VariableExprAST *>(LHS.get());
        if (!LHSE)
            return LogErrorV("destination of '=' must be a variable");

        auto Val = RHS->codegen();
        if (!Val)
            return nullptr;

        auto Variable = scope->getVal(LHSE->getName());
        if (!Variable)
            return LogErrorV("Unkown variable name");

        TheParser->getBuilder()->CreateStore(Val, Variable);
        return Val;
    }

    auto L = LHS->codegen();
    auto R = RHS->codegen();
    if (!L || !R) {
        LogError("BinaryExpr codgen error.");
        return nullptr;
    }

    switch (Op) {
        case tok_add:
            if (L->getType()->isFloatTy() || L->getType()->isFloatTy())
                return TheParser->getBuilder()->CreateFAdd(L, R, "addtmp");
            else if (L->getType()->isIntegerTy() || R->getType()->isIntegerTy())
                return TheParser->getBuilder()->CreateAdd(L, R, "addtmp");
        case tok_sub:
            if (L->getType()->isDoubleTy() || L->getType()->isDoubleTy())
                return TheParser->getBuilder()->CreateFSub(L, R, "subtmp");
            else if (L->getType()->isIntegerTy() || R->getType()->isIntegerTy())
                return TheParser->getBuilder()->CreateSub(L, R, "subtmp");
        case tok_mul:
            if (L->getType()->isDoubleTy() || L->getType()->isDoubleTy())
                return TheParser->getBuilder()->CreateFMul(L, R, "multmp");
            else if (L->getType()->isIntegerTy() || R->getType()->isIntegerTy())
                return TheParser->getBuilder()->CreateMul(L, R, "multmp");
        case tok_less:
            if (L->getType()->isDoubleTy() || L->getType()->isDoubleTy()) {
                L = TheParser->getBuilder()->CreateFCmpULT(L, R, "lttmp");
                return TheParser->getBuilder()->CreateUIToFP(L, Type::getDoubleTy(TheParser->getContext()),
                "booltmp");
            }
            else if (L->getType()->isIntegerTy() || R->getType()->isIntegerTy()) {
                return TheParser->getBuilder()->CreateICmpSLT(L, R, "lttmp");
            }
            else
                assert(false && "not implemented");
        case tok_greater:
            if (L->getType()->isDoubleTy() || L->getType()->isDoubleTy()) {
                R = TheParser->getBuilder()->CreateFCmpUGT(L, R, "gttmp");
                return TheParser->getBuilder()->CreateUIToFP(R, Type::getDoubleTy(TheParser->getContext()),
                "booltmp");
            }
            else if (L->getType()->isIntegerTy() || R->getType()->isIntegerTy()) {
                return TheParser->getBuilder()->CreateICmpSGT(L, R, "gttmp");
            }
            else
                assert(false && "not implemented");
        default:
        {
            auto F = TheParser->getFunction(string("binary") + Op);
            assert(F && "binary operator not found!");
            auto Ops = { L, R };
            return TheParser->getBuilder()->CreateCall(F, Ops, "calltmp");
        }
    }
}

Value *MemberAccessAST::codegen() {
    auto V = Var->codegen();
    string StructName;
    if (V->getType()->isPointerTy() && V->getType()->getPointerElementType()->isStructTy()) {
        StructName = V->getType()->getPointerElementType()->getStructName();
    } else {
        return LogErrorV("fail to get struct name from var");
    }
    auto PrefixLen = string("class.").length();
    string ClassName = StructName.substr(PrefixLen, StructName.length() - PrefixLen);
    auto ClsDecl = scope->getClass(ClassName);
    if (!ClsDecl)
        return LogErrorV(string("Class not found: ") + ClassName);
    unsigned Idx = ClsDecl->indexOfMember(Member);
    VarType VT = ClsDecl->getMember(Idx)->VType;
    auto MT = VT.getType(TheParser->getContext());
    auto ElePtr = TheParser->getBuilder()->CreateStructGEP(V, Idx, string(".") + Member); // i64*
    if (RHS) {
        Value *RVal;
        RVal = RHS->codegen(); //i64
        if (!RVal)
            return nullptr;
        TheParser->getBuilder()->CreateStore(RHS->codegen(), ElePtr);
        return RVal;
    }
    return TheParser->getBuilder()->CreateLoad(MT, ElePtr);
}

Value *IndexerAST::codegen() {
    auto V = Var->codegen();
    if (V->getType()->isPointerTy()) {

        auto Idx = Index->codegen();
        auto ElePtr = TheParser->getBuilder()->CreateGEP(V, Idx);

        auto EleTy = V->getType()->getPointerElementType();
        if (RHS) {
            Value *RVal;
            RVal = RHS->codegen();
            if (!RVal)
                return nullptr;
            TheParser->getBuilder()->CreateStore(RVal, ElePtr);
            return RVal;
        }
        return TheParser->getBuilder()->CreateLoad(EleTy, ElePtr, "idxVal");
    } else {
        return LogErrorV("fail to get index of non pointer type");
    }
}

Value *NewAST::codegen() {
    unsigned Sizeof = Type.getMemoryBytes();
    auto Cap = TheParser->getBuilder()->CreateMul(Size->codegen(), ConstantInt::get(TheParser->getContext(), APInt(64, Sizeof)));
    auto MallocF = TheParser->getModule().getFunction("malloc");
    Value *SizeArg[] = { Cap };
    auto Ptr = TheParser->getBuilder()->CreateCall(MallocF, SizeArg, "ptr");
    auto ObjPtr = TheParser->getBuilder()->CreateBitCast(Ptr, Type.getType(TheParser->getContext())->getPointerTo(), "new");
    return ObjPtr;
}

Value *IfExprAST::codegen() {
    SispDbgInfo.emitLocation(this);
    auto CondV = Cond->codegen();
    if (!CondV)
        return nullptr;
    CondV = TheParser->getBuilder()->CreateICmpNE(CondV, ConstantInt::get(TheParser->getContext(), APInt(1, 0)), "ifcond");

    auto F = TheParser->getBuilder()->GetInsertBlock()->getParent();

    auto ThenBlock = BasicBlock::Create(TheParser->getContext(), "then", F);
    auto ElseBlock = BasicBlock::Create(TheParser->getContext(), "else");
    auto MergeBlock = BasicBlock::Create(TheParser->getContext(), "ifcont");

    TheParser->getBuilder()->CreateCondBr(CondV, ThenBlock, ElseBlock);

    TheParser->getBuilder()->SetInsertPoint(ThenBlock);

    auto ThenV = Then->codegen();
    if (!ThenV)
        return nullptr;

    TheParser->getBuilder()->CreateBr(MergeBlock);

    ThenBlock = TheParser->getBuilder()->GetInsertBlock();

    F->getBasicBlockList().push_back(ElseBlock);
    TheParser->getBuilder()->SetInsertPoint(ElseBlock);

    auto ElseV = Else->codegen();
    if (!ElseV)
        return nullptr;

    TheParser->getBuilder()->CreateBr(MergeBlock);

    ElseBlock = TheParser->getBuilder()->GetInsertBlock();

    F->getBasicBlockList().push_back(MergeBlock);
    TheParser->getBuilder()->SetInsertPoint(MergeBlock);

    auto PN = TheParser->getBuilder()->CreatePHI(ThenV->getType(), 2, "iftmp");

    PN->addIncoming(ThenV, ThenBlock);
    PN->addIncoming(ElseV, ElseBlock);

    return PN;
}

Value *ForExprAST::codegen() {
    auto F = TheParser->getBuilder()->GetInsertBlock()->getParent();

    SispDbgInfo.emitLocation(this);

    auto StartVal = Var->codegen();
    if (!StartVal)
        return nullptr;

    auto Alloca = getScope()->getVal(Var->getName());

    auto TestBlock = BasicBlock::Create(TheParser->getContext(), "test", F);
    auto LoopBlock = BasicBlock::Create(TheParser->getContext(), "loop", F);
    auto AfterBlock = BasicBlock::Create(TheParser->getContext(), "afterloop", F);

    TheParser->getBuilder()->CreateBr(TestBlock);

    // %test:
    TheParser->getBuilder()->SetInsertPoint(TestBlock);

    auto LoopCond = End->codegen();
    if (!LoopCond)
        return nullptr;

    // if %loopcond is true; then go to %loop; else goto %afterloop
    TheParser->getBuilder()->CreateCondBr(LoopCond, LoopBlock, AfterBlock);

    // %loop:
    TheParser->getBuilder()->SetInsertPoint(LoopBlock);
    if (!Body->codegen())
        return nullptr;

    Value *StepVal = nullptr;
    if (Step) {
        StepVal = Step->codegen();
        if (!StepVal)
            return nullptr;
    } else {
        StepVal = ConstantInt::get(TheParser->getContext(), APInt(64, 1));
    }
    // i = i + %StepVal
    auto CurVar = TheParser->getBuilder()->CreateLoad(Alloca, Var->getName());
    auto NextVar = TheParser->getBuilder()->CreateAdd(CurVar, StepVal, "nextvar");
    TheParser->getBuilder()->CreateStore(NextVar, Alloca);
    // goto %test
    TheParser->getBuilder()->CreateBr(TestBlock);

    // %afterloop:
    TheParser->getBuilder()->SetInsertPoint(AfterBlock);

    return Constant::getNullValue(Type::getInt64Ty(TheParser->getContext()));
}

Value *VarExprAST::codegen() {

    auto F = TheParser->getBuilder()->GetInsertBlock()->getParent();

    Value *InitVal;
    if (Init) {
        InitVal = Init->codegen();
        scope->setVal(Name, InitVal);
        return InitVal;
//        if (InitVal->getType()->isPointerTy()) {
//        }
//        auto Load = TheParser->getBuilder()->CreateLoad(InitVal->getType()->getPointerTo(), InitVal, "var");
//        scope->setVal(Name, Load);
//        return Load;
    } else {
        InitVal = Type.getDefaultValue(TheParser->getContext());
    }

    AllocaInst *Alloca = Parser::CreateEntryBlockAlloca(F, this);
    TheParser->getBuilder()->CreateStore(InitVal, Alloca);

    scope->setVal(Name, Alloca);

    return Alloca;
}

Value *UnaryExprAST::codegen() {
    auto OperandV = Operand->codegen();
    if (!OperandV)
        return nullptr;

    auto F = TheParser->getFunction(string("unary") + Opcode);
    if (!F)
        return LogErrorV("Unkown unary operator");

    SispDbgInfo.emitLocation(this);
    return TheParser->getBuilder()->CreateCall(F, OperandV, "unop");
}

Value *CompoundExprAST::codegen() {
    int i = 0;
    Value *RetVal = nullptr;
    for (auto Expr = Exprs.begin(); Expr != Exprs.end(); Expr ++) {
        cout << "subExpr " << to_string(i++) << endl;
        RetVal = (*Expr)->codegen();
//        if (!RetVal)
//            return nullptr;
    }
    return RetVal ?: Constant::getNullValue(Type::getInt64Ty(TheParser->getContext()));;
}

Value *CallExprAST::codegen() {
    SispDbgInfo.emitLocation(this);
    // Look up the name in the global module table.
    Function *CalleeF = TheParser->getFunction(Callee);
    if (!CalleeF) {
        auto ClassType = scope->getClassType(Callee);
        if (!ClassType)
            return LogErrorV((string("Unknown function referenced ") + Callee));

        // %ptr = malloc()
        auto Bytes = scope->getClass(Callee)->getMemoryBytes();
        auto MallocF = TheParser->getModule().getFunction("malloc");
        Value *SizeArg[] = {ConstantInt::get(Type::getInt64Ty(TheParser->getContext()), Bytes)};
        auto Ptr = TheParser->getBuilder()->CreateCall(MallocF, SizeArg, "ptr");

        // %obj = bitcase %ptr
        auto ObjPtr = TheParser->getBuilder()->CreateBitCast(Ptr, ClassType->getPointerTo(), "obj");

        return ObjPtr;

//        auto F = TheParser->getBuilder()->GetInsertBlock()->getParent();
//        auto Alloca = Parser::CreateEntryBlockAlloca(F, ClassType, "obj");
//        return TheParser->getBuilder()->CreateBitCast(Alloca, ClassType->getPointerTo());
    }

    // If argument mismatch error.
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");

    std::vector<Value *> ArgsV;
    for (unsigned long i = 0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back())
            return nullptr;
    }

    return TheParser->getBuilder()->CreateCall(CalleeF, ArgsV, "calltmp");
}

Value * MethodCallAST::codegen() {
    auto V = Var->codegen();
    string StructName = V->getType()->getPointerElementType()->getStructName();
    auto PrefixLen = string("class.").length();
    string ClassName = StructName.substr(PrefixLen, StructName.length() - PrefixLen);
    string Fn = ClassName + "$" + Callee;
    // Look up the name in the global module table.
    Function *CalleeF = TheParser->getFunction(Fn);
    if (!CalleeF) {
        return LogErrorV(string("method not found: ") + Callee);
    }

    // If argument mismatch error.
    if (CalleeF->arg_size() != Args.size() + 1)
        return LogErrorV("Incorrect arguments passed");

    std::vector<Value *> ArgsV;
    ArgsV.push_back(V);
    for (unsigned long i = 0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back())
            return nullptr;
    }

    return TheParser->getBuilder()->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen() {
    vector<Type *> ArgTypes;
    for (auto E = Args.begin(); E != Args.end(); E ++) {
        Type *ArgType = (*E)->getIRType(TheParser->getContext());
        ArgTypes.push_back(ArgType);
    }
    Type *TheRetType = getType(RetType, TheParser->getContext());
    FunctionType *FT = FunctionType::get(TheRetType, ArgTypes, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheParser->getModule());
    unsigned long Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(Args[Idx++]->getName());
    return F;
}

Function *FunctionAST::codegen() {
    // Transfer ownership of the prototype to the FunctionProtos map, but keep a
    // reference to it for use below.
    auto &P = *Proto;
    cout << "codegen: " << P.getName() << endl;
    TheParser->AddFunctionProtos(std::move(Proto));
    Function *F = TheParser->getFunction(P.getName());
    if (!F)
        return nullptr;

    if (P.isBinaryOp())
        TheParser->SetBinOpPrecedence(P.getOperatorName(), P.getBinaryPrecedence());

    // Create a new basic block to start insertion into.
    auto BB = BasicBlock::Create(TheParser->getContext(), "entry", F);
    TheParser->getBuilder()->SetInsertPoint(BB);

    // Create a subprogram DIE for this function.
    DIFile *Unit = DBuilder->createFile(SispDbgInfo.TheCU->getFilename(),
                                        SispDbgInfo.TheCU->getDirectory());
    DIScope *FContext = Unit;
    unsigned LineNo = P.getLine();
    unsigned ScopeLine = LineNo;
    DISubprogram *SP =
    DBuilder->createFunction(FContext,
                             P.getName(),
                             StringRef(),
                             Unit,
                             LineNo,
                             CreateFunctionType(F->arg_size(), Unit),
                             ScopeLine,
                             DINode::FlagPrototyped,
                             DISubprogram::SPFlagDefinition);
    F->setSubprogram(SP);

    // Push the current scope.
    SispDbgInfo.LexicalBlocks.push_back(SP);

    // Unset the location for the prologue emission (leading instructions with no
    // location in a function are considered part of the prologue and the debugger
    // will run past them when breaking on a function)
    SispDbgInfo.emitLocation(nullptr);

    unsigned ArgIdx = 0;
    for (auto &Arg : F->args()) {
        auto ArgTy = Arg.getType();
//        Body->getScope()->setVal(Arg.getName(), &Arg);
        auto Alloca = Parser::CreateEntryBlockAlloca(F, ArgTy, Arg.getName());

        // Create a debug descriptor for the variable.
        DILocalVariable *D = DBuilder->createParameterVariable(
            SP, Arg.getName(), ++ArgIdx, Unit, LineNo, SispDbgInfo.getDoubleTy(),
            true);

        DBuilder->insertDeclare(Alloca, D, DBuilder->createExpression(),
                                DebugLoc::get(LineNo, 0, SP),
                                TheParser->getBuilder()->GetInsertBlock());

        TheParser->getBuilder()->CreateStore(&Arg, Alloca);
        auto ArgLocal = TheParser->getBuilder()->CreateLoad(Alloca);
        Body->getScope()->setVal(Arg.getName(), ArgLocal);
    }

    SispDbgInfo.emitLocation(Body.get());

    if (Value *RetVal = Body->codegen()) {
        // Finish off the function.
        TheParser->getBuilder()->CreateRet(RetVal);

        SispDbgInfo.LexicalBlocks.pop_back();

        // Validate the generated code, checking for consistency.
        verifyFunction(*F);

        if (TheParser->isJITEnabled()) {
            // Run the optimizer on the function.
            TheParser->RunFunction(F);
        }

        return F;
    }

    // Error reading body, remove function.
    F->eraseFromParent();

    if (P.isBinaryOp())
        TheParser->SetBinOpPrecedence(Proto->getOperatorName(), -1);

    SispDbgInfo.LexicalBlocks.pop_back();

    return nullptr;
}

StructType * ClassDeclAST::codegen() {
    vector<Type *> Tys;
    for (auto E = Members.begin(); E != Members.end(); E ++) {
        Tys.push_back((*E)->VType.getType(TheParser->getContext()));
    }
    auto ST = StructType::create(TheParser->getContext(), Tys, string("class") + "." + Name, false);
    scope->setClassType(Name, ST);

    for (auto E = Methods.begin(); E != Methods.end(); E ++)
        (*E)->codegen();

    return ST;
}
