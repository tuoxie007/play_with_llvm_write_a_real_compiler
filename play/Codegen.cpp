//
//  Codegen.cpp
//  play
//
//  Created by Jason Hsu on 2020/2/19.
//  Copyright © 2020 Jason Hsu<tuoxie007@gmail.com>. All rights reserved.
//

#include <vector>
#include "llvm/IR/DerivedTypes.h"
#include "Codegen.hpp"
#include "GlobalVars.hpp"

static vector<DIScope *> LexicalBlocks;

DIType *DebugInfo::getDoubleTy() {
    if (DblTy)
        return DblTy;

    DblTy = DBuilder->createBasicType("double", 64, dwarf::DW_ATE_float);
    return DblTy;
}

void DebugInfo::emitLocation(ExprAST *AST) {
    if (!AST)
        return getBuilder()->SetCurrentDebugLocation(DebugLoc());

    DIScope *Scope;
    if (LexicalBlocks.empty())
        Scope = TheCU;
    else
        Scope = LexicalBlocks.back();

    getBuilder()->SetCurrentDebugLocation(DebugLoc::get(AST->getLine(), AST->getCol(), Scope));
}

static DISubroutineType *CreateFunctionType(unsigned long NumArgs, DIFile *Unit) {
  SmallVector<Metadata *, 8> EltTys;
  DIType *DblTy = PlayDbgInfo.getDoubleTy();

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
    PlayDbgInfo.emitLocation(this);
    return ConstantInt::get(getContext(), APInt(64, Val));
}

Value *FloatLiteralAST::codegen() {
    PlayDbgInfo.emitLocation(this);
    return ConstantFP::get(getContext(), APFloat(Val));
}

Value *VariableExprAST::codegen() {
    Value *V = scope->getVal(Name);
    if (!V)
        LogError("Unkown variable name");

//    cout << "VariableExprAST::codegen()" << V << endl;
    PlayDbgInfo.emitLocation(this);

    // var
    return V;
}

Value *RightValueAST::codegen() {
    auto V = Expr->codegen();
    if (V->getType()->isPointerTy() && V->getType()->getPointerElementType()->isStructTy())
        return V;
    else if (V->getType()->isPointerTy())
        return getBuilder()->CreateLoad(V, "rv");
    return V;
}

Value *BinaryExprAST::codegen() {
    PlayDbgInfo.emitLocation(this);

    if (Op == tok_equal) { // assign

        auto LHSRV = static_cast<RightValueAST *>(LHS.get());

        auto LD = LHSRV->getExpr()->codegen();
        auto Val = RHS->codegen();
        if (!Val)
            return LogErrorV("RHS codegen return null");

        getBuilder()->CreateStore(Val, LD);

        return getBuilder()->CreateLoad(LD);
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
                return getBuilder()->CreateFAdd(L, R, "addtmp");
            else if (L->getType()->isIntegerTy() || R->getType()->isIntegerTy())
                return getBuilder()->CreateAdd(L, R, "addtmp");
        case tok_sub:
            if (L->getType()->isDoubleTy() || L->getType()->isDoubleTy())
                return getBuilder()->CreateFSub(L, R, "subtmp");
            else if (L->getType()->isIntegerTy() || R->getType()->isIntegerTy())
                return getBuilder()->CreateSub(L, R, "subtmp");
        case tok_mul:
            if (L->getType()->isDoubleTy() || L->getType()->isDoubleTy())
                return getBuilder()->CreateFMul(L, R, "multmp");
            else if (L->getType()->isIntegerTy() || R->getType()->isIntegerTy())
                return getBuilder()->CreateMul(L, R, "multmp");
        case tok_less:
            if (L->getType()->isDoubleTy() || L->getType()->isDoubleTy()) {
                L = getBuilder()->CreateFCmpULT(L, R, "lttmp");
                return getBuilder()->CreateUIToFP(L, Type::getDoubleTy(getContext()),
                "booltmp");
            }
            else if (L->getType()->isIntegerTy() || R->getType()->isIntegerTy()) {
                return getBuilder()->CreateICmpSLT(L, R, "lttmp");
            }
            else
                assert(false && "not implemented");
        case tok_greater:
            if (L->getType()->isDoubleTy() || L->getType()->isDoubleTy()) {
                R = getBuilder()->CreateFCmpUGT(L, R, "gttmp");
                return getBuilder()->CreateUIToFP(R, Type::getDoubleTy(getContext()),
                "booltmp");
            }
            else if (L->getType()->isIntegerTy() || R->getType()->isIntegerTy()) {
                return getBuilder()->CreateICmpSGT(L, R, "gttmp");
            }
            else
                assert(false && "not implemented");
        default:
        {
            auto F = TheParser->getFunction(string("binary") + Op);
            assert(F && "binary operator not found!");
            auto Ops = { L, R };
            return getBuilder()->CreateCall(F, Ops, "calltmp");
        }
    }
}

Value *MemberAccessAST::codegen() {
    auto V = Var->codegen();
    V = getBuilder()->CreateLoad(V);
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
    auto MT = VT.getType(getContext());
    auto ElePtr = getBuilder()->CreateStructGEP(V, Idx, string(".") + Member); // i64*
    if (RHS) {
        Value *RVal;
        RVal = RHS->codegen(); //i64
        if (!RVal)
            return nullptr;
        getBuilder()->CreateStore(RHS->codegen(), ElePtr);
        return RVal;
    }
    return getBuilder()->CreateLoad(MT, ElePtr);
}

Value *IndexerAST::codegen() {
    auto V = Var->codegen();
    V = getBuilder()->CreateLoad(V);
    if (V->getType()->isPointerTy()) {

        auto Idx = Index->codegen();
        auto ElePtr = getBuilder()->CreateGEP(V, Idx);

        auto EleTy = V->getType()->getPointerElementType();
        if (RHS) {
            Value *RVal;
            RVal = RHS->codegen();
            if (!RVal)
                return nullptr;
            getBuilder()->CreateStore(RVal, ElePtr);
//            return RVal;
        }
        return getBuilder()->CreateLoad(EleTy, ElePtr, "idxVal");
    } else {
        return LogErrorV("fail to get index of non pointer type");
    }
}

Value *NewAST::codegen() {
    unsigned Sizeof = Type.getMemoryBytes();
    auto Cap = getBuilder()->CreateMul(Size->codegen(), ConstantInt::get(getContext(), APInt(64, Sizeof)));
    auto MallocF = TheParser->getModule().getFunction("malloc");
    Value *SizeArg[] = { Cap };
    auto Ptr = getBuilder()->CreateCall(MallocF, SizeArg, "ptr");
    auto ObjPtr = getBuilder()->CreateBitCast(Ptr, Type.getType(getContext())->getPointerTo(), "new");
    return ObjPtr;
}

Value *DeleteAST::codegen() {
    auto ReleaseF = TheParser->getModule().getFunction("free");
    getBuilder()->CreateCall(ReleaseF, Var->codegen());
    return Constant::getNullValue(Type::getVoidTy(getContext()));
}

Value *ReturnAST::codegen() {
    auto F = getBuilder()->GetInsertBlock()->getParent();
    auto RT = F->getReturnType();
    if (!Var && RT->isVoidTy())
        return getBuilder()->CreateRetVoid();

    auto RV = Var->codegen();
    switch (RT->getTypeID()) {
        case llvm::Type::IntegerTyID:
            if (RV->getType()->isIntegerTy()) {
                if (RV->getType()->getIntegerBitWidth() == RT->getIntegerBitWidth()) {
                    break;
                }
                RV = getBuilder()->CreateZExtOrTrunc(RV, RT);
                break;
            }
            if (RV->getType()->isDoubleTy()) {
                RV = getBuilder()->CreateFPToSI(RV, RT);
                break;
            }
            return LogErrorV("expected int value to return");
        case llvm::Type::DoubleTyID:
            if (RV->getType()->isDoubleTy()) {
                break;
            }
            if (RV->getType()->isIntegerTy()) {
                RV = getBuilder()->CreateSIToFP(RV, RT);
                break;
            }
            return LogErrorV("expected float value to return");
        case llvm::Type::PointerTyID:
            RV = getBuilder()->CreateBitCast(RV, RT);
            break;
        case llvm::Type::VoidTyID:
            return LogErrorV("unexpected return type");
        default:
            return LogErrorV("unexpected return type");
    }
    return getBuilder()->CreateRet(RV);
}

Value *IfExprAST::codegen() {
    PlayDbgInfo.emitLocation(this);

    auto CondV = Cond->codegen();
    if (!CondV)
        return nullptr;
    CondV = getBuilder()->CreateICmpNE(CondV, ConstantInt::get(getContext(), APInt(1, 0)), "ifcond");

    auto F = getBuilder()->GetInsertBlock()->getParent();

    auto ThenBlock = BasicBlock::Create(getContext(), "then", F);
    auto ElseBlock = BasicBlock::Create(getContext(), "else");
    auto MergeBlock = BasicBlock::Create(getContext(), "fi");

    getBuilder()->CreateCondBr(CondV, ThenBlock, ElseBlock);

    getBuilder()->SetInsertPoint(ThenBlock);

    Then->codegen();

    getBuilder()->CreateBr(MergeBlock);

    ThenBlock = getBuilder()->GetInsertBlock();

    F->getBasicBlockList().push_back(ElseBlock);
    getBuilder()->SetInsertPoint(ElseBlock);

    Else->codegen();

    getBuilder()->CreateBr(MergeBlock);

    ElseBlock = getBuilder()->GetInsertBlock();

    F->getBasicBlockList().push_back(MergeBlock);
    getBuilder()->SetInsertPoint(MergeBlock);

    return nullptr;

//    auto PN = getBuilder()->CreatePHI(ThenV->getType(), 2, "iftmp");
//
//    PN->addIncoming(ThenV, ThenBlock);
//    PN->addIncoming(ElseV, ElseBlock);

//    return PN;
}

Value *ForExprAST::codegen() {
    auto F = getBuilder()->GetInsertBlock()->getParent();

    PlayDbgInfo.emitLocation(this);

    auto StartVal = Var->codegen();
    if (!StartVal)
        return nullptr;

    auto Alloca = getScope()->getVal(Var->getName());

    auto TestBlock = BasicBlock::Create(getContext(), "test", F);
    auto LoopBlock = BasicBlock::Create(getContext(), "loop", F);
    auto AfterBlock = BasicBlock::Create(getContext(), "afterloop", F);

    getBuilder()->CreateBr(TestBlock);

    // %test:
    getBuilder()->SetInsertPoint(TestBlock);

    auto LoopCond = End->codegen();
    if (!LoopCond)
        return nullptr;

    // if %loopcond is true; then go to %loop; else goto %afterloop
    getBuilder()->CreateCondBr(LoopCond, LoopBlock, AfterBlock);

    // %loop:
    getBuilder()->SetInsertPoint(LoopBlock);
    Body->codegen();

    Value *StepVal = nullptr;
    if (Step) {
        StepVal = Step->codegen();
        if (!StepVal)
            return nullptr;
    } else {
        StepVal = ConstantInt::get(getContext(), APInt(64, 1));
    }
    // i = i + %StepVal
    auto CurVar = getBuilder()->CreateLoad(Alloca, Var->getName());
    auto NextVar = getBuilder()->CreateAdd(CurVar, StepVal, "nextvar");
    getBuilder()->CreateStore(NextVar, Alloca);
    // goto %test
    getBuilder()->CreateBr(TestBlock);

    // %afterloop:
    getBuilder()->SetInsertPoint(AfterBlock);

    return Constant::getNullValue(Type::getInt64Ty(getContext()));
}

Value *VarExprAST::codegen() {

    auto F = getBuilder()->GetInsertBlock()->getParent();

    Value *InitVal;
    if (Init) {
        InitVal = Init->codegen();
//        scope->setVal(Name, InitVal);
//        return InitVal;
    } else {
        InitVal = Type.getDefaultValue(getContext());
    }

    AllocaInst *Alloca = Parser::CreateEntryBlockAlloca(F, this);
    getBuilder()->CreateStore(InitVal, Alloca);

    scope->setVal(Name, Alloca);

    return Alloca;
}

Value *UnaryExprAST::codegen() {
    auto OperandV = Operand->codegen();
    if (!OperandV)
        return nullptr;

    switch (Opcode) {
        case tok_sub:
            {
                auto V = Operand->codegen();
                if (V->getType()->isIntegerTy())
                    return getBuilder()->CreateNeg(V);
                return getBuilder()->CreateFNeg(V);
            }
        case tok_add:
            return Operand->codegen();

        default:
            break;
    }

    auto F = TheParser->getFunction(string("unary") + Opcode);
    if (!F)
        return LogErrorV("Unkown unary operator");

    PlayDbgInfo.emitLocation(this);
    return getBuilder()->CreateCall(F, OperandV, "unop");
}

Value *CompoundExprAST::codegen() {
//    int i = 0;
    for (auto Expr = Exprs.begin(); Expr != Exprs.end(); Expr ++) {
//        cout << "subExpr " << to_string(i++) << endl;
        (*Expr)->codegen();
    }
    return nullptr;
}

Value *CallExprAST::codegen() {
    PlayDbgInfo.emitLocation(this);
    // Look up the name in the global module table.
    Function *CalleeF = TheParser->getFunction(Callee);
    if (!CalleeF) {
        auto ClassType = scope->getClassType(Callee);
        if (!ClassType)
            return LogErrorV((string("Unknown function referenced ") + Callee));

        // %ptr = malloc()
        auto Bytes = scope->getClass(Callee)->getMemoryBytes();
        auto MallocF = TheParser->getModule().getFunction("malloc");
        Value *SizeArg[] = {ConstantInt::get(Type::getInt64Ty(getContext()), Bytes)};
        auto Ptr = getBuilder()->CreateCall(MallocF, SizeArg, "ptr");

        // %obj = bitcase %ptr
        auto ObjPtr = getBuilder()->CreateBitCast(Ptr, ClassType->getPointerTo(), "obj");

        return ObjPtr;

//        auto F = getBuilder()->GetInsertBlock()->getParent();
//        auto Alloca = Parser::CreateEntryBlockAlloca(F, ClassType, "obj");
//        return getBuilder()->CreateBitCast(Alloca, ClassType->getPointerTo());
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

    return getBuilder()->CreateCall(CalleeF, ArgsV, "calltmp");
}

Value * MethodCallAST::codegen() {
    auto V = Var->codegen();
    V = getBuilder()->CreateLoad(V);
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

    return getBuilder()->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen() {
    vector<Type *> ArgTypes;
    for (auto E = Args.begin(); E != Args.end(); E ++) {
        Type *ArgType = (*E)->getIRType(getContext());
        ArgTypes.push_back(ArgType);
    }
    Type *TheRetType = RetType.getType(getContext());
    FunctionType *FT = FunctionType::get(TheRetType, ArgTypes, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheParser->getModule());
    unsigned long Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(Args[Idx++]->getName());
    return F;
}

Function *FunctionAST::codegen() {
    auto &P = *Proto;
    cout << "codegen: " << P.getName() << endl;
    TheParser->AddFunctionProtos(std::move(Proto));
    Function *F = TheParser->getFunction(P.getName());
    if (!F)
        return nullptr;

    if (P.isBinaryOp())
        TheParser->SetBinOpPrecedence(P.getOperatorName(), P.getBinaryPrecedence());

    auto BB = BasicBlock::Create(getContext(), "entry", F);
    getBuilder()->SetInsertPoint(BB);

    // Create a subprogram DIE for this function.
    DIFile *Unit = DBuilder->createFile(PlayDbgInfo.TheCU->getFilename(),
                                        PlayDbgInfo.TheCU->getDirectory());
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
    PlayDbgInfo.LexicalBlocks.push_back(SP);

    // Unset the location for the prologue emission (leading instructions with no
    // location in a function are considered part of the prologue and the debugger
    // will run past them when breaking on a function)
    PlayDbgInfo.emitLocation(nullptr);

    unsigned ArgIdx = 0;
    for (auto &Arg : F->args()) {
        auto ArgTy = Arg.getType();
//        Body->getScope()->setVal(Arg.getName(), &Arg);
        auto Alloca = Parser::CreateEntryBlockAlloca(F, ArgTy, Arg.getName());

        // Create a debug descriptor for the variable.
        DILocalVariable *D = DBuilder->createParameterVariable(
            SP, Arg.getName(), ++ArgIdx, Unit, LineNo, PlayDbgInfo.getDoubleTy(),
            true);

        DBuilder->insertDeclare(Alloca, D, DBuilder->createExpression(),
                                DebugLoc::get(LineNo, 0, SP),
                                getBuilder()->GetInsertBlock());

        // arg type
        getBuilder()->CreateStore(&Arg, Alloca);
//        auto ArgLocal = getBuilder()->CreateLoad(Alloca);
        Body->getScope()->setVal(Arg.getName(), Alloca);
    }

    PlayDbgInfo.emitLocation(Body.get());

    Body->codegen();

    if (F->getReturnType()->isVoidTy()) {
        getBuilder()->CreateRetVoid();
    }

    PlayDbgInfo.LexicalBlocks.pop_back();

    verifyFunction(*F);

    return F;
}

StructType * ClassDeclAST::codegen() {
    vector<Type *> Tys;
    for (auto E = Members.begin(); E != Members.end(); E ++) {
        Tys.push_back((*E)->VType.getType(getContext()));
    }
    auto ST = StructType::create(getContext(), Tys, string("class") + "." + Name, false);
    scope->setClassType(Name, ST);

    for (auto E = Methods.begin(); E != Methods.end(); E ++)
        (*E)->codegen();

    return ST;
}
