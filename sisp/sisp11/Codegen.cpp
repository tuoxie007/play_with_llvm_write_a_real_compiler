//
//  Codegen.cpp
//  sisp
//
//  Created by 徐可 on 2020/2/19.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#include "Codegen.hpp"

DebugInfo SispDbgInfo;

static IRBuilder<> Builder(TheContext);
std::unique_ptr<DIBuilder> DBuilder;
static vector<DIScope *> LexicalBlocks;

DIType *DebugInfo::getDoubleTy() {
    if (DblTy)
        return DblTy;

    DblTy = DBuilder->createBasicType("double", 64, dwarf::DW_ATE_float);
    return DblTy;
}

void DebugInfo::emitLocation(ExprAST *AST) {
    if (!AST)
        return Builder.SetCurrentDebugLocation(DebugLoc());

    DIScope *Scope;
    if (LexicalBlocks.empty())
        Scope = TheCU;
    else
        Scope = LexicalBlocks.back();

    Builder.SetCurrentDebugLocation(DebugLoc::get(AST->getLine(), AST->getCol(), Scope));
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

static AllocaInst *CreateEntryBlockAlloca(Function *F,
                                          const string &VarName) {
    IRBuilder<> TmpBlock(&F->getEntryBlock(),
                         F->getEntryBlock().begin());
    return TmpBlock.CreateAlloca(Type::getDoubleTy(TheContext), 0, VarName.c_str());
}

static Function *getFunction(std::string Name) {
    // First, see if the function has already been added to the current module.
    if (auto *F = TheModule->getFunction(Name))
        return F;

    // If not, check whether we can codegen the declaration from some existing
    // prototype.
    auto FI = FunctionProtos.find(Name);
    if (FI != FunctionProtos.end())
        return FI->second->codegen();

    // If no existing prototype exists, return null.
    return nullptr;
}

const std::string& FunctionAST::getName() const {
    return Proto->getName();
}

Value *NumberExprAST::codegen() {
    SispDbgInfo.emitLocation(this);
    return ConstantFP::get(TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen() {
    Value *V = scope->getVal(Name);
    if (!V)
        LogError("Unkown variable name");

    SispDbgInfo.emitLocation(this);
    return Builder.CreateLoad(V, Name.c_str());
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

        Builder.CreateStore(Val, Variable);
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
            return Builder.CreateFAdd(L, R, "addtmp");
        case tok_sub:
            return Builder.CreateFSub(L, R, "subtmp");
        case tok_mul:
            return Builder.CreateFMul(L, R, "multmp");
        case tok_less:
            L = Builder.CreateFCmpULT(L, R, "cmptmp");
            return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext));
        default:
        {
            auto F = getFunction(string("binary") + Op);
            assert(F && "binary operator not found!");
            auto Ops = { L, R };
            return Builder.CreateCall(F, Ops, "calltmp");
        }
    }
}

Value *IfExprAST::codegen() {
    SispDbgInfo.emitLocation(this);
    auto CondV = Cond->codegen();
    if (!CondV)
        return nullptr;

    CondV = Builder.CreateFCmpONE(CondV, ConstantFP::get(TheContext, APFloat(0.0)), "ifcond");

    auto F = Builder.GetInsertBlock()->getParent();

    auto ThenBlock = BasicBlock::Create(TheContext, "then", F);
    auto ElseBlock = BasicBlock::Create(TheContext, "else");
    auto MergeBlock = BasicBlock::Create(TheContext, "ifcont");

    Builder.CreateCondBr(CondV, ThenBlock, ElseBlock);

    Builder.SetInsertPoint(ThenBlock);

    auto ThenV = Then->codegen();
    if (!ThenV)
        return nullptr;

    Builder.CreateBr(MergeBlock);

    ThenBlock = Builder.GetInsertBlock();

    F->getBasicBlockList().push_back(ElseBlock);
    Builder.SetInsertPoint(ElseBlock);

    auto ElseV = Else->codegen();
    if (!ElseV)
        return nullptr;

    Builder.CreateBr(MergeBlock);

    ElseBlock = Builder.GetInsertBlock();

    F->getBasicBlockList().push_back(MergeBlock);
    Builder.SetInsertPoint(MergeBlock);

    auto PN = Builder.CreatePHI(Type::getDoubleTy(TheContext), 2, "iftmp");

    PN->addIncoming(ThenV, ThenBlock);
    PN->addIncoming(ElseV, ElseBlock);

    return PN;
}

Value *ForExprAST::codegen() {
    auto StartValue = Start->codegen();
    if (!StartValue)
        return nullptr;

    auto F = Builder.GetInsertBlock()->getParent();

    auto Alloca = CreateEntryBlockAlloca(F, VarName);

    SispDbgInfo.emitLocation(this);

    auto StartVal = Start->codegen();
    if (!Start)
        return nullptr;

    Builder.CreateStore(StartVal, Alloca);

    auto LoopBlock = BasicBlock::Create(TheContext, "loop", F);
    Builder.CreateBr(LoopBlock);
    Builder.SetInsertPoint(LoopBlock);

    getScope()->setVal(VarName, Alloca);

    if (!Body->codegen())
        return nullptr;

    Value *StepVal = nullptr;
    if (Step) {
        StepVal = Step->codegen();
        if (!StepVal)
            return nullptr;
    } else {
        StepVal = ConstantFP::get(TheContext, APFloat(1.0));
    }

    auto EndCond = End->codegen();
    if (!EndCond)
        return nullptr;

    auto CurVar = Builder.CreateLoad(Alloca, VarName.c_str());
    auto NextVar = Builder.CreateFAdd(CurVar, StepVal, "nextvar");
    Builder.CreateStore(NextVar, Alloca);

    EndCond = Builder.CreateFCmpONE(EndCond, ConstantFP::get(TheContext, APFloat(0.0)), "loopcond");

    auto AfterBlock = BasicBlock::Create(TheContext, "afterloop", F);
    Builder.CreateCondBr(EndCond, LoopBlock, AfterBlock);
    Builder.SetInsertPoint(AfterBlock);

    return Constant::getNullValue(Type::getDoubleTy(TheContext));
}

Value *VarExprAST::codegen() {
//    vector<AllocaInst *> OldBindings;

    auto F = Builder.GetInsertBlock()->getParent();

    for (unsigned long i = 0, e = VarNames.size(); i != e; i++) {
        string &VarName = VarNames[i].first;
        auto Init = VarNames[i].second.get();
        Value *InitVal;
        if (Init) {
            InitVal = Init->codegen();
            if (!InitVal)
                return nullptr;
        } else {
            InitVal = ConstantFP::get(TheContext, APFloat(0.0));
        }

        auto Alloca = CreateEntryBlockAlloca(F, VarName);
        Builder.CreateStore(InitVal, Alloca);

        scope->setVal(VarName, Alloca);
    }

    SispDbgInfo.emitLocation(this);

    return Constant::getNullValue(Type::getDoubleTy(TheContext));
}

Value *UnaryExprAST::codegen() {
    auto OperandV = Operand->codegen();
    if (!OperandV)
        return nullptr;

    auto F = getFunction(string("unary") + Opcode);
    if (!F)
        return LogErrorV("Unkown unary operator");

    SispDbgInfo.emitLocation(this);
    return Builder.CreateCall(F, OperandV, "unop");
}

Value *CompoundExprAST::codegen() {
    int i = 0;
    for (auto Expr = Exprs.begin(); Expr != Exprs.end(); Expr ++) {
        cout << "subExpr " << to_string(i++) << endl;
        if (!(*Expr)->codegen())
            return nullptr;
    }
    return Constant::getNullValue(Type::getDoubleTy(TheContext));
}

Value *CallExprAST::codegen() {
    SispDbgInfo.emitLocation(this);
    // Look up the name in the global module table.
    Function *CalleeF = getFunction(Callee);
    if (!CalleeF)
        return LogErrorV((string("Unknown function referenced ") + Callee).c_str());

    // If argument mismatch error.
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");

    std::vector<Value *> ArgsV;
    for (unsigned long i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
        return nullptr;
    }

    return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen() {
    vector<Type *> Doubles(Args.size(), Type::getDoubleTy(TheContext));
    FunctionType *FT = FunctionType::get(Type::getDoubleTy(TheContext), Doubles, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());
    unsigned long Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(Args[Idx++]);
    return F;
}

Function *FunctionAST::codegen() {
    // Transfer ownership of the prototype to the FunctionProtos map, but keep a
    // reference to it for use below.
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    Function *F = getFunction(P.getName());
    if (!F)
        return nullptr;

    if (P.isBinaryOp())
        BinOpPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();

    // Create a new basic block to start insertion into.
    auto BB = BasicBlock::Create(TheContext, "entry", F);
    Builder.SetInsertPoint(BB);

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
        auto Alloca = CreateEntryBlockAlloca(F, Arg.getName());

        // Create a debug descriptor for the variable.
        DILocalVariable *D = DBuilder->createParameterVariable(
            SP, Arg.getName(), ++ArgIdx, Unit, LineNo, SispDbgInfo.getDoubleTy(),
            true);

        DBuilder->insertDeclare(Alloca, D, DBuilder->createExpression(),
                                DebugLoc::get(LineNo, 0, SP),
                                Builder.GetInsertBlock());

        Builder.CreateStore(&Arg, Alloca);
        Body->getScope()->setVal(Arg.getName(), Alloca);
    }

    SispDbgInfo.emitLocation(Body.get());

    if (Value *RetVal = Body->codegen()) {
        // Finish off the function.
        Builder.CreateRet(RetVal);

        SispDbgInfo.LexicalBlocks.pop_back();

        // Validate the generated code, checking for consistency.
        verifyFunction(*F);

        if (JITEnabled) {
            // Run the optimizer on the function.
            TheFPM->run(*F);
        }

        return F;
    }

    // Error reading body, remove function.
    F->eraseFromParent();

    if (P.isBinaryOp())
        BinOpPrecedence.erase(Proto->getOperatorName());

    SispDbgInfo.LexicalBlocks.pop_back();

    return nullptr;
}
