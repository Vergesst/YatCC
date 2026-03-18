#include "EmitIR.hpp"
#include "Obj.hpp"
#include "asg.hpp"
#include <cstdlib>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <vector>

#define self (*this)

using namespace asg;

EmitIR::EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid)
  : mMgr(mgr)
  , mMod(mid, ctx)
  , mCtx(ctx)
  , mIntTy(llvm::Type::getInt32Ty(ctx))
  , mCurIrb(std::make_unique<llvm::IRBuilder<>>(ctx))
  , mCtorTy(llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), false))
{
}

llvm::Module &EmitIR::operator()(asg::TranslationUnit *tu) {
  for (auto&& i : tu->decls)
    self(i);
  return mMod;
}

//==============================================================================
// 类型
//==============================================================================

llvm::Type *EmitIR::operator()(const Type *type) {
  if (type->texp == nullptr) {
    switch (type->spec) {
      case Type::Spec::kInt:
        return llvm::Type::getInt32Ty(mCtx);
        // TODO: 在此添加对更多基础类型的处理
      case Type::Spec::kVoid:
        return llvm::Type::getVoidTy(mCtx);
      default:
        ABORT();
    }
  }

  Type subt;
  subt.spec = type->spec;
  subt.qual = type->qual;
  subt.texp = type->texp->sub;

  // TODO: 在此添加对指针类型、数组类型和函数类型的处理

  if (auto p = type->texp->dcst<FunctionType>()) {
    std::vector<llvm::Type*> pty;
    // TODO: 在此添加对函数参数类型的处理

    pty.reserve(p->params.size());
    for (const auto &param_type : p->params) {
      pty.push_back(self(param_type));
    }

    return llvm::FunctionType::get(self(&subt), std::move(pty), false);
  }
  if (auto p = type->texp->dcst<ArrayType>()) {
    return llvm::ArrayType::get(self(&subt), p->len);
  }
  if (auto p = type->dcst<PointerType>()) {
    return llvm::PointerType::get(self(&subt), 0);
  }

  ABORT();
}

//==============================================================================
// 表达式
//==============================================================================

llvm::Value *EmitIR::operator()(Expr *obj) {
  /// fixtures available: modern cpp
  // TODO: 在此添加对更多表达式处理的跳转
  if (auto p = obj->dcst<IntegerLiteral>())
    return self(p);

  if (auto p = obj->dcst<BinaryExpr>())
    return self(p);

  if (auto p = obj->dcst<UnaryExpr>())
    return self(p);

  if (auto p = obj->dcst<ParenExpr>())
    return self(p);

  if (auto p = obj->dcst<ImplicitInitExpr>())
    return self(p);

  if (auto p = obj->dcst<DeclRefExpr>())
    return self(p);

  if (auto p = obj->dcst<CallExpr>())
    return self(p);
  ABORT();
}

llvm::Constant *EmitIR::operator()(IntegerLiteral *obj) {
  return llvm::ConstantInt::get(self(obj->type), obj->val);
}

// TODO: 在此添加对更多表达式类型的处理
/**
  BinaryExpr

  UnaryExpr

  ParenExpr

  ImplicitCastExpr

  DeclRefExpr

  CallExpr
*/
llvm::Value *EmitIR::operator()(BinaryExpr *obj) {
  llvm::Value *lftVal{};
  llvm::Value *rhtVal{};
  auto &irb = *mCurIrb;
  auto op = obj->op;

  if (op != asg::BinaryExpr::kAnd && op != asg::BinaryExpr::kOr) {
    lftVal = self(obj->lft);
    rhtVal = self(obj->rht);

    switch (op) {
    case asg::BinaryExpr::kAdd:
      return irb.CreateAdd(lftVal, rhtVal);

    case asg::BinaryExpr::kSub:
      return irb.CreateSub(lftVal, rhtVal);

    case asg::BinaryExpr::kMul:
      return irb.CreateMul(lftVal, rhtVal);

    case asg::BinaryExpr::kDiv:
      return irb.CreateFDiv(lftVal, rhtVal);

    case asg::BinaryExpr::kMod:
      return irb.CreateSRem(lftVal, rhtVal);

    case asg::BinaryExpr::kGt:
      return irb.CreateICmpSGT(lftVal, rhtVal);

    case BinaryExpr::kLt:
      return irb.CreateICmpSLT(lftVal, rhtVal);

    case BinaryExpr::kGe:
      return irb.CreateICmpSGE(lftVal, rhtVal);

    case BinaryExpr::kLe:
      return irb.CreateICmpSLE(lftVal, rhtVal);

    case BinaryExpr::kEq:
      return irb.CreateICmpEQ(lftVal, rhtVal);

    case BinaryExpr::kNe:
      return irb.CreateICmpNE(lftVal, rhtVal);

    case BinaryExpr::kAssign:
      return irb.CreateStore(rhtVal, lftVal);

    case BinaryExpr::kIndex:
      return irb.CreateInBoundsGEP(self(obj->type), lftVal,
                                   std::vector<llvm::Value *>{rhtVal});

    default:
      ABORT();
    }
  } else {
    switch (op) {
    case asg::BinaryExpr::kAnd: {
      llvm::BasicBlock *blockBeforeRhs{};
      llvm::BasicBlock *blockBeforeEnd{};

      auto land_rhs = llvm::BasicBlock::Create(mCtx, "land_rhs", mCurFunc);
      auto land_end = llvm::BasicBlock::Create(mCtx, "land_end", mCurFunc);

      // # define self (*this) --- process lft expression
      lftVal = self(obj->lft);
      auto exp1_val = cast_to_boolean(lftVal);

      // itb.GetInsertBlock() --- get current position of parser
      // and this is where the lft expr ends, which is the beginning of rht expr
      blockBeforeRhs = irb.GetInsertBlock();
      land_rhs->moveAfter(blockBeforeRhs);

      // CreateCallBr(value, target_1, target_2) ---
      // if value == true => target else => target_2
      // so if lft_val(exp1_val) is true, it will jump to the right to vertify
      // whether rhs is true
      // and by contrary, this block will end (jump to the end)
      irb.CreateCondBr(exp1_val, land_rhs, land_end);

      irb.SetInsertPoint(land_rhs);

      // the same as right hand expression
      rhtVal = self(obj->rht);
      auto exp2_val = cast_to_boolean(rhtVal);

      blockBeforeEnd = irb.GetInsertBlock();
      land_end->moveAfter(blockBeforeEnd);

      irb.SetInsertPoint(land_end);

      // phi usage
      auto phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2);
      // lft value --- if false
      phi->addIncoming(
          // create a llvm_based false
          llvm::ConstantInt::get(llvm::Type::getInt1Ty(mCtx), false),
          blockBeforeRhs);
      // rht value
      phi->addIncoming(exp2_val, blockBeforeEnd);
    }

    case asg::BinaryExpr::kOr: {
      llvm::BasicBlock *blockBeforeRhs{};
      llvm::BasicBlock *blockBeforeEnd{};

      auto lor_rhs = llvm::BasicBlock::Create(mCtx, "lor_rhs", mCurFunc);
      auto lor_end = llvm::BasicBlock::Create(mCtx, "lor_lft", mCurFunc);

      lftVal = self(obj->lft);
      auto exp1_val = cast_to_boolean(lftVal);

      blockBeforeRhs = irb.GetInsertBlock();
      lor_rhs->moveAfter(blockBeforeRhs);

      // short process --- true exit and false continue
      irb.CreateCondBr(exp1_val, lor_end, lor_rhs);

      rhtVal = self(obj->rht);
      auto exp2_val = cast_to_boolean(rhtVal);

      // set insert point to force code generate into lor_end
      irb.SetInsertPoint(lor_end);
      lor_end->moveAfter(blockBeforeEnd);

      auto phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2);
      phi->addIncoming(
          llvm::ConstantInt::get(llvm::Type::getInt1Ty(mCtx), true),
          blockBeforeRhs);
      phi->addIncoming(exp2_val, blockBeforeEnd);
    }

    default:
      ABORT();
    }
  }
}

llvm::Value *EmitIR::operator()(UnaryExpr *obj) {
  auto &irb = *mCurIrb;
  auto op = obj->op;
  auto val = self(obj->sub);

  switch (op) {
  // case UnaryExpr::kINVALID:
  case UnaryExpr::kPos:
    return val;
  case UnaryExpr::kNot:
    return irb.CreateNot(cast_to_boolean(val));
  case UnaryExpr::kNeg:
    return irb.CreateNeg(val);
  default:
    ABORT();
  }
}

llvm::Value *EmitIR::operator()(ParenExpr *obj) { return self(obj->sub); }

// ?hyw
llvm::Value *EmitIR::operator()(ImplicitCastExpr *obj) {
  auto sub = self(obj->sub);
  auto &irb = *mCurIrb;

  switch (obj->kind) {
  case ImplicitCastExpr::kLValueToRValue: {
    auto ty = self(obj->sub->type);
    auto loadVal = irb.CreateLoad(ty, sub);
    return loadVal;
  }
  case ImplicitCastExpr::kArrayToPointerDecay: {
    auto ty = self(obj->sub->type);
    auto elemTy = ty->getArrayElementType();
    auto ptrTy = llvm::PointerType::get(elemTy, 0);
    auto decayPtr = irb.CreateBitCast(sub, ptrTy);

    return decayPtr;
  }
  case ImplicitCastExpr::kFunctionToPointerDecay: {
    auto funcTy = self(obj->sub->type);
    auto funcPtrTy = llvm::PointerType::get(funcTy, 0);
    auto funcPtr = irb.CreateBitCast(sub, funcPtrTy);

    return funcPtr;
  }
  default:
    ABORT();
  }
}

// ?hyw
llvm::Value *EmitIR::operator()(DeclRefExpr *obj) {
  return reinterpret_cast<llvm::Value *>(obj->decl->any);
}

llvm::Value *EmitIR::operator()(CallExpr *obj) {
  auto function{llvm::dyn_cast<llvm::Function>(self(obj->head))};
  auto &irb = *mCurIrb;
  auto args = obj->args;

  if (!function)
    ABORT();

  llvm::Function *func =
      mCurFunc->getParent()->getFunction(function->getName());

  std::vector<llvm::Value *> func_params;
  func_params.reserve(args.size());
  for (const auto &arg : args) {
    func_params.push_back(self(arg));
  }

  return irb.CreateCall(func, func_params);
}

//==============================================================================
// 语句
// decl stmt
// expr stmt
// if stmt
// while stmt
// break stmt
// continue stmt
// null stmt --- ?
//==============================================================================

void EmitIR::operator()(Stmt *obj) {
  // TODO: 在此添加对更多Stmt类型的处理的跳转

  if (auto p = obj->dcst<CompoundStmt>())
    return self(p);

  if (auto p = obj->dcst<ReturnStmt>())
    return self(p);

  if (auto p = obj->dcst<DeclStmt>())
    return self(p);

  if (auto p = obj->dcst<ExprStmt>())
    return self(p);

  if (auto p = obj->dcst<IfStmt>())
    return self(p);

  if (auto p = obj->dcst<WhileStmt>())
    return self(p);

  if (auto p = obj->dcst<BreakStmt>())
    return self(p);

  if (auto p = obj->dcst<BreakStmt>())
    return self(p);

  if (auto p = obj->dcst<NullStmt>())
    return self(p);

  ABORT();
}

// TODO: 在此添加对更多Stmt类型的处理

void EmitIR::operator()(CompoundStmt *obj) {
  // TODO: 可以在此添加对符号重名的处理
  for (auto&& stmt : obj->subs)
    self(stmt);
}

void EmitIR::operator()(ReturnStmt *obj) {
  auto& irb = *mCurIrb;

  llvm::Value* retVal;
  if (!obj->expr)
    retVal = nullptr;
  else
    retVal = self(obj->expr);

  mCurIrb->CreateRet(retVal);

  auto exitBb = llvm::BasicBlock::Create(mCtx, "return_exit", mCurFunc);
  mCurIrb->SetInsertPoint(exitBb);
}

void EmitIR::operator()(DeclStmt *obj) {
  for (auto &decl : obj->decls)
    // works assigned to VarDecl and other Decl expressions
    self(decl);
}

void EmitIR::operator()(ExprStmt *obj) {
  // works assigned to expressions
  self(obj->expr);
}
// =============================================
// guess cond in if is false
// so store then stmt
// and process else stmt first
// after that, process cond and create a jump node
//
// NEED OPT
// =============================================
void EmitIR::operator()(IfStmt *obj) {
  auto &irb = *mCurIrb;
  auto blockBeforeThen{mCurIrb->GetInsertBlock()};
  auto thenBlock{llvm::BasicBlock::Create(mCtx, "then_block", mCurFunc)};
  llvm::BasicBlock *blockAfterThen;
  llvm::BasicBlock *blockAfterElse;

  irb.SetInsertPoint(thenBlock);
  self(obj->then);

  blockAfterThen = irb.GetInsertBlock();

  // create else block
  auto elseBlock{obj->else_ ? llvm::BasicBlock::Create(mCtx, "else_block")
                            : nullptr};

  if (elseBlock != nullptr) {
    irb.SetInsertPoint(elseBlock);
    self(obj->else_);
    blockAfterElse = irb.GetInsertBlock();
  }

  auto endBlock{llvm::BasicBlock::Create(mCtx, "end_block", mCurFunc)};
  irb.SetInsertPoint(blockBeforeThen);

  // process  cond
  auto cond{cast_to_boolean(self(obj->cond))};

  irb.CreateCondBr(cond, thenBlock,
                   (elseBlock == nullptr ? endBlock : elseBlock));

  irb.SetInsertPoint(blockAfterThen);

  // check whether break exists
  if (!irb.GetInsertBlock()->getTerminator())
    irb.CreateBr(endBlock);

  if (blockAfterElse) {
    irb.SetInsertPoint(blockAfterElse);
    if (!irb.GetInsertBlock()->getTerminator())
      irb.CreateBr(endBlock);
  }

  // for coming stmt process
  irb.SetInsertPoint(endBlock);
}

// NOTE available --- whether mCurFunc is available to handle block order
void EmitIR::operator()(WhileStmt *obj) {
  auto &irb = *mCurIrb;
  llvm::BasicBlock *blockAfterBody{};
  auto bodyBlock{llvm::BasicBlock::Create(mCtx, "body_block", mCurFunc)};
  auto condBlock{llvm::BasicBlock::Create(mCtx, "cond_block", mCurFunc)};
  auto endBlock{llvm::BasicBlock::Create(mCtx, "end_block", mCurFunc)};

  obj->any = endBlock;
  obj->cond->any = condBlock;

  irb.CreateBr(condBlock);

  // process cond block
  irb.SetInsertPoint(condBlock);
  auto cond_val = cast_to_boolean(self(obj->cond));
  irb.CreateCondBr(cond_val, bodyBlock, endBlock);

  irb.SetInsertPoint(bodyBlock);
  self(obj->body);

  blockAfterBody = irb.GetInsertBlock();

  // no jump keywords like break and continue
  //! break and continue will find the nearest loop
  if (!irb.GetInsertBlock()->getTerminator())
    irb.CreateBr(condBlock);

  endBlock->moveAfter(blockAfterBody);
  irb.SetInsertPoint(endBlock);
}

void EmitIR::operator()(BreakStmt *obj) {
  auto &irb{*mCurIrb};
  if (auto p{obj->loop->dcst<WhileStmt>()})
    // p->any --- endBlock
    irb.CreateBr(reinterpret_cast<llvm::BasicBlock *>(p->any));
  else
    ABORT();
}

void EmitIR::operator()(ContinueStmt *obj) {
  auto &irb{*mCurIrb};
  if (auto p{obj->loop->dcst<WhileStmt>()})
    irb.CreateBr(reinterpret_cast<llvm::BasicBlock *>(p->cond->any));
  else
    ABORT();
}

void EmitIR::operator()(NullStmt *obj) {}

//==============================================================================
// 声明
//==============================================================================

void EmitIR::operator()(Decl *obj) {
  // TODO: 添加变量声明处理的跳转
  if (auto p{obj->dcst<VarDecl>()})
    return self(p);

  if (auto p = obj->dcst<FunctionDecl>())
    return self(p);

  ABORT();
}

llvm::Constant *EmitIR::array_constant(llvm::ArrayType *array_type,
                                       asg::InitListExpr *init,
                                       LocationAndValue &location_and_value,
                                       std::vector<llvm::Value *> path) {
  auto &irb{*mCurIrb};

  if (!array_type)
    ABORT();

  if (path.empty())
    path.push_back(irb.getInt32(0));

  std::vector<llvm::Constant *> init_vec;
  for (size_t i{}; i < array_type->getNumElements(); i++) {
    auto elem_type = array_type->getElementType();
    if (elem_type->isArrayTy()) {
      if (init && i < init->list.size() &&
          init->list[i]->dcst<InitListExpr>()) {
        path.push_back(irb.getInt32(i));
        init_vec.push_back(array_constant(
            llvm::dyn_cast<llvm::ArrayType>(elem_type),
            init->list[i]->dcst<InitListExpr>(), location_and_value, path));
        path.pop_back();
      } else {
        init_vec.push_back(llvm::Constant::getNullValue(elem_type));
      }
    } else if (elem_type->isIntegerTy()) {
      // normal i32 literal
      if (init && i < init->list.size() &&
          init->list[i]->dcst<IntegerLiteral>()) {
        if (init->list[i]) {
          init_vec.push_back(llvm::ConstantInt::get(
              elem_type, init->list[i]->dcst<IntegerLiteral>()->val));
        }
        // process cast expr --- ? what should it be like
      } else if (init && i < init->list.size() &&
                 init->list[i]->dcst<ImplicitCastExpr>()) {
        auto load_value{llvm::dyn_cast<llvm::LoadInst>(self(init->list[i]))};
        path.push_back(irb.getInt32(i));
        location_and_value.emplace_back(path, load_value);
        path.pop_back();
        init_vec.push_back(llvm::Constant::getNullValue(elem_type));
        // process binary expr elements
      } else if (init && i < init->list.size() &&
                 init->list[i]->dcst<BinaryExpr>()) {
        // process current element
        auto load_value{self(init->list[i])};

        path.push_back(irb.getInt32(i));
        location_and_value.emplace_back(path, load_value);
        path.pop_back();
        init_vec.push_back(llvm::Constant::getNullValue(elem_type));
      } else {
        init_vec.push_back(llvm::Constant::getNullValue(elem_type));
      }
    } else {
      ABORT();
    }
  }

  return llvm::ConstantArray::get(array_type, init_vec);
}

void EmitIR::trans_init(llvm::Value *val, asg::Expr *obj, llvm::Type *ty) {
  auto &irb{*mCurIrb};

  if (auto p = obj->dcst<IntegerLiteral>()) {
    auto init_val = llvm::ConstantInt::get(self(p->type), p->val);
    irb.CreateStore(init_val, val);
    return;
  }

  if (auto p = obj->dcst<InitListExpr>()) {
    LocationAndValue location_and_value;
    auto cons_arr{array_constant(llvm::dyn_cast<llvm::ArrayType>(ty), p,
                                 location_and_value)};
    irb.CreateStore(cons_arr, val);

    for (const auto &[location, value] : location_and_value) {
      auto elem = irb.CreateInBoundsGEP(llvm::dyn_cast<llvm::ArrayType>(ty),
                                        val, location);
      irb.CreateStore(value, elem);
    }
    return;
  }

  if (auto p = obj->dcst<ImplicitCastExpr>()) {
    auto init{self(p)};
    irb.CreateStore(init, val);

    return;
  }

  if (auto p = obj->dcst<CallExpr>()) {
    auto init{self(p)};
    irb.CreateStore(init, val);

    return;
  }

  if (auto p = obj->dcst<BinaryExpr>()) {
    auto init{self(p)};
    irb.CreateStore(init, val);

    return;
  }

  if (auto p = obj->dcst<UnaryExpr>()) {
    auto init{self(p)};
    irb.CreateStore(init, val);

    return;
  }

  if (auto p = obj->dcst<ParenExpr>()) {
    auto init{self(p)};
    irb.CreateStore(init, val);

    return;
  }
}

// TODO: 添加变量声明的处理

void EmitIR::operator()(VarDecl *obj) {
  auto &irb{*mCurIrb};
  auto ty = self(obj->type);
  // global variable
  if (!mCurFunc) {
    auto global_var = new llvm::GlobalVariable(
        mMod, ty, false, llvm::GlobalVariable::ExternalLinkage, nullptr,
        obj->name);

    obj->any = global_var;
    // assign variable empty
    global_var->setInitializer(llvm::Constant::getNullValue(ty));

    if (obj->init == nullptr)
      return;

    save_state();

    mCurFunc =
        llvm::Function::Create(mCtorTy, llvm::GlobalVariable::PrivateLinkage,
                               "ctor_" + obj->name, mMod);
    llvm::appendToGlobalCtors(mMod, mCurFunc, 65535);

    auto entry_block = llvm::BasicBlock::Create(mCtx, "entry", mCurFunc);
    irb.SetInsertPoint(entry_block);
    trans_init(global_var, obj->init, ty);
    irb.CreateRet(nullptr);

    restore_state();
  } else {
    save_state();

    auto entry_block = llvm::BasicBlock::Create(mCtx, "entry", mCurFunc);

    // determines whether there is terminator in entry_block
    if (!entry_block->getTerminator())
      // if there is no branch command, append init_stmt to entry_block
      irb.SetInsertPoint(entry_block);
    else
      // if there is branch, append it before terminator
      // terminator here refers to <br> --- command jump to other blocks
      // and ret stmt
      irb.SetInsertPoint(entry_block->getTerminator());

    auto local_var = irb.CreateAlloca(ty, nullptr, obj->name);
    obj->any = local_var;

    restore_state();

    if (obj->init == nullptr)
      return;
    // else init this variable with trans_init function
    trans_init(local_var, obj->init, ty);
  }
}

void EmitIR::operator()(FunctionDecl *obj) {
  // 创建函数
  auto fty = llvm::dyn_cast<llvm::FunctionType>(self(obj->type));
  auto func = llvm::Function::Create(fty, llvm::GlobalVariable::ExternalLinkage,
                                     obj->name, mMod);

  obj->any = func;

  if (obj->body == nullptr)
    return;
  auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", func);
  auto &entryIrb = *mCurIrb;

  // 翻译函数体
  mCurFunc = func;
  // TODO: 添加对函数参数的处理
  auto &irb{*mCurIrb};

  int i = 0;
  for (auto arg_iter{func->arg_begin()}; arg_iter != func->arg_end();
       ++arg_iter) {
    arg_iter->setName(obj->params[i]->name);
    auto local_var{irb.CreateAlloca(self(obj->params[i]->type), nullptr,
                                    obj->params[i]->name + ".addr")};
    // store variable to certain location (local_var)
    irb.CreateStore(&(*arg_iter), local_var);
    // create binding from pointer to variable
    obj->params[i]->any = local_var;
    i++;
  }

  self(obj->body);
  auto &exitIrb = *mCurIrb;

  if (fty->getReturnType()->isVoidTy())
    exitIrb.CreateRetVoid();
  else
    exitIrb.CreateUnreachable();

  // ?
  mCurFunc = nullptr;
}

// copied
void EmitIR::save_state() {
  mPreviousFunc = mCurFunc;
  mPreviousBasicBlock = mCurIrb->GetInsertBlock();
}

void EmitIR::restore_state() {
  if (!mPreviousFunc || !mPreviousBasicBlock) {
    ABORT();
  } else {
    mCurFunc = mPreviousFunc.value();
    mCurIrb->SetInsertPoint(mPreviousBasicBlock.value());
  }
}

llvm::Value *EmitIR::cast_to_boolean(llvm::Value *val) {
  auto &irb{*mCurIrb};
  llvm::Value *cmp_result{};

  if (val->getType()->isIntegerTy(32)) {
    cmp_result = irb.CreateICmpNE(val, irb.getInt32(0));
  } else if (val->getType()->isIntegerTy(64)) {
    cmp_result = irb.CreateICmpNE(val, irb.getInt64(0));
  } else if (val->getType()->isIntegerTy(1)) {
    cmp_result = val;
  }

  return cmp_result;
}
