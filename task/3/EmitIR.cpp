#include "EmitIR.hpp"
#include "Obj.hpp"
#include "asg.hpp"
#include <cstdlib>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/TypeSize.h>
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
  if (auto p = type->texp->dcst<PointerType>()) {
    return llvm::PointerType::get(self(&subt), 0);
  }

  ABORT();
}

//==============================================================================
// 表达式
//==============================================================================

llvm::Value *EmitIR::operator()(Expr *obj) {
  // TODO: 在此添加对更多表达式处理的跳转
  if (auto p = obj->dcst<IntegerLiteral>())
    return self(p);

  if (auto p = obj->dcst<DeclRefExpr>())
    return self(p);

  if (auto p = obj->dcst<ImplicitCastExpr>())
    return self(p);

  if (auto p = obj->dcst<BinaryExpr>())
    return self(p);

  if (auto p = obj->dcst<UnaryExpr>())
    return self(p);

  if (auto p = obj->dcst<ParenExpr>())
    return self(p);

  if (auto p = obj->dcst<CallExpr>())
    return self(p);

  ABORT();
}

llvm::Constant *EmitIR::operator()(IntegerLiteral *obj) {
  return llvm::ConstantInt::get(self(obj->type), obj->val);
}

// TODO: 在此添加对更多表达式类型的处理
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
    // 假设 sub 是指向数组类型的值
    auto arrayTy = self(obj->sub->type);
    // 获取数组元素的类型
    auto elemTy = arrayTy->getArrayElementType();
    // 创建一个指向数组首元素的指针类型
    auto ptrTy = llvm::PointerType::get(elemTy, 0);
    // 获取数组的地址，这里假设 sub 已经是一个指向数组的指针
    auto arrayPtr = sub;
    // 将数组地址转换为指向首元素的指针
    auto decayedPtr = irb.CreateBitCast(arrayPtr, ptrTy);
    return decayedPtr;
  }

  case ImplicitCastExpr::kFunctionToPointerDecay: {
    // 获取子表达式的类型，这里假设它是一个函数类型
    auto funcTy = self(obj->sub->type);
    // 创建一个指向该函数的指针类型
    auto funcPtrTy = llvm::PointerType::get(funcTy, 0);
    // 将函数值转换为函数指针
    auto funcPtr = irb.CreateBitCast(sub, funcPtrTy);
    return funcPtr;
  }

  default:
    ABORT();
  }
}

llvm::Value *EmitIR::operator()(DeclRefExpr *obj) {
  // 在LLVM IR层面，左值体现为返回指向值的指针
  // 在ImplicitCastExpr::kLValueToRValue中发射load指令从而变成右值
  return reinterpret_cast<llvm::Value *>(obj->decl->any);
}

llvm::Value *EmitIR::operator()(BinaryExpr *obj) {
  llvm::Value *lftVal{}, *rhtVal{};
  auto &irb = *mCurIrb;
  if (obj->op != BinaryExpr::kAnd && obj->op != BinaryExpr::kOr) {
    // 不需要短路求值的可以直接计算lftVal和rhtVal
    lftVal = self(obj->lft);
    rhtVal = self(obj->rht);
    switch (obj->op) {
    case BinaryExpr::kMul:
      return irb.CreateMul(lftVal, rhtVal);

    case BinaryExpr::kDiv:
      return irb.CreateSDiv(lftVal, rhtVal);

    case BinaryExpr::kMod:
      return irb.CreateSRem(lftVal, rhtVal);

    case BinaryExpr::kAdd:
      return irb.CreateAdd(lftVal, rhtVal);

    case BinaryExpr::kSub:
      return irb.CreateSub(lftVal, rhtVal);

    case BinaryExpr::kGt:
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
    // 需要短路求值的，计算左右值需要有先后顺序
    switch (obj->op) {
    case BinaryExpr::kAnd: {
      llvm::BasicBlock *blockBeforeRhs{}, *blockBeforeEnd{};

      // 创建两个新的基本块：land_rhs 和 land_end
      llvm::BasicBlock *land_rhs =
          llvm::BasicBlock::Create(mCtx, "land_rhs", mCurFunc);
      llvm::BasicBlock *land_end =
          llvm::BasicBlock::Create(mCtx, "land_end", mCurFunc);

      // 处理 exp_1 (lftVal)
      lftVal = self(obj->lft);
      llvm::Value *exp1_val = cast_to_boolean(lftVal);

      // 这里会创建新的语句块，而land_rhs应该在这些语句块之后，所以要进行移动
      blockBeforeRhs = irb.GetInsertBlock();
      land_rhs->moveAfter(blockBeforeRhs);

      // 在当前基本块末尾创建条件跳转指令
      irb.CreateCondBr(exp1_val, land_rhs, land_end);

      // 设置插入点到 land_rhs 基本块
      irb.SetInsertPoint(land_rhs);

      // 处理 exp_2 (rhtVal)
      rhtVal = self(obj->rht);
      llvm::Value *exp2_val = cast_to_boolean(rhtVal);

      // 在 land_rhs 基本块末尾创建无条件跳转指令
      irb.CreateBr(land_end);

      // 因为添加了新的语句块，而land_end应该在这些语句块之后
      blockBeforeEnd = irb.GetInsertBlock();
      land_end->moveAfter(blockBeforeRhs);

      // 设置插入点到 land_end 基本块
      irb.SetInsertPoint(land_end);

      // 使用 phi 指令来合并两个路径的值
      llvm::PHINode *phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2);
      phi->addIncoming(
          llvm::ConstantInt::get(llvm::Type::getInt1Ty(mCtx), false),
          blockBeforeRhs);
      phi->addIncoming(exp2_val, blockBeforeEnd);

      return phi;
    }

    case BinaryExpr::kOr: {
      llvm::BasicBlock *blockBeforeRhs{}, *blockBeforeEnd{};

      // 创建两个新的基本块：land_rhs 和 land_end
      llvm::BasicBlock *land_rhs =
          llvm::BasicBlock::Create(mCtx, "land_rhs", mCurFunc);
      llvm::BasicBlock *land_end =
          llvm::BasicBlock::Create(mCtx, "land_end", mCurFunc);

      // 处理 exp_1 (lftVal)
      lftVal = self(obj->lft);
      llvm::Value *exp1_val = cast_to_boolean(lftVal);

      // 这里会创建新的语句块，而land_rhs应该在这些语句块之后，所以要进行移动
      blockBeforeRhs = irb.GetInsertBlock();
      land_rhs->moveAfter(blockBeforeRhs);

      // 在当前基本块末尾创建条件跳转指令
      irb.CreateCondBr(exp1_val, land_end, land_rhs);

      // 设置插入点到 land_rhs 基本块
      irb.SetInsertPoint(land_rhs);

      // 处理 exp_2 (rhtVal)
      rhtVal = self(obj->rht);
      llvm::Value *exp2_val = cast_to_boolean(rhtVal);

      // 在 land_rhs 基本块末尾创建无条件跳转指令
      irb.CreateBr(land_end);

      // 因为添加了新的语句块，而land_end应该在这些语句块之后
      blockBeforeEnd = irb.GetInsertBlock();
      land_end->moveAfter(blockBeforeRhs);

      // 设置插入点到 land_end 基本块
      irb.SetInsertPoint(land_end);

      // 使用 phi 指令来合并两个路径的值
      llvm::PHINode *phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2);
      phi->addIncoming(
          llvm::ConstantInt::get(llvm::Type::getInt1Ty(mCtx), true),
          blockBeforeRhs);
      phi->addIncoming(exp2_val, blockBeforeEnd);

      return phi;
    }

    default:
      ABORT();
    }
  }
}

llvm::Value *EmitIR::operator()(UnaryExpr *obj) {
  auto &irb = *mCurIrb;
  auto *val = self(obj->sub);

  switch (obj->op) {
  case UnaryExpr::kNeg:
    return irb.CreateNeg(val);

  case UnaryExpr::kNot:
    return irb.CreateNot(cast_to_boolean(val));

  case UnaryExpr::kPos:
    return val;

  default:
    ABORT();
  }
}

llvm::Value *EmitIR::operator()(ParenExpr *obj) {
  auto *val = self(obj->sub);
  return val;
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

  if (auto p = obj->dcst<ContinueStmt>())
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
  for (auto &decl : obj->decls) {
    self(decl);
  }
}

void EmitIR::operator()(ExprStmt *obj) { self(obj->expr); }

//==============================================================================
// 1. process cond_block --- before then_block
// 2. process then_block
// 3. create branch from then_block to end_block --- if terminator exists
// 4. check whether else_block exists
// 5. create branch to else_block
// 6. create branch to end_block
//==============================================================================
void EmitIR::operator()(IfStmt *obj) {
  auto &irb{*mCurIrb};
  auto cond_block{irb.GetInsertBlock()};
  auto then_block{llvm::BasicBlock::Create(mCtx, "if.then", mCurFunc)};
  auto else_block{obj->else_
                      ? llvm::BasicBlock::Create(mCtx, "if.else", mCurFunc)
                      : nullptr};
  auto end_block{llvm::BasicBlock::Create(mCtx, "if.end", mCurFunc)};

  // process cond and create branch
  auto cond = self(obj->cond);
  auto cond_res = cast_to_boolean(cond);
  irb.CreateCondBr(cond_res, then_block,
                   else_block == nullptr ? end_block : else_block);

  irb.SetInsertPoint(then_block);
  self(obj->then);

  // choose irb.GetInsertBlock() --- get current block
  if (!irb.GetInsertBlock()->getTerminator())
    irb.CreateBr(end_block);

  // process else block
  if (else_block != nullptr) {
    irb.SetInsertPoint(else_block);
    self(obj->else_);
    if (!irb.GetInsertBlock()->getTerminator())
      irb.CreateBr(end_block);
  }

  // end processing and set insert point at end
  irb.SetInsertPoint(end_block);
}

// NOTE available --- mCurFunc is available to handle block order
void EmitIR::operator()(WhileStmt *obj) {
  auto &irb{*mCurIrb};
  auto entry_block{irb.GetInsertBlock()};
  auto cond_block{llvm::BasicBlock::Create(mCtx, "while.cond", mCurFunc)};
  auto body_block{llvm::BasicBlock::Create(mCtx, "while.body", mCurFunc)};
  auto end_block{llvm::BasicBlock::Create(mCtx, "while.end", mCurFunc)};

  // mount relative data
  obj->any = end_block;
  obj->cond->any = cond_block;

  // process cond
  irb.CreateBr(cond_block);
  irb.SetInsertPoint(cond_block);
  auto cond_res = self(obj->cond);

  irb.CreateCondBr(cast_to_boolean(cond_res), body_block, end_block);

  // process body block
  irb.SetInsertPoint(body_block);
  self(obj->body);
  if (!irb.GetInsertBlock()->getTerminator())
    irb.CreateBr(cond_block);

  // end processing
  irb.SetInsertPoint(end_block);
}

void EmitIR::operator()(BreakStmt *obj) {
  auto &irb{*mCurIrb};
  if (auto p{obj->loop->dcst<WhileStmt>()}; p) {
    irb.CreateBr(reinterpret_cast<llvm::BasicBlock *>(p->any));
  } else {
    ABORT();
  }
}

void EmitIR::operator()(ContinueStmt *obj) {
  auto &irb{*mCurIrb};
  if (auto p{obj->loop->dcst<WhileStmt>()}; p) {
    irb.CreateBr(reinterpret_cast<llvm::BasicBlock *>(p->cond->any));
  } else {
    ABORT();
  }
}

void EmitIR::operator()(NullStmt *obj) { return; }

//==============================================================================
// 声明
//==============================================================================

void EmitIR::operator()(Decl *obj) {
  // TODO: 添加变量声明处理的跳转
  if (auto p = obj->dcst<VarDecl>())
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

// TODO: 添加变量声明的处理
void EmitIR::trans_init(llvm::Value *val, Expr *obj, llvm::Type *type) {
  auto &irb = *mCurIrb;

  // 处理整数字面量的初始化
  if (auto p = obj->dcst<IntegerLiteral>()) {
    auto initVal = llvm::ConstantInt::get(self(p->type), p->val);
    irb.CreateStore(initVal, val);
    return;
  }

  if (auto p = obj->dcst<
               InitListExpr>()) // 处理初始化列表的初始化（val是个数组类型）
  {
    LocationAndValue locationAndValue;
    llvm::Constant *arrayConstant{array_constant(
        llvm::dyn_cast<llvm::ArrayType>(type), p, locationAndValue)};
    irb.CreateStore(arrayConstant, val);
    // 如果有无法用常量初始化的就在这里初始化
    for (const auto &[location, value] : locationAndValue) {
      /// GEP 指令访问 数组的指定位置
      llvm::Value *element = irb.CreateInBoundsGEP(
          llvm::dyn_cast<llvm::ArrayType>(type), val, location);
      irb.CreateStore(value, element);
    }
    return;
  }

  if (auto p = obj->dcst<ImplicitCastExpr>()) {
    auto initVal{self(p)};
    irb.CreateStore(initVal, val);
    return;
  }

  if (auto p = obj->dcst<CallExpr>()) {
    auto initVal{self(p)};
    irb.CreateStore(initVal, val);
    return;
  }

  if (auto p = obj->dcst<BinaryExpr>()) {
    auto initVal{self(p)};
    irb.CreateStore(initVal, val);
    return;
  }

  if (auto p = obj->dcst<UnaryExpr>()) {
    auto initVal{self(p)};
    irb.CreateStore(initVal, val);
    return;
  }

  if (auto p = obj->dcst<ParenExpr>()) {
    auto initVal{self(p)};
    irb.CreateStore(initVal, val);
    return;
  }

  ABORT();
}

void EmitIR::operator()(VarDecl *obj) {
  auto &irb{*mCurIrb};
  auto ty = self(obj->type);

  if (!mCurFunc) {
    auto global_var = new llvm::GlobalVariable(
        mMod, ty, false, llvm::GlobalVariable::ExternalLinkage, nullptr,
        obj->name);

    obj->any = global_var;
    global_var->setInitializer(llvm::Constant::getNullValue(ty));

    if (obj->init == nullptr)
      return;

    // jump to initializer so that we can use certain init_function to init
    // variable
    save_state();

    mCurFunc =
        llvm::Function::Create(mCtorTy, llvm::GlobalVariable::PrivateLinkage,
                               "ctor_" + obj->name, mMod);
    llvm::appendToGlobalCtors(mMod, mCurFunc, 65535);

    auto entry_block{llvm::BasicBlock::Create(mCtx, "entry", mCurFunc)};
    irb.SetInsertPoint(entry_block);
    trans_init(global_var, obj->init, ty);
    irb.CreateRet(nullptr);

    restore_state();
    return;
  }

  auto local_var = irb.CreateAlloca(ty, nullptr, obj->name);
  obj->any = local_var;

  if (obj->init != nullptr)
    trans_init(local_var, obj->init, ty);
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
  mCurIrb->SetInsertPoint(entryBb);
  auto &entryIrb = *mCurIrb;

  // 翻译函数体
  mCurFunc = func;

  // TODO: 添加对函数参数的处理
  int i{};
  for (auto argIter{func->arg_begin()}; argIter != func->arg_end(); ++argIter) {
    // 设置函数参数的名字
    argIter->setName(obj->params[i]->name);
    // 创建一个局部变量指向函数的参数
    auto local_var{mCurIrb->CreateAlloca(self(obj->params[i]->type), nullptr,
                                         obj->params[i]->name + ".addr")};
    mCurIrb->CreateStore(&(*argIter), local_var);
    obj->params[i]->any = local_var;
    i++;
  }

  self(obj->body);
  auto &exitIrb = *mCurIrb;

  if (fty->getReturnType()->isVoidTy())
    exitIrb.CreateRetVoid();
  else
    exitIrb.CreateUnreachable();

  mCurFunc = nullptr;
}

//==============================================================================
// 自己定义的辅助函数
//==============================================================================

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