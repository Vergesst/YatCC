#include "asg.hpp"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <optional>

class EmitIR {
public:
  Obj::Mgr &mMgr;
  llvm::Module mMod;

  EmitIR(Obj::Mgr &mgr, llvm::LLVMContext &ctx, llvm::StringRef mid = "-");

  llvm::Module &operator()(asg::TranslationUnit *tu);

private:
  llvm::LLVMContext &mCtx;

  llvm::Type *mIntTy{};
  llvm::FunctionType *mCtorTy{};

  llvm::Function *mCurFunc{};
  std::unique_ptr<llvm::IRBuilder<>> mCurIrb;

  //============================================================================
  // 类型
  //============================================================================

  llvm::Type *operator()(const asg::Type *type);

  //============================================================================
  // 表达式
  //============================================================================

  llvm::Value *operator()(asg::Expr *obj);

  llvm::Constant *operator()(asg::IntegerLiteral *obj);

  llvm::Value *operator()(asg::BinaryExpr *obj);

  llvm::Value *operator()(asg::UnaryExpr *obj);

  llvm::Value *operator()(asg::ParenExpr *obj);

  llvm::Value *operator()(asg::ImplicitCastExpr *obj);

  llvm::Value *operator()(asg::DeclRefExpr *obj);

  // TODO: 添加表达式处理相关声明

  llvm::Value *operator()(asg::CallExpr *obj);

  //============================================================================
  // 语句
  //============================================================================

  void operator()(asg::Stmt *obj);

  void operator()(asg::CompoundStmt *obj);

  void operator()(asg::ReturnStmt *obj);

  // TODO: 添加语句处理相关声明

  void operator()(asg::DeclStmt *obj);

  void operator()(asg::ExprStmt *obj);

  void operator()(asg::IfStmt *obj);

  void operator()(asg::WhileStmt *obj);

  void operator()(asg::BreakStmt *obj);

  void operator()(asg::ContinueStmt *obj);

  void operator()(asg::NullStmt *obj);

  //============================================================================
  // 声明
  //============================================================================

  void operator()(asg::Decl *obj);

  void operator()(asg::FunctionDecl *obj);

  void operator()(asg::VarDecl *obj);

  void trans_init(llvm::Value *val, asg::Expr *obj, llvm::Type *type);

  // TODO: 添加声明处理相关声明

  //==============================================================================
  // 自己定义的辅助函数
  //==============================================================================

  std::optional<llvm::Function *> mPreviousFunc;
  std::optional<llvm::BasicBlock *> mPreviousBasicBlock;
  using LocationAndValue =
      std::vector<std::pair<std::vector<llvm::Value *>, llvm::Value *>>;

  llvm::Constant *array_constant(llvm::ArrayType *arrayType,
                                 asg::InitListExpr *init,
                                 LocationAndValue &locationAndValue,
                                 std::vector<llvm::Value *> path = {});

  llvm::Value *cast_to_boolean(llvm::Value *val);

  void save_state();

  void restore_state();
};
