#include "Ast2Asg.hpp"
#include "Obj.hpp"
#include "asg.hpp"
#include <cstdlib>
#include <tree/TerminalNode.h>
#include <unordered_map>

#define self (*this)

namespace asg {

// 符号表，保存当前作用域的所有声明
struct Ast2Asg::Symtbl : public std::unordered_map<std::string, Decl *> {
  Ast2Asg &m;
  Symtbl *mPrev;

  Symtbl(Ast2Asg &m) : m(m), mPrev(m.mSymtbl) { m.mSymtbl = this; }

  ~Symtbl() { m.mSymtbl = mPrev; }

  Decl *resolve(const std::string &name);
};

Decl *Ast2Asg::Symtbl::resolve(const std::string &name) {
  auto iter = find(name);
  if (iter != end())
    return iter->second;
  ASSERT(mPrev != nullptr); // 标识符未定义
  return mPrev->resolve(name);
}

TranslationUnit *Ast2Asg::operator()(ast::TranslationUnitContext *ctx) {
  auto ret = make<asg::TranslationUnit>();
  if (ctx == nullptr)
    return ret;

  Symtbl localDecls(self);

  for (auto &&i : ctx->externalDeclaration()) {
    if (auto p = i->declaration()) {
      auto decls = self(p);
      ret->decls.insert(ret->decls.end(),
                        std::make_move_iterator(decls.begin()),
                        std::make_move_iterator(decls.end()));
    }

    else if (auto p = i->functionDeclaration()) {
      auto funcDecl = self(p);
      ret->decls.push_back(funcDecl);

      // 添加到声明表
      localDecls[funcDecl->name] = funcDecl;
    }

    else
      ABORT();
  }

  return ret;
}

//==============================================================================
// 类型
//==============================================================================

Ast2Asg::SpecQual Ast2Asg::operator()(ast::DeclarationSpecifiersContext *ctx) {
  SpecQual ret = {Type::Spec::kINVALID, Type::Qual()};

  auto set_type{[&ret](const Type::Spec &type) {
    if (ret.first == Type::Spec::kINVALID) {
      ret.first = type;
    } else {
      ABORT();
    }
  }};

  for (auto &&i : ctx->declarationSpecifier()) {
    if (auto p = i->typeSpecifier()) {
      if (p->Int()) {
        set_type(Type::Spec::kInt);
      } else if (p->Char()) {
        set_type(Type::Spec::kChar);
      } else if (p->Void()) {
        set_type(Type::Spec::kVoid);
      } else if (p->Long()) {
        set_type(Type::Spec::kLong);
      } else if (p->LongLong()) {
        set_type(Type::Spec::kLongLong);
      } else if (p->Const()) {
        if (!ret.second.const_) {
          ret.second.const_ = true;
        } else {
          ABORT(); // 重复的Const限定符
        }
      } else {
        ABORT(); // 异常情况
      }
    }
  }

  return ret;
}

std::pair<TypeExpr *, std::string>
Ast2Asg::operator()(ast::DeclaratorContext *ctx, TypeExpr *sub) {
  return self(ctx->directDeclarator(), sub);
}

static int eval_arrlen(Expr *expr) {
  if (auto p = expr->dcst<IntegerLiteral>())
    return p->val;

  if (auto p = expr->dcst<DeclRefExpr>()) {
    if (p->decl == nullptr)
      ABORT();

    auto var = p->decl->dcst<VarDecl>();
    if (!var || !var->type->qual.const_)
      ABORT(); // 数组长度必须是编译期常量

    switch (var->type->spec) {
    case Type::Spec::kChar:
    case Type::Spec::kInt:
    case Type::Spec::kLong:
    case Type::Spec::kLongLong:
      return eval_arrlen(var->init);

    default:
      ABORT(); // 长度表达式必须是数值类型
    }
  }

  if (auto p = expr->dcst<UnaryExpr>()) {
    auto sub = eval_arrlen(p->sub);

    switch (p->op) {
    case UnaryExpr::kPos:
      return sub;

    case UnaryExpr::kNeg:
      return -sub;

    default:
      ABORT();
    }
  }

  if (auto p = expr->dcst<BinaryExpr>()) {
    auto lft = eval_arrlen(p->lft);
    auto rht = eval_arrlen(p->rht);

    switch (p->op) {
    case BinaryExpr::kAdd:
      return lft + rht;

    case BinaryExpr::kSub:
      return lft - rht;

    default:
      ABORT();
    }
  }

  if (auto p = expr->dcst<InitListExpr>()) {
    if (p->list.empty())
      return 0;
    return eval_arrlen(p->list[0]);
  }

  ABORT();
}

std::pair<TypeExpr *, std::string>
Ast2Asg::operator()(ast::DirectDeclaratorContext *ctx, TypeExpr *sub) {
  if (auto p = ctx->Identifier())
    return {sub, p->getText()};

  if (ctx->LeftBracket()) {
    auto array_type = make<ArrayType>();
    array_type->sub = sub;

    if (auto p = ctx->assignmentExpression())
      array_type->len = eval_arrlen(self(p));
    else
      array_type->len = ArrayType::kUnLen;

    return self(ctx->directDeclarator(), array_type);
  }

  ABORT();
}

//==============================================================================
// 表达式
//==============================================================================

Expr *Ast2Asg::operator()(ast::ExpressionContext *ctx) {
  auto list = ctx->assignmentExpression();
  Expr *ret = self(list[0]);

  for (unsigned i = 1; i < list.size(); ++i) {
    auto node = make<BinaryExpr>();
    node->op = asg::BinaryExpr::kComma;
    node->lft = ret;
    node->rht = self(list[i]);
    ret = node;
  }

  return ret;
}

Expr *Ast2Asg::operator()(ast::AssignmentExpressionContext *ctx) {
  if (auto p = ctx->additiveExpression())
    return self(p);

  auto ret = make<BinaryExpr>();
  ret->op = asg::BinaryExpr::kAssign;
  ret->lft = self(ctx->unaryExpression());
  ret->rht = self(ctx->assignmentExpression());
  return ret;
}

Expr *Ast2Asg::operator()(ast::AdditiveExpressionContext *ctx) {
  auto children = ctx->children;
  Expr *ret =
      self(dynamic_cast<ast::MultiplicativeExpressionContext *>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode *>(children[i])
                     ->getSymbol()
                     ->getType();
    switch (token) {
      case ast::Plus:
        node->op = asg::BinaryExpr::kAdd;
        break;

      case ast::Minus:
        node->op = asg::BinaryExpr::kSub;
        break;

      default:
        ABORT();
      }

      node->lft = ret;
      node->rht = self(
          dynamic_cast<ast::MultiplicativeExpressionContext *>(children[++i]));
      ret = node;
  }

  return ret;
}

Expr *Ast2Asg::operator()(ast::UnaryExpressionContext *ctx) {
  if (auto p = ctx->postfixExpression()) {
    return self(p);
  }
  if (auto p = ctx->bracedExpression()) {
    return self(p);
  }
  if (auto p = ctx->indexExpression()) {
    return self(p);
  }
  if (auto p = ctx->functionCallExpression()) {
    return self(p);
  }

  auto ret = make<UnaryExpr>();

  switch (dynamic_cast<antlr4::tree::TerminalNode *>(
              ctx->unaryOperator()->children[0])
              ->getSymbol()
              ->getType()) {
  case ast::Plus:
    ret->op = asg::UnaryExpr::kPos;
    break;

  case ast::Minus:
    ret->op = asg::UnaryExpr::kNeg;
    break;

  case ast::LogicalNot:
    ret->op = asg::UnaryExpr::kNot;
    break;

  default:
    ABORT();
  }

  ret->sub = self(ctx->unaryExpression());

  return ret;
}

Expr *Ast2Asg::operator()(ast::PrimaryExpressionContext *ctx) {

  if (auto p = ctx->Identifier()) {
    auto name = p->getText();
    auto ret = make<DeclRefExpr>();
    ret->decl = mSymtbl->resolve(name);
    return ret;
  }

  if (auto p = ctx->Constant()) {
    auto text = p->getText();

    auto ret = make<IntegerLiteral>();

    ASSERT(!text.empty());
    if (text[0] != '0')
      ret->val = std::stoll(text);

    else if (text.size() == 1)
      ret->val = 0;

    else if (text[1] == 'x' || text[1] == 'X')
      ret->val = std::stoll(text.substr(2), nullptr, 16);

    else
      ret->val = std::stoll(text.substr(1), nullptr, 8);

    return ret;
  }

  ABORT();
}

Expr *Ast2Asg::operator()(ast::InitializerContext *ctx) {
  if (auto p = ctx->assignmentExpression())
    return self(p);

  auto ret = make<InitListExpr>();

  if (auto p = ctx->initializerList()) {
    for (auto &&i : p->initializer()) {
      // 将初始化列表展平
      auto expr = self(i);
      if (auto p = expr->dcst<InitListExpr>()) {
        for (auto &&sub : p->list)
          ret->list.push_back(sub);
      } else {
        ret->list.push_back(expr);
      }
    }
  }

  return ret;
}

Expr *Ast2Asg::operator()(ast::MultiplicativeExpressionContext *ctx) {
  auto ret{self(ctx->unaryExpression()[0])};

  auto children = ctx->children;
  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto op = dynamic_cast<antlr4::tree::TerminalNode *>(children[i])
                  ->getSymbol()
                  ->getType();

    if (op == ast::Star)
      node->op = BinaryExpr::kMul;
    else if (op == ast::Div)
      node->op = BinaryExpr::kDiv;
    else if (op == ast::Mod)
      node->op = BinaryExpr::kMod;
    else
      ABORT();

    node->lft = ret;
    node->rht =
        self(dynamic_cast<ast::UnaryExpressionContext *>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr *Ast2Asg::operator()(ast::RelationExpressionContext *ctx) {
  auto ret{self(ctx->additiveExpression()[0])};
  auto children = ctx->children;

  for (size_t i = 1; i < children.size(); ++i) {
    auto node{make<BinaryExpr>()};

    auto op{dynamic_cast<antlr4::tree::TerminalNode *>(children[i])
                ->getSymbol()
                ->getType()};

    if (op == ast::EqualEqual)
      node->op = BinaryExpr::kEq;
    else if (op == ast::NotEqual)
      node->op = BinaryExpr::kNe;
    else if (op == ast::Greater)
      node->op = BinaryExpr::kGt;
    else if (op == ast::Less)
      node->op = BinaryExpr::kLt;
    else if (op == ast::GreaterEqual)
      node->op = BinaryExpr::kGe;
    else if (op == ast::LessEqual)
      node->op = BinaryExpr::kLe;
    else
      ABORT();

    node->lft = ret;
    node->rht =
        self(dynamic_cast<ast::AdditiveExpressionContext *>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr *Ast2Asg::operator()(ast::BracedExpressionContext *ctx) {
  auto node{make<ParenExpr>()};
  auto children = ctx->children;

  if (auto p = dynamic_cast<ast::LogicalExpressionContext *>(children[1])) {
    node->sub = self(p);
  } else {
    ABORT();
  }

  return node;
}

Expr *Ast2Asg::operator()(ast::LogicalExpressionContext *ctx) {
  auto ret{self(ctx->logicalAndExpression()[0])};
  auto children = ctx->children;

  for (size_t i = 1; i < children.size(); ++i) {
    auto node{make<BinaryExpr>()};
    auto op = dynamic_cast<antlr4::tree::TerminalNode *>(children[i])
                  ->getSymbol()
                  ->getType();

    if (op == ast::LogicalOr)
      node->op = BinaryExpr::kOr;
    else
      ABORT();

    node->lft = ret;
    node->rht =
        self(dynamic_cast<ast::LogicalAndExpressionContext *>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr *Ast2Asg::operator()(ast::LogicalAndExpressionContext *ctx) {
  auto ret{self(ctx->relationExpression()[0])};
  auto children = ctx->children;

  for (size_t i = 1; i < children.size(); ++i) {
    auto node{make<BinaryExpr>()};
    auto op = dynamic_cast<antlr4::tree::TerminalNode *>(children[i])
                  ->getSymbol()
                  ->getType();

    if (op == ast::LogicalAnd)
      node->op = BinaryExpr::kAnd;
    else
      ABORT();

    node->lft = ret;
    node->rht =
        self(dynamic_cast<ast::RelationExpressionContext *>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr *Ast2Asg::operator()(ast::FunctionCallExpressionContext *ctx) {
  auto ret{make<CallExpr>()};
  if (auto p = ctx->Identifier()) {
    auto name = p->getText();
    auto ident{make<DeclRefExpr>()};
    ident->decl = mSymtbl->resolve(name);
    ret->head = ident;
  } else {
    ABORT();
  }

  auto children = ctx->children;
  for (size_t i = 2; i < children.size() - 1; i += 2) {
    if (dynamic_cast<ast::LogicalExpressionContext *>(children[i]))
      ret->args.push_back(
          self(dynamic_cast<ast::LogicalExpressionContext *>(children[i])));
    else
      ABORT();
  }

  return ret;
}

Expr *Ast2Asg::operator()(ast::IndexExpressionContext *ctx) {
  auto node{make<BinaryExpr>()};
  auto children{ctx->children};

  // children[0]是Indentifier，children[1]是[，children[2]是UnaryExpression，children[3]是]。如果有children[4]，又是[，以此类推
  node->op = BinaryExpr::kIndex;

  auto name = ctx->Identifier()->getText();
  auto identifier = make<DeclRefExpr>();
  identifier->decl = mSymtbl->resolve(name);
  Expr *ret{identifier};

  for (size_t i{2}; i < children.size(); i += 3) {
    node = make<BinaryExpr>();

    node->op = BinaryExpr::kIndex;
    node->lft = ret;
    node->rht =
        self(dynamic_cast<ast::LogicalExpressionContext *>(children[i]));

    ret = node;
  }

  return ret;
}

//==============================================================================
// 语句
//==============================================================================

CompoundStmt *Ast2Asg::operator()(ast::CompoundStatementContext *ctx) {
  auto ret = make<CompoundStmt>();

  if (auto p = ctx->blockItemList()) {
    Symtbl local_decls(self);

    for (auto &&i : p->blockItem()) {
      if (auto q = i->declaration()) {
        auto sub = make<DeclStmt>();
        sub->decls = self(q);
        ret->subs.push_back(sub);
      }

      else if (auto q = i->statement())
        ret->subs.push_back(self(q));

      else
        ABORT();
    }
  }

  return ret;
}

Stmt *Ast2Asg::operator()(ast::ExpressionStatementContext *ctx) {
  if (auto p = ctx->expression()) {
    auto ret = make<ExprStmt>();
    ret->expr = self(p);
    return ret;
  }

  return make<NullStmt>();
}

Stmt *Ast2Asg::operator()(ast::JumpStatementContext *ctx) {
  if (ctx->Return()) {
    auto ret = make<ReturnStmt>();
    ret->func = mCurrentFunc;
    if (auto p = ctx->expression())
      ret->expr = self(p);
    return ret;
  }

  ABORT();
}

// Stmt *Ast2Asg::operator()(ast::IfElseStatementContext *ctx);

// Stmt *Ast2Asg::operator()(ast::WhileStatementContext *ctx);

// Stmt *Ast2Asg::operator()(ast::BreakStatementContext *ctx);

// Stmt *Ast2Asg::operator()(ast::ContinueStatementContext *ctx);
Stmt *Ast2Asg::operator()(ast::IfElseStatementContext *ctx) {
  auto ret{make<IfStmt>()};

  auto children = ctx->children;

  // children里的依次是：if,(,条件,),语句,else,语句
  ret->cond = self(dynamic_cast<ast::LogicalExpressionContext *>(children[2]));

  ret->then = self(dynamic_cast<ast::StatementContext *>(children[4]));

  if (children.size() > 5) // 说明有else语句
  {
    if (auto p{dynamic_cast<ast::StatementContext *>(children[6])}) {
      ret->else_ = self(p);
    } else {
      ABORT();
    }
  }

  return ret;
}

Stmt *Ast2Asg::operator()(ast::WhileStatementContext *ctx) {
  auto ret{make<WhileStmt>()};

  auto children = ctx->children;

  // children里的依次是：while,(,条件,),语句

  ret->cond = self(dynamic_cast<ast::LogicalExpressionContext *>(children[2]));

  ret->body = self(dynamic_cast<ast::StatementContext *>(children[4]));

  return ret;
}

Stmt *Ast2Asg::operator()(ast::BreakStatementContext *ctx) {
  auto ret{make<BreakStmt>()};
  return ret;
}

Stmt *Ast2Asg::operator()(ast::ContinueStatementContext *ctx) {
  auto ret{make<ContinueStmt>()};
  return ret;
}

//==============================================================================
// 声明
//==============================================================================

std::vector<Decl *> Ast2Asg::operator()(ast::DeclarationContext *ctx) {
  std::vector<Decl *> ret;

  auto specs = self(ctx->declarationSpecifiers());

  if (auto p = ctx->initDeclaratorList()) {
    for (auto &&j : p->initDeclarator())
      ret.push_back(self(j, specs));
  }

  // 如果 initDeclaratorList 为空则这行声明语句无意义
  return ret;
}

FunctionDecl *Ast2Asg::operator()(ast::FunctionDeclarationContext *ctx) {
  auto ret = make<FunctionDecl>();
  mCurrentFunc = ret;

  auto type = make<Type>();
  ret->type = type;

  auto sq = self(ctx->declarationSpecifiers());
  type->spec = sq.first, type->qual = sq.second;

  auto [texp, name] = self(ctx->directDeclarator(), nullptr);
  auto func_type = make<FunctionType>();
  func_type->sub = texp;
  type->texp = func_type;
  ret->name = std::move(name);

  Symtbl local_decls(self);

  // 函数定义在签名之后就加入符号表，以允许递归调用
  (*mSymtbl)[ret->name] = ret;

  if (auto p{ctx->parameterList()}) {
    ret->params = self(p);
  }

  if (auto p{ctx->compoundStatement()}) {
    ret->body = self(p);
  }
  return ret;
}

Decl *Ast2Asg::operator()(ast::InitDeclaratorContext *ctx, SpecQual sq) {
  auto [texp, name] = self(ctx->declarator(), nullptr);
  Decl *ret;

  if (auto funcType = texp->dcst<FunctionType>()) {
    auto fdecl = make<FunctionDecl>();
    auto type = make<Type>();
    fdecl->type = type;

    type->spec = sq.first;
    type->qual = sq.second;
    type->texp = funcType;

    fdecl->name = std::move(name);
    for (auto p : funcType->params) {
      auto paramDecl = make<VarDecl>();
      paramDecl->type = p;
      fdecl->params.push_back(paramDecl);
    }

    if (ctx->initializer())
      ABORT();
    fdecl->body = nullptr;

    ret = fdecl;
  }

  else {
    auto vdecl = make<VarDecl>();
    auto type = make<Type>();
    vdecl->type = type;

    type->spec = sq.first;
    type->qual = sq.second;
    type->texp = texp;
    vdecl->name = std::move(name);

    if (auto p = ctx->initializer())
      vdecl->init = self(p);
    else
      vdecl->init = nullptr;

    ret = vdecl;
  }

  // 这个实现允许符号重复定义，新定义会取代旧定义
  (*mSymtbl)[ret->name] = ret;
  return ret;
}

Expr *Ast2Asg::operator()(ast::PostfixExpressionContext *ctx) {
  auto children = ctx->children;
  auto sub = self(dynamic_cast<ast::PrimaryExpressionContext *>(children[0]));
  return sub;
}

//==============================================================================
// 语句
//==============================================================================

Stmt *Ast2Asg::operator()(ast::StatementContext *ctx) {
  if (auto p = ctx->compoundStatement())
    return self(p);

  if (auto p = ctx->expressionStatement())
    return self(p);

  if (auto p = ctx->jumpStatement())
    return self(p);

  if (auto p = ctx->ifElseStatement())
    return self(p);

  if (auto p = ctx->whileStatement())
    return self(p);

  if (auto p = ctx->breakStatement())
    return self(p);

  if (auto p = ctx->continueStatement())
    return self(p);

  ABORT();
}

std::vector<Decl *> Ast2Asg::operator()(ast::ParameterListContext *ctx) {
  std::vector<Decl *> ret;

  auto children = ctx->children;

  for (size_t i{}; i < children.size(); i += 3) {
    if (!dynamic_cast<ast::DeclarationSpecifiersContext *>(children[i]) ||
        !dynamic_cast<ast::InitDeclaratorContext *>(children[i + 1])) {
      ABORT();
    }
    auto specs =
        self(dynamic_cast<ast::DeclarationSpecifiersContext *>(children[i]));
    auto new_identifier{self(
        dynamic_cast<ast::InitDeclaratorContext *>(children[i + 1]), specs)};
    ret.push_back(new_identifier);
    (*mSymtbl)[new_identifier->name] = new_identifier;
  }

  return ret;
}

} // namespace asg
