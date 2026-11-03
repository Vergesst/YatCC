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

// 从声明前缀 ctx（如 const int）解析基础类型及限定符。
// ctx 可来自变量声明、函数返回类型或函数参数声明。
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

  // ？
  if (auto p = expr->dcst<InitListExpr>()) {
    if (p->list.empty())
      return 0;
    return eval_arrlen(p->list[0]);
  }

  ABORT();
}

// TypeName varName
// or
// TypeName varName[]
// and so on
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

// comma seperated expressions like
// x = 1, y  = 2;
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

// assignment exprsssions
// Unary kAssign AssignmentExpression
Expr *Ast2Asg::operator()(ast::AssignmentExpressionContext *ctx) {
  if (auto p = ctx->additiveExpression())
    return self(p);

  auto ret = make<BinaryExpr>();
  ret->op = asg::BinaryExpr::kAssign;
  ret->lft = self(ctx->unaryExpression());
  ret->rht = self(ctx->assignmentExpression());
  return ret;
}

// additiveExpressions
// Unary kAdd AdditiveExpression
Expr *Ast2Asg::operator()(ast::AdditiveExpressionContext *ctx) {
  // 由于 antlr 根据bnf生成的AdditiveExpression是 AdditiveExpression {Mul1, op1,
  // Mul2, op2, ...} 此类，所以需要使用ctx->children来做左结合处理
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

  // UnaryExpression: UnaryOperator UnaryExpression;
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

    // fetch reference from symbol table
    ret->decl = mSymtbl->resolve(name);
    return ret;
  }

  // process Constant
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

// MultiplicativeExpression: UnaryExpression ((Star | Div | Mod)
// UnaryExpression) so there is a vector<UnaryExpression*> and vector<op> inside
// MultiplicativeExpression
Expr *Ast2Asg::operator()(ast::MultiplicativeExpressionContext *ctx) {
  auto ret{self(ctx->unaryExpression()[0])};

  auto children = ctx->children;
  /**
   * 3 * 4 / 2 * 5
   *
   *           kMul
   *          /    \
   *       kDiv     5
   *      /    \
   *   kMul     2
   *  /    \
   * 3      4
   *
   * to create ((3 * 4) / 2) * 5
   *
   * ret = 3
   * read '*' and 4: ret = kMul(ret, 4)
   * read '/' and 2: ret = kDiv(ret, 2)
   * read '*' and 5: ret = kMul(ret, 5)
   */

  for (unsigned i = 1; i < children.size(); i += 2) {
    auto op = dynamic_cast<antlr4::tree::TerminalNode *>(children[i]);
    auto unary_expr =
        dynamic_cast<ast::UnaryExpressionContext *>(children[i + 1]);
    auto node = make<BinaryExpr>();

    if (!op || !unary_expr) {
      ABORT();
    } else {
      switch (op->getSymbol()->getType()) {
      case ast::Star:
        node->op = BinaryExpr::kMul;
        break;
      case ast::Div:
        node->op = BinaryExpr::kDiv;
        break;
      case ast::Mod:
        node->op = BinaryExpr::kMod;
        break;
      default:
        ABORT();
      }
    }

    // process ret and node
    node->lft = ret;
    node->rht = self(unary_expr);
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

// bracedExpression: LeftParen logicalExpression RightParen
// Preserve the parenthesized subexpression as ParenExpr.
Expr *Ast2Asg::operator()(ast::BracedExpressionContext *ctx) {
  auto node{make<ParenExpr>()};
  auto children = ctx->children;

  // if (auto p = dynamic_cast<ast::LogicalExpressionContext *>(children[1])) {
  //   node->sub = self(p);
  // } else {
  //   ABORT();
  // }
  node->sub = self(ctx->logicalExpression());

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

// TODO! UNDERSTAND THE FUNCTION_CALL_EXPRESSION
Expr *Ast2Asg::operator()(ast::FunctionCallExpressionContext *ctx) {
  // auto ret{make<CallExpr>()};
  // if (auto p = ctx->Identifier()) {
  //   auto name = p->getText();
  //   auto ident{make<DeclRefExpr>()};
  //   ident->decl = mSymtbl->resolve(name);
  //   ret->head = ident;
  // } else {
  //   ABORT();
  // }

  // auto children = ctx->children;
  // for (size_t i = 2; i < children.size() - 1; i += 2) {
  //   if (dynamic_cast<ast::LogicalExpressionContext *>(children[i]))
  //     ret->args.push_back(
  //         self(dynamic_cast<ast::LogicalExpressionContext *>(children[i])));
  //   else
  //     ABORT();
  // }

  // return ret;

  auto call_expr{make<CallExpr>()};
  auto p = ctx->Identifier();
  auto args = ctx->logicalExpression();

  if (!p)
    ABORT();
  auto name = p->getText();
  auto ident{make<DeclRefExpr>()};
  // locate the FunctionDecl
  ident->decl = mSymtbl->resolve(name);
  call_expr->head = ident;

  for (auto ele : args) {
    call_expr->args.push_back(self(ele));
  }

  return call_expr;
}

Expr *Ast2Asg::operator()(ast::IndexExpressionContext *ctx) {
  auto node{make<BinaryExpr>()};
  auto children{ctx->children};

  node->op = BinaryExpr::kIndex;

  auto name = ctx->Identifier()->getText();
  // do not forget to resolve Identifier --- error - seg fault
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
    // create symbol table
    Symtbl local_decls(self);

    for (auto &&i : p->blockItem()) {
      if (auto q = i->declaration()) {
        auto sub = make<DeclStmt>();
        // insert decl into symbol table at the end of this call.
        sub->decls = self(q);
        ret->subs.push_back(sub);
      }

      else if (auto q = i->statement())
        ret->subs.push_back(self(q));

      else
        ABORT();
    }
  }

  // with RAII (or drop in rust), mSymtbl will be able to link to outer symbol
  // table
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

// only return stmt
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
  auto stmts = ctx->statement();

  // process basic elements of IfStmt
  ret->cond = self(ctx->logicalExpression());
  // then clause
  if (stmts.empty() || stmts.size() > 2)
    ABORT();
  ret->then = self(stmts[0]);

  if (ctx->Else()) {
    if (stmts.size() != 2)
      ABORT();

    ret->else_ = self(stmts[1]);
  }
  // TODO: 为什么下面的代码有问题
  // if (ctx->Else() && stmts.size() == 2) {
  //   ret->else_ = self(stmts[1]);
  // } else {
  //   ABORT();
  // }

  return ret;
}

Stmt *Ast2Asg::operator()(ast::WhileStatementContext *ctx) {
  auto ret{make<WhileStmt>()};
  // 使用 ANTLR 提供的 api 获取各个成分

  ret->cond = self(ctx->logicalExpression());
  // then clause
  ret->body = self(ctx->statement());

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

  // auto ret{make<FunctionDecl>()};
  // mCurrentFunc = ret;

  // 创建 ASG 中的函数声明节点：

  // FunctionDecl(ret)

  // 并记为“当前正在处理的函数”，后续 return 会指回它。

  // auto type{make<Type>()};
  // ret->type = type;

  // 为 f 创建类型对象：

  // ret->type → Type

  // auto sq{self(ctx->declarationSpecifiers())};
  // type->spec = sq.first;
  // type->qual = sq.second;

  // 解析源码中的 int，得到：

  // sq.first  = Type::Spec::kInt
  // sq.second = Type::Qual { const_ = false }

  // 所以你注释的理解是对的：grammar 把 const 和 int 都塞进 typeSpecifier；但
  // SpecQual 会将它们重新拆为：

  // 基础类型     → spec
  // 限定符       → qual

  // auto [type_expr, name] = self(ctx->directDeclarator(), nullptr);

  // 这是 C++ 的结构化绑定，拆开 pair<TypeExpr*, std::string>：

  // auto result = self(ctx->directDeclarator(), nullptr);

  // auto type_expr = result.first;
  // auto name = result.second;

  // 对简单的 f：

  // type_expr = nullptr
  // name      = "f"

  // directDeclarator 只负责名字和数组形状；不是解析参数。若是：

  // int a[3];

  // 它才会构造 ArrayType 并作为 type_expr 返回。

  // auto func_type {make<FunctionType>()};
  // func_type->sub = type_expr;
  // type->texp = func_type;

  // 给原来的 int 再包一层“这是函数类型”：

  // FunctionDecl(f)
  // └─ Type
  //    ├─ spec: kInt
  //    ├─ qual: {}
  //    └─ texp: FunctionType
  //       └─ sub: nullptr

  // 之后参数列表会填入：

  // func_type->params

  // 最后：

  // ret->name = std::move(name);

  // 把 "f" 移交给 FunctionDecl。std::move 这里只是避免复制临时字符串。

  // ———

  // 每个 self(...) 都只是：

  // #define self (*this)

  // 所以：

  // self(ctx->declarationSpecifiers())

  // 等价于：

  // (*this)(ctx->declarationSpecifiers())

  // 即“调用当前 Ast2Asg 对象中，参数类型匹配的转换函数”。

  // self(DeclarationSpecifiersContext*)
  // → 调用 SpecQual operator()(DeclarationSpecifiersContext*)

  // self(DirectDeclaratorContext*, nullptr)
  // → 调用 pair<TypeExpr*, string> operator()(DirectDeclaratorContext*, ...)

  // ———

  // 局部符号表则是：

  // Symtbl local_decls(self);

  // 展开理解为：

  // Symtbl local_decls(*this);

  // 构造函数做的是：

  // mPrev = m.mSymtbl; // 保存此前的“当前符号表”
  // m.mSymtbl = this;  // 把自己设为新的当前符号表

  // 它创建的是一张空的函数局部表，并不复制全局表内容：

  // 函数局部表（空）
  // └─ mPrev → 外层全局表

  // 以后查找名字：

  // 先查函数局部表
  // → 找不到则递归查 mPrev
  // → 即全局表

  // 所以局部作用域“看得见”外层符号，不是因为复制了外层内容，而是因为它保存了外层表的链接。

  auto ret{make<FunctionDecl>()};
  mCurrentFunc = ret;

  auto type{make<Type>()};
  ret->type = type;

  auto sq{self(ctx->declarationSpecifiers())};
  // hyw 你的意思是，在文法设计上把 const --- type_qual 和 int / char / long /
  // long long --- type_spec 混在一起，但是实际提取的时候还是分开做了一个 pair吗
  // 那很有设计了
  type->spec = sq.first;
  type->qual = sq.second;

  // 这啥
  auto [type_expr, name] = self(ctx->directDeclarator(), nullptr);
  auto func_type{make<FunctionType>()};
  func_type->sub = type_expr;
  type->texp = func_type;
  ret->name = std::move(name);

  // 插入符号表
  (*mSymtbl)[ret->name] = ret;

  // 处理函数参数和 local_symbol_table
  Symtbl local_tbl(self);

  if (auto p{ctx->parameterList()}) {
    ret->params = self(p);
  }

  if (auto p{ctx->compoundStatement()}) {
    ret->body = self(p);
  }

  return ret;
}

// 真有意思，前面写了 FunctionDeclarationContext 的处理这里又写一遍
Decl *Ast2Asg::operator()(ast::InitDeclaratorContext *ctx, SpecQual sq) {
  auto [texp, name] = self(ctx->declarator(), nullptr);
  auto ret{make<Decl>()};
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
  auto decl_spec = ctx->declarationSpecifiers();
  auto init_decl = ctx->initDeclarator();
  if (decl_spec.size() != init_decl.size()) {
    ABORT();
  }

  for (size_t i = 0; i < decl_spec.size(); i++) {
    auto spec = self(decl_spec[i]);
    // 此处调用 Decl* operator()(InitDeclaratorContext* ctx, SpecQual spec)
    // InitDeclaratorConntext里面已经做了符号表插入
    auto new_ident = self(init_decl[i], spec);

    ret.push_back(new_ident);
    // 这是不需要的重复代码
    // (*mSymtbl)[new_ident->name] = new_ident;
  }

  return ret;
}

} // namespace asg
