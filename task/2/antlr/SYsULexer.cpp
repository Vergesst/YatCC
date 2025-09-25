#include "SYsULexer.hpp"
#include "SYsULexer.tokens.hpp"
#include <unordered_map>
#include <vector>

using antlr4::ParseCancellationException;
using std::make_pair;

namespace {

using namespace SYsULexerTokens;

const std::unordered_map<std::string, size_t> k_clang_tokens{
    {"eof", antlr4::Token::EOF},
    {"int", kInt},
    {"identifier", kIdentifier},
    {"l_paren", kLeftParen},
    {"r_paren", kRightParen},
    {"return", kReturn},
    {"r_brace", kRightBrace},
    {"l_brace", kLeftBrace},
    {"numeric_constant", kConstant},
    {"semi", kSemi},
    {"equal", kEqual},
    {"plus", kPlus},
    {"minus", kMinus},
    {"comma", kComma},
    {"l_square", kLeftBracket},
    {"r_square", kRightBracket},
    // 补充的
    {"star", kStar},
    {"slash", kDiv},
    {"percent", kMod}, // 改到这里
    {"increment", kIncrement},
    {"decrement", kDecrement},
    {"bitwise_and", kBitwiseAnd},
    {"bitwise_or", kBitwiseOr},
    {"bitwise_xor", kBitwiseXor},
    {"bitwise_not", kBitwiseNot},
    {"ampamp", kLogicalAnd},
    {"pipepipe", kLogicalOr},
    {"exclaim", kLogicalNot},
    {"equalequal", kEqualEqual},
    {"exclaimequal", kNotEqual},
    {"greater", kGreater},
    {"less", kLess},
    {"greaterequal", kGreaterEqual},
    {"lessequal", kLessEqual},
    {"plus_equal", kPlusEqual},
    {"minus_equal", kMinusEqual},
    {"multiply_equal", kMultiplyEqual},
    {"divide_equal", kDivideEqual},
    {"modulus_equal", kModulusEqual},
    {"left_shift", kLeftShift},
    {"right_shift", kRightShift},
    {"left_shift_equal", kLeftShiftEqual},
    {"right_shift_equal", kRightShiftEqual},
    {"dot", kDot},
    {"arrow", kArrow},
    {"question", kQuestion},
    {"colon", kColon},
    {"if", kIf},
    {"else", kElse},
    {"while", kWhile},
    {"for", kFor},
    {"do", kDo},
    {"switch", kSwitch},
    {"case", kCase},
    {"default", kDefault},
    {"break", kBreak},
    {"continue", kContinue},
    {"goto", kGoto},
    {"char", kChar},
    {"short", kShort},
    {"long", kLong},
    {"float", kFloat},
    {"longlong", kLongLong},
    {"void", kVoid},
    {"const", kConst}};

} // namespace

SYsULexer::SYsULexer(antlr4::CharStream *input)
    : mInput(input), mSource(make_pair(this, input)),
      mFactory(antlr4::CommonTokenFactory::DEFAULT.get()),
      mSourceName(input->getSourceName()) {}

std::unique_ptr<antlr4::Token> SYsULexer::nextToken() {
  auto c = mInput->LA(1);
  if (c == antlr4::Token::EOF) {
    // 到达文件末尾，退出循环
    return common_token(antlr4::Token::EOF, mInput->index(), mInput->index());
  }

  auto start = mInput->index();
  std::string line;
  while (true) {
    mInput->consume();
    if (c == U'\n') {
      if (mInput->LA(1) == U'\r')
        mInput->consume();
      break;
    }
    if (c == U'\r') {
      if (mInput->LA(1) == U'\n')
        mInput->consume();
      break;
    }
    line.push_back(c);
    c = mInput->LA(1);
    if (c == antlr4::Token::EOF)
      return common_token(antlr4::Token::INVALID_TYPE, start, mInput->index());
  }
  auto stop = mInput->index();

  std::size_t type_end;
  std::size_t type;
  std::size_t text_end;
  std::string text;

  // 提取类型段
  {
    decltype(k_clang_tokens)::const_iterator type_iter;
    type_end = line.find(' ');
    if (type_end == std::string::npos)
      goto FAIL;
    type_iter = k_clang_tokens.find(line.substr(0, type_end));
    if (type_iter == k_clang_tokens.end())
      goto FAIL;
    type = type_iter->second;
  }

  // 提取文本段
  {
    text_end = line.find('\t', type_end + 1);
    if (text_end == std::string::npos ||
        text_end < type_end + 3 // 至少要有一对空引号 ''
    )
      goto FAIL;
    text = line.substr(type_end + 2, text_end - type_end - 3);
  }

  // 提取位置段
  {
    std::size_t loc_start;
    std::size_t loc_end;
    std::size_t col_start;
    std::size_t row_start;

    loc_start = line.find("Loc=<", text_end + 1);
    if (loc_start == std::string::npos)
      goto FAIL;
    loc_end = line.rfind(">");
    if (loc_end == std::string::npos)
      goto FAIL;
    col_start = line.rfind(':', loc_end);
    if (col_start == std::string::npos)
      goto FAIL;
    row_start = line.rfind(':', col_start - 1);
    if (row_start == std::string::npos)
      goto FAIL;

    mSourceName = line.substr(loc_start + 5, row_start - loc_start - 5);
    mLine = std::stoul(line.substr(row_start + 1, col_start - row_start - 1));
    mColumn = std::stoul(line.substr(col_start + 1, loc_end - col_start - 1));
  }

  // 解析成功
  return common_token(type, start, stop, text);

FAIL: // 解析失败
  assert(false);
}

size_t
SYsULexer::getLine() const
{
  return mLine;
}

size_t
SYsULexer::getCharPositionInLine()
{
  return mColumn;
}

antlr4::CharStream*
SYsULexer::getInputStream()
{
  return mInput;
}

std::string
SYsULexer::getSourceName()
{
  return mSourceName;
}

antlr4::TokenFactory<antlr4::CommonToken>*
SYsULexer::getTokenFactory()
{
  return mFactory;
}
