// I hate cpp lang
#include "SYsULexer.h" // 确保这里的头文件名与您生成的词法分析器匹配
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

// util functions
bool is_start_with(const std::string &m_str, const std::string &prefix) {
  return m_str.find(prefix) == 0;
}

std::vector<std::string> split(const std::string &m_str, char delimiter) {
  std::vector<std::string> tokens;
  std::string token;
  std::istringstream token_stream(m_str);

  while (std::getline(token_stream, token, delimiter)) {
    if (!token.empty()) {
      tokens.emplace_back(token);
    }
  }

  return tokens;
}

std::string first_str_between(const std::string &m_str,
                              const std::string &start, const std::string &end,
                              int index = 0) {
  if (start.empty() || end.empty())
    return "";

  if (index < 0 || index >= m_str.length())
    return "";

  auto start_index = index;

  // fetch valid start and end point of target string
  auto start_pos = m_str.find(start, start_index);
  auto end_pos = m_str.find(end, start_pos + start.length());

  if (start_pos == std::string::npos || end_pos == std::string::npos)
    return "";

  return m_str.substr(start_pos + start.length(),
                      end_pos - (start_pos + start.length()));
}

// use regex to determine whether num is a number
bool is_number(const std::string &num) {
  const std::regex pattern("^[+-]?\\d*(\\.\\d*)?$");
  return regex_match(num, pattern);
}

// 映射定义，将ANTLR的tokenTypeName映射到clang的格式
std::unordered_map<std::string, std::string> gtokenTypeMapping = {
    {"Int", "int"},
    {"Identifier", "identifier"},
    {"LeftParen", "l_paren"},
    {"RightParen", "r_paren"},
    {"RightBrace", "r_brace"},
    {"LeftBrace", "l_brace"},
    {"LeftBracket", "l_square"},
    {"RightBracket", "r_square"},
    {"Constant", "numeric_constant"},
    {"Return", "return"},
    {"Semi", "semi"},
    {"EOF", "eof"},
    {"Assign", "assign"},
    {"Plus", "plus"},
    {"Comma", "comma"},
    {"Minus", "minus"},
    {"Multiply", "star"},
    {"Divide", "slash"},
    {"Modulus", "percent"},
    {"Increment", "plusplus"},
    {"Decrement", "minusminus"},
    {"BitwiseAnd", "amp"},
    {"BitwiseOr", "pipe"},
    {"BitwiseXor", "caret"},
    {"BitwiseNot", "tilde"},
    {"LogicalAnd", "ampamp"},
    {"LogicalOr", "pipepipe"},
    {"LogicalNot", "exclaim"},
    {"EqualEqual", "equalequal"},
    {"Unequal", "exclaimequal"},
    {"Greater", "greater"},
    {"Less", "less"},
    {"GreaterEqual", "greaterequal"},
    {"LessEqual", "lessequal"},
    {"PlusEqual", "plusequal"},
    {"MinusEqual", "minusequal"},
    {"MultiplyEqual", "starequal"},
    {"DivideEqual", "slashequal"},
    {"ModulusEqual", "percentequal"},
    {"LeftShift", "leftshift"},
    {"RightShift", "rightshift"},
    {"LeftShiftEqual", "leftshiftequal"},
    {"RightShiftEqual", "rightshiftequal"},
    {"Dot", "dot"},
    {"Arrow", "arrow"},
    {"Question", "question"},
    {"Colon", "colon"},
    {"Const", "const"},
    {"If", "if"},
    {"Else", "else"},
    {"While", "while"},
    {"For", "for"},
    {"Do", "do"},
    {"Switch", "switch"},
    {"Case", "case"},
    {"Default", "default"},
    {"Break", "break"},
    {"Continue", "continue"},
    {"Char", "char"},
    {"Short", "short"},
    {"Long", "long"},
    {"Float", "float"},
    {"Double", "double"},
    {"Void", "void"},
};

void print_token(const antlr4::Token *token,
                 const antlr4::CommonTokenStream &tokens,
                 std::ofstream &out_file, const antlr4::Lexer &lexer,
                 const std::unordered_map<int, std::string>
                     &line_filename_map, // 行号到文件名的映射表
                 const antlr4::Token *previous_token, // 指向上一个token的指针
                 const std::unordered_map<int, int>
                     &line_translate_map // 行号和实际行号的映射表
) {
  auto &vocabulary = lexer.getVocabulary();

  auto token_type_name =
      std::string(vocabulary.getSymbolicName(token->getType()));

  if (token_type_name.empty())
    token_type_name = "<UNKNOWN>"; // 处理可能的空字符串情况

  if (gtokenTypeMapping.find(token_type_name) != gtokenTypeMapping.end()) {
    token_type_name = gtokenTypeMapping[token_type_name];
  }
  // 计算locInfo，之类的行号需要减去前面预处理指令所占的行号
  std::string loc_info =
      " Loc=<" + line_filename_map.at(token->getLine()) + ":" +
      std::to_string(line_translate_map.at(token->getLine())) + ":" +
      std::to_string(token->getCharPositionInLine() + 1) + ">";

  bool start_of_line{
      !previous_token ||
      previous_token->getLine() !=
          token
              ->getLine()}; // 是第一个token或者上一个token和当前token的行号不一样
  bool leading_space{
      (start_of_line && token->getCharPositionInLine() != 0) ||
      (!start_of_line &&
       ((previous_token->getCharPositionInLine() +
         (previous_token->getText()).length()) !=
        token
            ->getCharPositionInLine()))}; // 是行的开头，并且列号不等于0 或者
                                          // 不是行的开头，并且和上一个token之间有空格

  if (token->getText() != "<EOF>")
    out_file << token_type_name << " '" << token->getText() << "'";
  else
    out_file << token_type_name << " '" << "'";
  if (start_of_line)
    out_file << "\t [StartOfLine]";
  if (leading_space)
    out_file << " [LeadingSpace]";
  out_file << loc_info << std::endl;
}

// 得到行号->文件名映射表
std::unordered_map<int, std::string> get_line_filename_map(std::ifstream &in_file) {
  // 检查文件是否打开
  if (!in_file.is_open()) {
    throw std::logic_error{"can't open the file!"};
  }

  std::string line;
  std::unordered_map<int, std::string> line_filename_map;
  int current_line_number{};
  std::string filename{"None"};
  // 正则表达式模式，用于匹配任意路径的 .c 文件
  std::regex pattern{R"QwQ(/.*\.(h|c))QwQ"};

  while (std::getline(in_file, line)) {
    current_line_number++;
    if (is_start_with(line, "#")) // 判断预处理行
    {
      // 用于存储匹配结果
      std::smatch matches;
      // 搜索字符串
      if (std::regex_search(line, matches, pattern)) {
        // 如果找到匹配，该字符串就是文件名，更改文件名
        filename = matches[0];
      }
    }
    line_filename_map[current_line_number] = filename;
  }
  line_filename_map[current_line_number + 1] = filename; // 处理'EOF'行
  in_file.clear();
  in_file.seekg(0);
  return line_filename_map;
}

std::unordered_map<int, int> get_line_translate_map(std::ifstream &in_file) {
  // 检查文件是否打开
  if (!in_file.is_open()) {
    throw std::logic_error{"can't open the file!"};
  }

  std::string line;
  std::unordered_map<int, int> line_translate_map;
  int current_line_number{};
  int actual_line_number{};
  while (std::getline(in_file, line)) {
    current_line_number++;
    if (is_start_with(line, "#")) // 判断预处理行
    {
      auto line_splited{split(line, ' ')};
      if (line_splited.size() > 1 &&
          is_number(line_splited[1])) // 如果格式是类似于# 1
                                      // ....的，就取这个数字-1作为行号
      {
        actual_line_number = std::stoi(line_splited[1]) - 1;
      } else {
        actual_line_number++; // 否则就把行号+1
      }
    } else // 非预处理行
    {
      actual_line_number++;
    }
    line_translate_map[current_line_number] = actual_line_number;
  }
  line_translate_map[current_line_number + 1] =
      actual_line_number + 1; // 补充上'EOF'的那行
  in_file.clear();
  in_file.seekg(0);
  return line_translate_map;
}

int main(int argc, char *argv[]) {
  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input> <output>\n";
    return -1;
  }

  std::ifstream in_file(argv[1]);
  if (!in_file) {
    std::cout << "Error: unable to open input file: " << argv[1] << '\n';
    return -2;
  }

  std::ofstream out_file(argv[2]);
  if (!out_file) {
    std::cout << "Error: unable to open output file: " << argv[2] << '\n';
    return -3;
  }

  auto origin_file_name{get_line_filename_map(in_file)};
  auto line_translate_map{get_line_translate_map(in_file)};

  std::cout << "程序 '" << argv[0] << std::endl;
  std::cout << "输入 '" << argv[1] << std::endl;
  std::cout << "输出 '" << argv[2] << std::endl;

  antlr4::ANTLRInputStream input(in_file);
  SYsULexer lexer(&input);

  antlr4::CommonTokenStream tokens(&lexer);
  tokens.fill();

  const antlr4::Token *previous_token{};
  for (auto &&token : tokens.getTokens()) {
    print_token(token, tokens, out_file, lexer, origin_file_name, previous_token,
                line_translate_map);
    previous_token = token;
  }
}