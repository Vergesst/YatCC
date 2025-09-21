lexer grammar SYsULexer;

// keywords
Return : 'return';
Const: 'const';

// types
Int : 'int';
Char: 'char';
Short: 'short';
Float: 'float';
Double: 'double';
Void: 'void';

// control statement
If: 'if';
While: 'while';
For: 'for';
Else: 'else';
Do: 'do';
Switch: 'switch';
Case: 'case';
Default: 'default';
Break: 'break';
Continue: 'continue';
Goto: 'goto';

LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';
LeftBrace : '{';
RightBrace : '}';

// operators
Plus : '+';
Minus: '-';
Multiply: '*';
Divide: '/';
Modulus: '%';
Increment: '++';
Decrement: '--';
BitwiseAnd: '&';
BitwiseOr: '|';
BitwiseXor: '^';
BitwiseNot: '~';
LogicalAnd: '&&';
LogicalOr: '||';
LogicalNot: '!';
Equal: '==';
Uneuqal: '!=';
Greater: '>';
Less: '<';
GreaterEqual: '>=';
LessEqual: '<=';
PlusEqual: '+=';
MinusEqual: '-=';
MultiplyEqual: '*=';
DivideEqual: '/=';
ModulusEqual: '%=';
RightShift: '>>';
LeftShift: '<<';
RightShiftEqual: '>>=';
LeftShiftEqual: '<<=';
Dot: '.';
Arrow: '->';
Question: '?';
Colon: ':';
Semi: ';';
Comma: ',';

// ?
Assign: '=';

Identifier: IdentifierNondigit ( IdentifierNondigit | Digit)*;

fragment IdentifierNondigit: Nondigit;

fragment Nondigit: [a-zA-Z_];

fragment Digit: [0-9];

Constant: IntegerConstant;

fragment IntegerConstant:
	HexadecimalConstant
	| DecimalConstant
	| OctalConstant;

fragment HexadecimalConstant: '0' [xX] HexDigit+;

fragment HexDigit: [0-9a-fA-F];

fragment DecimalConstant: '0' | NonzeroDigit Digit*;

fragment OctalConstant: '0' OctalDigit+;

fragment NonzeroDigit: [1-9];

fragment OctalDigit: [0-7];

// 预处理信息处理，可以从预处理信息中获得文件名以及行号 预处理信息中的第一个数字即为行号
LineAfterPreprocessing: '#' Whitespace* ~[\r\n]* -> skip;

Whitespace: [ \t]+ -> skip;

// 换行符号，可以利用这个信息来更新行号
Newline: ( '\r' '\n'? | '\n') -> skip;

