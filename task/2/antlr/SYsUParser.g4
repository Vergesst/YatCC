parser grammar SYsUParser;

options {
	tokenVocab = SYsULexer;
}

primaryExpression: Identifier | Constant;

postfixExpression: primaryExpression;

unaryExpression:
	(
		postfixExpression // a / indentifier
		| unaryOperator unaryExpression // +a
		| bracedExpression // (...)
		| indexExpression //a[...][...]
		| functionCallExpression //func()
	);

unaryOperator: Plus | Minus | LogicalNot;

bracedExpression:
	LeftParen (
	 logicalExpression
	) RightParen;

indexExpression:
	Identifier (LeftBracket logicalExpression RightBracket)+;

functionCallExpression:
	Identifier LeftParen logicalExpression? (Comma logicalExpression)* RightParen ;  // func(a,b)

relationExpression:
	additiveExpression ((EqualEqual | NotEqual | Greater | Less | GreaterEqual | LessEqual) additiveExpression)*;

logicalExpression:
	logicalAndExpression (LogicalOr logicalAndExpression)*
	;

logicalAndExpression:
	relationExpression (LogicalAnd relationExpression)*
	;

additiveExpression:
	multiplicativeExpression (
		(Plus | Minus) multiplicativeExpression
	)*;

multiplicativeExpression:
	unaryExpression ((Star | Div | Mod) unaryExpression)*;

assignmentExpression:
	additiveExpression
	| unaryExpression Equal assignmentExpression;

expression: assignmentExpression (Comma assignmentExpression)*;

declaration: declarationSpecifiers initDeclaratorList? Semi; // 声明 int a = {4}, b = {5};

declarationSpecifiers: declarationSpecifier+;

declarationSpecifier: typeSpecifier;

initDeclaratorList: initDeclarator (Comma initDeclarator)*; // a = {4}, b = {5}

initDeclarator: declarator (Equal initializer)?;  // 初始化声明，a = {4}

typeSpecifier: Int | Const | Char | Void | Long | LongLong;

declarator: directDeclarator;

directDeclarator:
	Identifier
	| directDeclarator LeftBracket assignmentExpression? RightBracket;

identifierList: Identifier (Comma Identifier)*;

initializer:
	assignmentExpression
	| LeftBrace initializerList? Comma? RightBrace;

initializerList
	: initializer (Comma initializer)*; // :   designation? initializer (Comma designation? initializer)*

statement:
	compoundStatement
	| expressionStatement
	| jumpStatement
	| ifElseStatement
	| whileStatement
	| breakStatement
	| continueStatement
	;

compoundStatement: LeftBrace blockItemList? RightBrace;

blockItemList: blockItem+;

blockItem: statement | declaration;

expressionStatement: expression? Semi;

jumpStatement: (Return expression?) Semi;

compilationUnit: translationUnit? EOF;

translationUnit: externalDeclaration+;

externalDeclaration: functionDeclaration | declaration;

functionDeclaration:
	declarationSpecifiers directDeclarator LeftParen parameterList? RightParen (compoundStatement | Semi); // int func(int a){...}或int func(int a);

parameterList:
	declarationSpecifiers initDeclarator (Comma declarationSpecifiers initDeclarator)*; // int a ={4},int b={4}

ifElseStatement:
	If LeftParen logicalExpression RightParen statement (Else statement)?;

whileStatement:
	While LeftParen logicalExpression RightParen statement;

breakStatement:
	Break Semi;

continueStatement:
	Continue Semi;