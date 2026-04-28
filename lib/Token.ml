type token_type = 
	| LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE
	| COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR
	| BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL
	| GREATER | GREATER_EQUAL | LESS | LESS_EQUAL
	| IDENTIFIER | STRING | NUMBER
	| AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR
	| PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE
	| EOF

type literal =
	| StringLit of string
	| NumberLit of float
	| NoneLit

let literal_to_string = function 
	| StringLit str -> str
	| NumberLit num -> string_of_float num
	| NoneLit -> "None"
	
let token_type_to_string = function
	| LEFT_PAREN -> "LEFT_PAREN"
	| RIGHT_PAREN -> "RIGHT_PAREN"
	| LEFT_BRACE -> "LEFT_BRACE"
	| RIGHT_BRACE -> "RIGHT_BRACE"
	| COMMA -> "COMMA"
	| DOT -> "DOT"
	| MINUS -> "MINUS"
	| PLUS -> "PLUS"
	| SEMICOLON -> "SEMICOLON"
	| SLASH -> "SLASH"
	| STAR -> "STAR"
	| BANG -> "BANG"
	| BANG_EQUAL -> "BANG_EQUAL"
	| EQUAL -> "EQUAL"
	| EQUAL_EQUAL -> "EQUAL_EQUAL"
	| GREATER -> "GREATER"
	| GREATER_EQUAL -> "GREATER_EQUAL"
	| LESS -> "LESS"
	| LESS_EQUAL -> "LESS_EQUAL"
	| IDENTIFIER -> "IDENTIFIER"
	| STRING -> "STRING"
	| NUMBER -> "NUMBER"
	| AND -> "AND"
	| CLASS -> "CLASS"
	| ELSE -> "ELSE"
	| FALSE -> "FALSE"
	| FUN -> "FUN"
	| FOR -> "FOR"
	| IF -> "IF"
	| NIL -> "NIL"
	| OR -> "OR"
	| PRINT -> "PRINT"
	| RETURN -> "RETURN"
	| SUPER -> "SUPER"
	| THIS -> "THIS"
	| TRUE -> "TRUE"
	| VAR -> "VAR"
	| WHILE -> "WHILE"
	| EOF -> "EOF"

type token = {
	token_type : token_type;
	lexeme : string;
	literal : literal;
	line : int;
}

let to_string token = 
	let type_str = token_type_to_string token.token_type in
		type_str ^ " " ^ token.lexeme ^ " " ^ string_of_int token.line

