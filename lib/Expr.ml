type expr = 
	| Binary of expr * Token.token * expr
	| Grouping of expr
	| Literal of Token.literal
	| Unary of Token.token * expr

(*functional languages are just better for compilers hence*)

(*new methods which act on Expr can be added easily*)
let rec print_ast = function 
	| Binary (left, op, right) -> "( " ^ op.lexeme ^ " " ^ (print_ast left) ^ " " ^ (print_ast right) ^ ")"
	| Grouping expr -> "(group " ^ (print_ast expr) ^ ")"
	| Literal lit -> Token.literal_to_string lit
	| Unary (op, right) -> "(" ^ op.lexeme ^ " " ^ (print_ast right) ^ ")"

