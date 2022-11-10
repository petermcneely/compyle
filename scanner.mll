(* Ocamllex scanner for ComPyle *)

(* header section *)
{ open Parser }

(*
 * declaration section
 * can be used to name frequently-occurring regular expressions
 *)
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let e = ['e' 'E']
let sign = ['+' '-']
let exponent = (e(sign)?digit+)
let float = (digit+('.'))|(digit+('.')digit+)
						 |((digit)exponent)|(digit*('.')digit+exponent?)


(*
  entrypoint (token):
    * takes an implicit argument lexbuf, a char string
    * matches each regex with an action
    * longest and earliest matches
*)
rule token = parse
| [' ' '\r'] { token lexbuf }
| '\t' { INDENT }
| "\"\"\""    { multi_comment lexbuf }
| '#'   { single_comment lexbuf }
| '\n'  { NEWLINE }
(* delimiters *)
| '('   { LPAREN }
| ')'   { RPAREN }
| '['   { LBRACKET }
| ']'   { RBRACKET }
| ':'   { COLON }
| ','   { COMMA }
| '.'   { PERIOD }
(* arithmetic operators *)
| '+'   { PLUS }
| '-'   { MINUS }
| '*'   { TIMES }
| '/'   { DIVIDE }
| '%'   { MODULO }
| "**"  { EXP }
| "//"  { FDIVIDE }
(* assignment operators *)
| '='   { ASSIGN }
| "+="  { PLUS_ASSIGN }
| "-="  { MINUS_ASSIGN }
| "*="  { TIMES_ASSIGN }
| "/="  { DIV_ASSIGN }
| "%="  { MOD_ASSIGN }
| "**=" { EXP_ASSIGN }
| "//=" { FDIV_ASSIGN }
(* comparison operators *)
| "=="  { EQ }
| "!="  { NEQ }
| '<'   { LT }
| "<="  { LEQ }
| '>'   { GT }
| ">="  { GEQ }
| '<'   { LT }
(* logical operators *)
| "and"      { AND }
| "or"       { OR }
| "not"      { NOT }
(* membership operators *)
| "in"       { IN }
(* conditional flow *)
| "if"       { IF }
| "elif"     { ELIF }
| "else"     { ELSE }
| "for"      { FOR }
| "while"    { WHILE }
| "continue" { CONTINUE }
| "break"    { BREAK }
(* function *)
| "def"      { DEF }
| "return"   { RETURN }
(* Primitive Types *)
| "True" { BOOL_LITERAL(true) }
| "False" { BOOL_LITERAL(false) }
| (alpha | '_') (alpha digit | '-' | '_')* as lexeme { ID(lexeme) }
| digit+ as i { INT_LITERAL(int_of_string i) } 
| float as f { FLOAT_LITERAL(float_of_string f)}
| (("\'.*\'") | ("\".*\"")) as s { STRING_LITERAL(s) }
| eof { EOF }
| _ as ch { raise (Failure("illegal character " ^ Char.escaped ch)) }

and multi_comment = parse
| "\"\"\""   { token lexbuf }
| _        { multi_comment lexbuf }

and single_comment = parse
| "\n"     { token lexbuf }
| _        { single_comment lexbuf }