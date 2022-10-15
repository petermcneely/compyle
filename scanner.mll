(* Ocamllex scanner for NanoC *)

(* header section *)
{ open Parser }

(*
 * declaration section
 * can be used to name frequently-occurring regular expressions
 *)
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

(*
  entrypoint (token):
    * takes an implicit argument lexbuf, a char string
    * matches each regex with an action
    * longest and earliest matches
*)
rule token = parse
  [' ']       { token lexbuf } (* Whitespace *)
| ['\r' '\n'] { NEWLINE }
| '\t'        { INDENT }
| "\"\"\""    { multiline_comment lexbuf }           (* Comments *)
| '#'         { comment lexbuf }
| '('         { LPAREN }
| ')'         { RPAREN }
| '{'         { LBRACE }
| '}'         { RBRACE }
| ':'         { COLON }
| ';'         { SEMI }
| ','         { COMMA }
| '+'         { PLUS }
| '-'         { MINUS }
| '*'         { MULT }
| '/'         { DIV }
| '%'         { MOD }
| '='         { ASSIGN }
| "=="        { EQ }
| "!="        { NEQ }
| '<'         { LT }
| "and"       { AND }
| "or"        { OR }
| "if"        { IF }
| "else"      { ELSE }
| "for"       { FOR }
| "in"        { IN }
| "range"     { RANGE }
| "break"     { BREAK }
| "continue"  { CONTINUE }
| "while"     { WHILE }
| "return"    { RETURN }
| "True"      { BLIT(true)  }
| "False"     { BLIT(false) }
| digit+ as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and multiline_comment = parse
  "\"\"\"" { token lexbuf }
| _        { multiline_comment lexbuf }

and comment = parse
  ['\r' '\n'] { token lexbuf }
| _            { comment lexbuf }
