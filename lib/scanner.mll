(* Ocamllex scanner for ComPyle *)

(* header section *)
{
  open Parser
  open Lexing
  open Lexing_stack
}

(*
 * declaration section
 * can be used to name frequently-occurring regular expressions
 *)
let digit = ['0'-'9']
let e = ['e' 'E']
let sign = ['+' '-']
let exponent = (e(sign)?digit+)
let float = (digit+('.'))|(digit+('.')digit+) | ((digit)+exponent)|(digit*('.')digit+exponent?)
let newline = ('\n'|"\r\n")
let whitespace = [' ' '\t']
let single_line_comment = '#' [^ '\r' '\n']*
let multi_line_comment = "\"\"\""
let multi_line_comment_block = "\"\"\"" _* "\"\"\""
let almost_ascii = [
  '!' '#' '$' '%' '&' '\'' '(' ')' '*' '+' ',' '-' '.'
  '/' '0'-'9' ':' ';' '<' '=' '>' '?' '@' 'A'-'Z' '['
  '\\' ']' '^' '_' '`' 'a'-'z' '{' '|' '}' '~' ' '
]
let lexeme = ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '-' '_']*

(*
  entrypoint (token):
      * takes an implicit argument lexbuf, a char string
      * takes an explicit argument manager, an indentation_manager
      * matches each regex with an action
      * longest and earliest matches

  From the Python LRM: The numbers pushed on the stack will always be strictly increasing from bottom to top.
  At the beginning of each logical line, the line’s indentation level is compared to the top of the stack.
  If it is equal, nothing happens. If it is larger, it is pushed on the stack, and one INDENT token is generated.
  If it is smaller, it must be one of the numbers occurring on the stack; all numbers on the stack that are larger
  are popped off, and for each number popped off a DEDENT token is generated. At the end of the file, a DEDENT token
  is generated for each number remaining on the stack that is larger than zero.

  We mimic that functionality here. Note though that if the line's indentation level is equal to the top of the stack,
  we do nothing _with the stack management_; instead, we pass through to scan the remainder of the line.
*)
rule token manager = parse
  (* an empty string will get evaluated every time *)
  "" {
    let top_of_stack = Stack.top manager.stack in
    let curr_indentation = manager.curr_indentation in
    if curr_indentation > top_of_stack then (
      Stack.push curr_indentation manager.stack;
      INDENT
    )
    else if curr_indentation < top_of_stack then (
      (* ignore the warning since calling this is intentional *)
      ignore (Stack.pop manager.stack);
      DEDENT
    )
    else (
      code manager lexbuf
    )
  }

and code manager = parse
| [' ']     { token manager lexbuf }
| (((whitespace* single_line_comment? multi_line_comment_block? newline)* whitespace* single_line_comment? multi_line_comment_block?) newline) {
  (*
    From the LRM: A logical line that contains only spaces, tabs, formfeeds and possibly a comment,
    is ignored (i.e., no NEWLINE token is generated).
    
    So, since there could be multiple, white space only lines
    in a row, we want to generate one token on the line that is not ignored and skip the rest.
  *)
  manager.curr_indentation <- 0;
  (* ignore the warning since calling this is intentional *)
  ignore (tab manager lexbuf);
  NEWLINE
}

| '('                                                       { LPAREN }
| ')'                                                       { RPAREN }
| '['                                                       { LBRACKET }
| ']'                                                       { RBRACKET }
| ':'                                                       { COLON }
| ','                                                       { COMMA }
| '.'                                                       { PERIOD }
| '+'                                                       { PLUS }
| '-'                                                       { MINUS }
| '*'                                                       { TIMES }
| '/'                                                       { DIVIDE }
| '%'                                                       { MODULO }
| '='                                                       { ASSIGN }
| "+="                                                      { PLUS_ASSIGN }
| "-="                                                      { MINUS_ASSIGN }
| "*="                                                      { TIMES_ASSIGN }
| "/="                                                      { DIV_ASSIGN }
| "%="                                                      { MOD_ASSIGN }
| "=="                                                      { EQ }
| "!="                                                      { NEQ }
| '<'                                                       { LT }
| "<="                                                      { LEQ }
| '>'                                                       { GT }
| ">="                                                      { GEQ }
| '<'                                                       { LT }
| "and"                                                     { AND }
| "or"                                                      { OR }
| "not"                                                     { NOT }
| "in"                                                      { IN }
| "if"                                                      { IF }
| "elif"                                                    { ELIF }
| "else"                                                    { ELSE }
| "for"                                                     { FOR }
| "while"                                                   { WHILE }
| "break"                                                   { BREAK }
| "def"                                                     { DEF }
| "->"                                                      { ARROW }
| "return"                                                  { RETURN }
| "True"                                                    { BOOL_LITERAL(true) }
| "False"                                                   { BOOL_LITERAL(false) }
| "tuple"                                                   { TUPLE }
| ("string" | "int" | "float" | "bool") ("[]")+ as a {


  let get_array_token (s: string) =
    match s with
      _ when String.starts_with ~prefix:"int" s -> INT_ARRAY(0)
    | _ when String.starts_with ~prefix:"string" s -> STRING_ARRAY(0)
    | _ when String.starts_with ~prefix:"float" s -> FLOAT_ARRAY(0)
    | _ when String.starts_with ~prefix:"bool" s -> BOOL_ARRAY(0)
    | _ -> raise (Failure "Scanning error. Scanning an unsupported array type")
  in
  
  get_array_token(a)
}
| "int"                                                     { INT }
| "bool"                                                    { BOOL }
| "float"                                                   { FLOAT }
| "string"                                                  { STRING }
| "None"                                                    { NONETYPE }
| lexeme as l                                               { ID(l) }
| digit+ as i                                               { INT_LITERAL(int_of_string i) } 
| float as f                                                { FLOAT_LITERAL(float_of_string f)}
| "\"" almost_ascii* "\"" as s                              { STRING_LITERAL(s) }
| eof                                                       { EOF }
| _ as ch                                                   { raise (Failure("illegal character " ^ Char.escaped ch)) }

and tab manager = parse
  ""     { (* do nothing; when we no longer have a tab, this will force us to go back to the newline token *) }
  | '\t' {
    manager.curr_indentation <- manager.curr_indentation + 1;
    tab manager lexbuf
  }