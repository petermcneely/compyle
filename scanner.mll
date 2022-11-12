(* Ocamllex scanner for ComPyle *)

(* header section *)
{
  open Parser
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
let float = (digit+('.'))|(digit+('.')digit+) | ((digit)exponent)|(digit*('.')digit+exponent?)

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
    let top = Stack.top manager.stack in
    print_string "stack top: "; print_int top; print_newline();
    let top_of_stack = Stack.top manager.stack in
    let curr_indentation = manager.curr_indentation in
    if curr_indentation > top_of_stack then (
      print_string "indenting\n";
      Stack.push curr_indentation manager.stack;
      INDENT
    )
    else if curr_indentation < top_of_stack then (
      print_string "dedenting\n";
      (* ignore the warning since calling this is intentional *)
      ignore (Stack.pop manager.stack);
      DEDENT
    )
    else (
      print_string "jumping into code\n";
      code manager lexbuf
    )
  }

and code manager = parse
| [' ']     { token manager lexbuf }
| "\"\"\""  { print_string "multi line comment!\n"; multi_comment manager lexbuf }
| '#'       { single_comment manager lexbuf }

| ('\n'|"\r\n")  {
  print_string "newline"; print_newline();
  manager.curr_indentation <- 0;
  (* ignore the warning since calling this is intentional *)
  ignore (tab manager lexbuf);
  print_string "tokenizing the newline\n";
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
| "**"                                                      { EXP }
| "//"                                                      { FDIVIDE }
| '='                                                       { ASSIGN }
| "+="                                                      { PLUS_ASSIGN }
| "-="                                                      { MINUS_ASSIGN }
| "*="                                                      { TIMES_ASSIGN }
| "/="                                                      { DIV_ASSIGN }
| "%="                                                      { MOD_ASSIGN }
| "**="                                                     { EXP_ASSIGN }
| "//="                                                     { FDIV_ASSIGN }
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
| "continue"                                                { CONTINUE }
| "break"                                                   { BREAK }
| "def"                                                     { DEF }
| "return"                                                  { print_string "return\n"; RETURN }
| "print"                                                   { PRINT }
| "True"                                                    { BOOL_LITERAL(true) }
| "False"                                                   { BOOL_LITERAL(false) }
| ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '-' '_']* as lexeme { ID(lexeme) }
| digit+ as i                                               { INT_LITERAL(int_of_string i) } 
| float as f                                                { FLOAT_LITERAL(float_of_string f)}
| (("\'.*\'") | ("\".*\"")) as s                            { STRING_LITERAL(s) }
| eof                                                       { EOF }
| _ as ch                                                   { raise (Failure("illegal character " ^ Char.escaped ch)) }

and tab manager = parse
  ""     { print_string "done tabbing\n"; }
  | '\t' {
    print_string "tabbing\n";
    manager.curr_indentation <- manager.curr_indentation + 1;
    tab manager lexbuf
  }

and multi_comment manager = parse
| "\"\"\"" {print_string "ending multi lin comment\n"; token manager lexbuf }
| _        { multi_comment manager lexbuf }

and single_comment manager = parse
| "\n" { token manager lexbuf }
| _    { single_comment manager lexbuf }