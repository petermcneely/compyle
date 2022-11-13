type token =
  | COMMA
  | COLON
  | NEWLINE
  | PERIOD
  | INDENT
  | DEDENT
  | DEF
  | RETURN
  | PRINT
  | ARROW
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | IF
  | ELIF
  | ELSE
  | WHILE
  | FOR
  | IN
  | BREAK
  | CONTINUE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULO
  | EXP
  | FDIVIDE
  | ASSIGN
  | PLUS_ASSIGN
  | MINUS_ASSIGN
  | TIMES_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | EXP_ASSIGN
  | FDIV_ASSIGN
  | EQ
  | NEQ
  | GT
  | LT
  | GEQ
  | LEQ
  | AND
  | OR
  | NOT
  | INT
  | BOOL
  | FLOAT
  | STRING
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | STRING_LITERAL of (string)
  | ID of (string)
  | BOOL_LITERAL of (bool)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
