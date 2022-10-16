/* Ocamlyacc parser for NanoC */

%{
open Ast
%}

/* these token names do not matter, but they just need to line up with the expectation of how it gets used */
%token SEMI COLON LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS MULT DIV MOD ASSIGN
%token EQ NEQ LT AND OR NOT
%token IF ELSE WHILE INT BOOL
%token RETURN COMMA
%token INDENT NEWLINE
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF

%start program
%type <Ast.tokenseq> program

%%

program:
  tokens EOF { $1}

tokens:
   /* nothing */ { [] }
 | one_token tokens { $1 :: $2 }

one_token:
  | INDENT { "INDENT" }
  | NEWLINE { "NEWLINE" }
  | SEMI  {  "SEMI" }
  | COLON { "COLON" }
  | LPAREN { "LPAREN" }
  | RPAREN { "RPAREN" }
  | LBRACE { "LBRACE" }
  | RBRACE { "RBRACE" }
  | COMMA { "COMMA" }
  | PLUS { "PLUS" }
  | MINUS { "MINUS" }
  | MULT { "MULT" }
  | DIV { "DIV" }
  | MOD { "MOD" }
  | ASSIGN { "ASSIGN" }
  | EQ { "EQ" }
  | NEQ { "NEQ" }
  | LT { "LT" }
  | AND { "AND" }
  | OR { "OR" }
  | NOT { "NOT" }
  | IF { "IF" }
  | ELSE { "ELSE" }
  | WHILE { "WHILE" }
  | RETURN { "RETURN" }
  | INT { "INT" }
  | BOOL { "BOOL" }
  | BLIT { "BOOL: " ^ string_of_bool $1 }
  | LITERAL { "LITERAL: " ^ string_of_int $1 }
  | ID { "ID: " ^ $1 }
