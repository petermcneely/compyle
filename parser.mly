/* Ocamlyacc parser for ComPyle */

%{
open Ast
%}

%token COMMA COLON NEWLINE PERIOD
%token INDENT DEDENT
%token DEF RETURN PRINT ARROW
%token LPAREN RPAREN LBRACKET RBRACKET
%token IF ELIF ELSE WHILE FOR IN
%token BREAK CONTINUE
%token PLUS MINUS TIMES DIVIDE MODULO EXP FDIVIDE
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN EXP_ASSIGN FDIV_ASSIGN
%token EQ NEQ GT LT GEQ LEQ
%token AND OR NOT
%token INT BOOL FLOAT STRING
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token <bool> BOOL_LITERAL
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN EXP_ASSIGN FDIV_ASSIGN
%left OR
%left AND
%left NOT
%left IN
%left EQ NEQ
%left LT GT
%left GEQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE FDIVIDE
%left MODULO
%left EXP

%%
program: stmt_list EOF { $1 }

stmt_list:
	| { [] }
	| stmt stmt_list { $1 :: $2 }
	| NEWLINE stmt stmt_list { $2 :: $3 }

stmt:
	expr NEWLINE { Expr($1) }
	| function_stmt { $1 }
	| return_stmt NEWLINE { $1 }
	| print_stmt NEWLINE { $1 }
	| if_stmt { $1 }
	| while_stmt { $1 }
	| for_stmt { $1 }
	| BREAK NEWLINE { Break }
	| CONTINUE NEWLINE { Continue }

block:
	NEWLINE INDENT stmt_list DEDENT { $3 }

function_stmt:
	DEF ID LPAREN formals_opt RPAREN ARROW typ COLON block { Function($2, $4, $7, $9) }

vdecl:
	ID COLON typ { ($1, $3) }

formals_opt:
	/* nothing */ { [] }
	| formals_list { $1 }

formals_list:
	| vdecl { [$1] }
	| vdecl COMMA formals_list { $1 :: $3 }

return_stmt: RETURN expr { Return($2) }

if_stmt:
	IF expr COLON block { If($2, $4, []) }

// elif_stmts:
// 	indented_elif_stmts { $1 }
// 	| unindented_elif_stmts { $1 }

// indented_elif_stmt:
// 	indents ELIF expr COLON block { ($1, $3, $5) }

// unindented_elif_stmt:
// 	ELIF expr COLON block { ([], $2, $4) }

// indented_elif_stmts:
// 	NEWLINE indented_elif_stmt { [$2] }
// 	| NEWLINE indented_elif_stmt indented_elif_stmts { $2 :: $3 }

// unindented_elif_stmts:
// 	NEWLINE unindented_elif_stmt { [$2] }
// 	| NEWLINE unindented_elif_stmt unindented_elif_stmts { $2 :: $3 }

while_stmt:
	WHILE expr COLON block { While($2, $4) }

for_stmt:
	FOR ID IN expr COLON block { For($2, $4, $6) }

print_stmt:
	PRINT LPAREN expr RPAREN { Print($3) }

expr_opt:
	/* nothing */ { [] }
	| expr_list { $1 }

expr_list:
	| expr { [$1] }
	| expr COMMA expr_list { $1 :: $3 }

expr:
	INT_LITERAL   { IntLit($1) }
	| FLOAT_LITERAL { FloatLit($1) }
	| STRING_LITERAL { StringLit($1) }
	| BOOL_LITERAL { BoolLit($1) }
	| LBRACKET expr_opt RBRACKET { ArrayLit($2) }
	| LPAREN expr_opt RPAREN { TupleLit($2) }
	| expr PLUS expr         { Binop($1, Add, $3)  }
	| expr MINUS expr        { Binop($1, Sub, $3)  }
	| expr TIMES expr        { Binop($1, Mult, $3) }
	| expr DIVIDE expr       { Binop($1, Div, $3)  }
	| expr MODULO expr       { Binop($1, Mod, $3)  }
	| expr EXP expr          { Binop($1, Exp, $3)  }
	| expr FDIVIDE expr      { Binop($1, FDiv, $3) }
	| ID { Id($1) }
	| ID COLON typ ASSIGN expr { DeclAsn($1, $3, $5) }
	| ID ASSIGN expr { Asn($1, $3) }
	| ID PLUS_ASSIGN expr   { AugAsn($1, AAAdd, $3) }
	| ID MINUS_ASSIGN expr  { AugAsn($1, AASub, $3)  }
	| ID TIMES_ASSIGN expr  { AugAsn($1, AAMult, $3) }
	| ID DIV_ASSIGN expr    { AugAsn($1, AADiv, $3)  }
	| ID MOD_ASSIGN expr    { AugAsn($1, AAMod, $3)  }
	| ID EXP_ASSIGN expr    { AugAsn($1, AAExp, $3)  }
	| ID FDIV_ASSIGN expr   { AugAsn($1, AAFDiv, $3) }
	| ID LPAREN args_opt RPAREN { Call($1, $3) }
	| expr EQ expr     { Binop($1, Eq, $3)  }
	| expr NEQ expr    { Binop($1, Neq, $3) }
	| expr GT expr     { Binop($1, Gt, $3)  }
	| expr LT expr     { Binop($1, Lt, $3)  }
	| expr GEQ expr    { Binop($1, Geq, $3) }
	| expr LEQ expr    { Binop($1, Leq, $3) }
	| expr AND expr   { Binop($1, And, $3) }
	| expr OR expr    { Binop($1, Or, $3)  }
	| NOT expr        { Not($2)      }
	| expr IN expr { In($1, $3) }
	| expr NOT IN expr { NotIn($1, $4) }

typ:
	INT { Int }
	| BOOL { Bool }
	| FLOAT { Float }
	| STRING { String }

args_opt:
  /* nothing */ { [] }
  | args_list { $1 }

args_list:
  expr { [$1] }
  | expr COMMA args_list { $1 :: $3 }