type typ = Int | Bool | Float | String | Tuple | Array of typ * int | EmptyArray
type aug_op = AAAdd | AASub | AAMult | AADiv | AAMod | AAExp | AAFDiv

type bin_op =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Exp
  | FDiv
  | And
  | Or
  | Eq
  | Neq
  | Gt
  | Lt
  | Geq
  | Leq

type expr =
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | ArrayLit of expr list
  | TupleLit of expr list
  | Binop of expr * bin_op * expr
  | Id of string
  | Asn of string * expr
  | AugAsn of string * aug_op * expr
  | Not of expr
  | In of expr * expr
  | NotIn of expr * expr
  | Call of string * expr list

type decl = string * typ

type stmt =
  | Break
  | Continue
  | Expr of expr
  | Function of string * decl list * typ * stmt list
  | Return of expr
  | If of expr * stmt list * stmt list
  | While of expr * stmt list
  | For of string * expr * stmt list
  | Print of expr
  | Decl of string * typ

type program = stmt list

(* Pretty-printing functions *)
let string_of_aug_op = function
  | AAAdd -> "+="
  | AASub -> "-="
  | AAMult -> "*="
  | AADiv -> "/="
  | AAMod -> "%="
  | AAExp -> "**="
  | AAFDiv -> "//="

let string_of_bin_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Exp -> "**"
  | FDiv -> "//"
  | And -> "and"
  | Or -> "or"
  | Eq -> "=="
  | Neq -> "!="
  | Gt -> ">"
  | Lt -> "<"
  | Geq -> ">="
  | Leq -> "<="

let rec string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Tuple -> "tuple"
  | EmptyArray -> "[]"
  | Array (t, dimensions) -> string_of_typ t ^ string_of_dimensions dimensions
and string_of_dimensions (dimensions: int): string =
  if dimensions = 0 then ""
  else
    "[]" ^ string_of_dimensions (dimensions - 1)

let string_of_decl (d : string * typ) : string =
  fst d ^ ": " ^ string_of_typ (snd d)

let string_of_formals f list =
  String.concat ", " (List.map (fun x -> string_of_decl x) f)

let rec string_of_expr = function
  | IntLit i -> string_of_int i
  | FloatLit f -> string_of_float f
  | StringLit s -> s
  | BoolLit b -> string_of_bool b
  | Id s -> s
  | ArrayLit l -> "[" ^ string_of_exprs l ^ "]"
  | TupleLit l -> "(" ^ string_of_exprs l ^ ")"
  | Binop (e1, op, e2) ->
      string_of_expr e1 ^ " " ^ string_of_bin_op op ^ " " ^ string_of_expr e2
  | Asn (v, e) -> v ^ " = " ^ string_of_expr e
  | AugAsn (v, op, e) -> v ^ " " ^ string_of_aug_op op ^ " " ^ string_of_expr e
  | Not e -> "not " ^ string_of_expr e
  | In (e1, e2) -> string_of_expr e1 ^ " in " ^ string_of_expr e2
  | NotIn (e1, e2) -> string_of_expr e1 ^ " not in " ^ string_of_expr e2
  | Call (fn, args) -> fn ^ "(" ^ string_of_exprs args ^ ")"

and string_of_exprs l =
  if List.length l = 1 then string_of_expr (List.hd l)
  else if List.length l > 1 then
    string_of_expr (List.hd l) ^ ", " ^ string_of_exprs (List.tl l)
  else ""

let rec string_of_stmt = function
  | Break -> "break\n"
  | Continue -> "continue\n"
  | Expr e -> string_of_expr e ^ "\n"
  | Function (fname, formals, return_type, block) ->
      "def " ^ fname ^ "("
      ^ string_of_formals formals formals
      ^ ") -> " ^ string_of_typ return_type ^ ":" ^ string_of_block block
  | Return e -> "return " ^ string_of_expr e ^ "\n"
  | If (e, if_block, elif_blocks) ->
      "if " ^ string_of_expr e ^ ":" ^ string_of_block if_block
      ^ string_of_elif_blocks elif_blocks
  | While (e, block) ->
      "while " ^ string_of_expr e ^ ":" ^ string_of_block block
  | For (v, e, b) ->
      "for " ^ v ^ " in " ^ string_of_expr e ^ ":" ^ string_of_block b
  | Print e -> "print(" ^ string_of_expr e ^ ")\n"
  | Decl (id, typ) -> string_of_decl (id, typ) ^ "\n"

and string_of_elif_blocks elif_blocks =
  String.concat "\n"
    (List.map (fun stmt -> "else:" ^ string_of_block [ stmt ]) elif_blocks)

and string_of_block (l : stmt list) : string =
  if List.length l > 0 then "\n" ^ String.concat "" (List.map string_of_stmt l)
  else ""

and string_of_line (l : stmt) : string = string_of_stmt l

let string_of_program (p : stmt list) : string =
  String.concat "" (List.map string_of_stmt p)

let pretty_string_of_program (p : stmt list) : string =
  "\n\nParsed Program: \n\n" ^ string_of_program p ^ "<EOF>\n"
