(* Semantically checked AST and functions for printing it*)

open Ast

type sexpr = typ * sx

and sx =
  | SIntLit of int
  | SFloatLit of float
  | SStringLit of string
  | SBoolLit of bool
  | SArrayLit of sexpr list
  | STupleLit of sexpr list
  | SBinop of sexpr * bin_op * sexpr
  | SId of string
  | SAsn of string * sexpr
  | SAugAsn of string * aug_op * sexpr
  | SNot of sexpr
  | SIn of sexpr * sexpr
  | SNotIn of sexpr * sexpr
  | SCall of string * sexpr list

type sstmt =
  | SBreak
  | SContinue
  | SExpr of sexpr
  | SFunction of string * decl list * typ * sstmt list
  | SReturn of sexpr
  | SIf of sexpr * sstmt list * sstmt list
  | SWhile of sexpr * sstmt list
  | SFor of string * sexpr * sstmt list
  | SPrint of sexpr
  | SDecl of string * typ * sexpr option

type sprogram = sstmt list

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : "
  ^ (match e with
    | SIntLit i -> string_of_int i
    | SFloatLit f -> string_of_float f
    | SStringLit s -> s
    | SBoolLit b -> string_of_bool b
    | SId s -> s
    | SArrayLit l -> "[" ^ string_of_sexprs l ^ "]"
    | STupleLit l -> "(" ^ string_of_sexprs l ^ ")"
    | SBinop (e1, op, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_bin_op op ^ " "
        ^ string_of_sexpr e2
    | SAsn (v, e) -> v ^ " = " ^ string_of_sexpr e
    | SAugAsn (v, op, e) ->
        v ^ " " ^ string_of_aug_op op ^ " " ^ string_of_sexpr e
    | SNot e -> "not " ^ string_of_sexpr e
    | SIn (e1, e2) -> string_of_sexpr e1 ^ " in " ^ string_of_sexpr e2
    | SNotIn (e1, e2) -> string_of_sexpr e1 ^ " not in " ^ string_of_sexpr e2
    | SCall (fn, args) -> fn ^ "(" ^ string_of_sexprs args ^ ")")
  ^ ")"

and string_of_sexprs l =
  if List.length l = 1 then string_of_sexpr (List.hd l)
  else if List.length l > 1 then
    string_of_sexpr (List.hd l) ^ ", " ^ string_of_sexprs (List.tl l)
  else ""

let string_of_sexpr_opt = function
  | None -> ""
  | Some se -> string_of_sexpr se

let rec string_of_sstmt = function
  | SBreak -> "break\n"
  | SContinue -> "continue\n"
  | SExpr e -> string_of_sexpr e ^ "\n"
  | SFunction (fname, formals, return_type, block) ->
      "def " ^ fname ^ "("
      ^ string_of_formals formals
      ^ ") -> " ^ string_of_typ return_type ^ ":" ^ string_of_sblock block
  | SReturn e -> "return " ^ string_of_sexpr e ^ "\n"
  | SIf (e, if_block, elif_blocks) ->
      "if " ^ string_of_sexpr e ^ ":" ^ string_of_sblock if_block
      ^ string_of_selif_blocks elif_blocks
  | SWhile (e, block) ->
      "while " ^ string_of_sexpr e ^ ":" ^ string_of_sblock block
  | SFor (v, e, b) ->
      "for " ^ v ^ " in " ^ string_of_sexpr e ^ ":" ^ string_of_sblock b
  | SPrint e -> "print(" ^ string_of_sexpr e ^ ")\n"
  | SDecl (id, typ, sexpr_opt) -> string_of_decl (id, typ) ^ string_of_sexpr_opt sexpr_opt ^ "\n"

and string_of_selif_blocks elif_blocks =
  String.concat "\n"
    (List.map (fun sstmt -> "else:" ^ string_of_sblock [ sstmt ]) elif_blocks)

and string_of_sblock (l : sstmt list) : string =
  if List.length l > 0 then "\n" ^ String.concat "" (List.map string_of_sstmt l)
  else ""

and string_of_sline (l : sstmt) : string = string_of_sstmt l

let string_of_sprogram (p : sstmt list) : string =
  String.concat "" (List.map string_of_sstmt p)

let pretty_string_of_sprogram (p : sstmt list) : string =
  "\n\nParsed Program: \n\n" ^ string_of_sprogram p ^ "<EOF>\n"