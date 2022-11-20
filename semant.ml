open Ast
open Sast
module StringMap = Map.Make (String)

(* Added defs below from ast.ml for VSCode syntax checking *)
type typ = Int | Bool | Float | String | Tuple | Array of typ
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
(* Added defs above from ast.ml for VSCode syntax checking *)

(* Added defs below from sast.ml for VSCode syntax checking *)
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
  | SDecl of string * typ

type sprogram = sstmt list
(* Added defs above from sast.ml for VSCode syntax checking *)

let rec check (program : program) : sprogram =
  (* in-place mutation *)
  let var_decls = Hashtbl.create 100 in
  let add_var ((id : string), (t : typ)) =
    try
      let _ = Hashtbl.find var_decls id in
      raise (Failure ("Duplicate variable " ^ id))
    with Not_found -> Hashtbl.add var_decls id t
  in
  let find_var_type (id : string) : typ =
    try Hashtbl.find var_decls id
    with Not_found -> raise (Failure ("Unbound variable " ^ id))
  in
  let rec check_expr (e : expr) : sexpr =
    match e with
    | IntLit i -> (Int, SIntLit i)
    | FloatLit f -> (Float, SFloatLit f)
    | StringLit s -> (String, SStringLit s)
    | BoolLit b -> (Bool, SBoolLit b)
    | ArrayLit a -> raise (Failure "Not implemented")
    | TupleLit t -> (Tuple, STupleLit (List.map check_expr t))
    | Binop (e1, bin_op, e2) ->
        let t1, e1' = check_expr e1 and t2, e2' = check_expr e2 in
        if t1 = t2 then
          let t =
            match bin_op with
            | (Add | Sub | Mult | FDiv | Mod | Exp) when t1 = Int -> Int
            | (Add | Sub | Mult | Div | Mod | Exp) when t1 = Float -> Float
            | (And | Or) when t1 = Bool -> Bool
            | (Eq | Neq) when t1 = Bool -> Bool
            | (Gt | Lt | Geq | Leq) when t1 = Float -> Bool
            | (Gt | Lt | Geq | Leq) when t1 = Int -> Bool
            | _ -> raise (Failure "Incompatible operands")
          in
          (t, SBinop ((t1, e1'), bin_op, (t2, e2')))
        else raise (Failure "Incompatible operands")
    | Id var -> (find_var_type var, SId var)
    | Asn (var, e) ->
        let t = find_var_type var and t', e' = check_expr e in
        if t = t' then (t, SAsn (var, (t', e')))
        else raise (Failure "Imcompatible type")
    | AugAsn (var, aug_op, e) ->
        let t1 = find_var_type var and t2, e' = check_expr e in
        if t1 = t2 then (t1, SAugAsn (var, aug_op, (t2, e')))
        else raise (Failure "Imcompatible type")
    | Not e ->
        let t, e' = check_expr e in
        if t = Bool then (t, SNot (t, e'))
        else raise (Failure "Expect boolean expression")
    | NotIn (e1, e2) ->
        let t1, e1' = check_expr e1 and t2, e2' = check_expr e2 in
        if t2 = Int || t2 = Bool || t2 = Float || t2 = String then
          raise (Failure "Expects iterables")
          (* todo: check type of e1 matches with type of array type *)
        else (Bool, SNotIn ((t1, e1'), (t2, e2')))
    | Call (id, f_block) -> raise (Failure "Not implemented")
    | _ -> raise (Failure "Semantically invalid expression")
  in
  let check_stmt (stmt : stmt) : sstmt =
    match stmt with
    | Break -> SBreak
    | Continue -> SContinue
    | Expr e -> SExpr (check_expr e)
    | Function (fname, args, t, f_block) -> raise (Failure "Not implemented")
    | Return e -> SReturn (check_expr e)
    | If (e, if_block, else_block) ->
        let t, e' = check_expr e
        and s_if_block = check if_block
        and s_else_block = check else_block in
        if t = Bool then SIf ((t, e'), s_if_block, s_else_block)
        else raise (Failure "Expect boolean expression")
    | While (e, while_block) ->
        let t, e' = check_expr e and s_while_block = check while_block in
        if t = Bool then SWhile ((t, e'), s_while_block)
        else raise (Failure "Expect boolean expression")
    | For (id, e, for_block) ->
        let t, e' = check_expr e in
        let s_stmt =
          match t with
          | Array array_elem_typ ->
              add_var (id, array_elem_typ);
              SFor (id, (t, e'), check for_block)
          | _ -> raise (Failure "Expect Array")
        in
        s_stmt
    | Print e -> SPrint (check_expr e)
    | Decl (id, t) ->
        add_var (id, t);
        SDecl (id, t)
  in
  List.map check_stmt program
