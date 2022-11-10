type aug_op = Add | Sub | Mult | Div | Mod | Exp | FDiv
type bin_op =
  Add | Sub | Mult | Div | Mod | Exp | FDiv
  | And | Or
  | Eq | Neq | Gt | Lt | Geq | Leq

type indent = Indent

type expr =
  IntLit of int
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | Id of string
  | ArrayLit of expr list
  | TupleLit of expr list
  | Binop of expr * bin_op * expr
  | Asn of string * expr
  | AugAsn of string * aug_op * expr
  | Not of expr
  | In of expr * expr
  | NotIn of expr * expr
  | Call of string * expr list

type stmt =
  Break
  | Continue
  | Expr of expr
  | Function of string * expr list * stmt
  | Return of expr
  | If of expr * stmt * stmt list * stmt list
  | While of expr * stmt
  | For of string * expr * stmt
  | Block of (indent list * stmt) list
  | Elif of expr * stmt
  | Else of stmt

type stmtseq = stmt list