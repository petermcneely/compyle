(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR
*)

module L = Llvm (* LLVM module *)
module A = Ast (* Written by us *)
open Sast

(* translate : Sast.program -> Llvm.module *)
(* Sast.program is a tuple of globals (vars) and functions *)
let translate (sprogram : sprogram) =
  (* Context holds the state of LLVM system for compilation *)
  (* Can think of context like an environment to hold the states *)
  let context = L.global_context () in
  (* Create the LLVM compilation module which generates code *)
  (* Modules represent the top-level structure in an LLVM program *)
  (* Each module consists of functions, global vars and symbol table entries*)
  (* in each function, it also contains basic blocks with error codes *)
  let the_module = L.create_module context "compyle" in
  (* More should be filled in here *)
  let rec build_IR_on_expr builder ((_, e) : sexpr) =
    match e with
    | SIntLit _ -> raise (Failure " Unimplemented")
    | SFloatLit _ -> raise (Failure " Unimplemented")
    | SStringLit _ -> raise (Failure " Unimplemented")
    | SBoolLit _ -> raise (Failure " Unimplemented")
    | SArrayLit _ -> raise (Failure " Unimplemented")
    | STupleLit _ -> raise (Failure " Unimplemented")
    | SBinop (_, _, _) -> raise (Failure " Unimplemented")
    | SId _ -> raise (Failure " Unimplemented")
    | SAsn (_, _) -> raise (Failure " Unimplemented")
    | SAugAsn (_, _, _) -> raise (Failure " Unimplemented")
    | SNot _ -> raise (Failure " Unimplemented")
    | SIn (_, _) -> raise (Failure " Unimplemented")
    | SNotIn (_, _) -> raise (Failure " Unimplemented")
    | SCall (_, _) -> raise (Failure " Unimplemented")
  in
  let rec build_IR_on_stmt builder = function
    (* match sstmt*)
    | SBreak -> raise (Failure "Unimplemented")
    | SContinue -> raise (Failure "Unimplemented")
    | SExpr _ -> raise (Failure "Unimplemented")
    | SFunction (_, _, _, _) -> raise (Failure "Unimplemented")
    | SReturn _ -> raise (Failure "Unimplemented")
    | SIf (_, _, _) -> raise (Failure "Unimplemented")
    | SWhile (_, _) -> raise (Failure "Unimplemented")
    | SFor (_, _, _) -> raise (Failure "Unimplemented")
    | SPrint _ -> raise (Failure "Unimplemented")
    | SDecl (_, _, _) -> raise (Failure "Unimplemented")
  in
  (* Unsure the usage of L.builder here but it helps compile for now*)
  List.iter (build_IR_on_stmt L.builder) sprogram;
  (* the_module is a mutable ptr *)
  the_module
(* return Llvm.module *)
