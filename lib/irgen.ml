(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR
*)

module L = Llvm (* LLVM module *)
module A = Ast (* Written by us *)
open Sast
module StringMap = Map.Make (String)

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
  (* Get types from the context *)
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and f32_t = L.float_type context
  and i1_t = L.i1_type context
  and pointer_i8_t = L.pointer_type (L.i8_type context) in

  let local_variables = Hashtbl.create 100 in
  let global_variables = Hashtbl.create 100 in
  let global_stringptrs = Hashtbl.create 100 in

  (* Return the LLVM type for a MicroC type *)
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> f32_t
    | A.String -> pointer_i8_t
    | A.Array (t, dim, length) -> L.array_type (ltype_of_typ t) length
    | _ -> raise (Failure "Unimplemented")
  in

  let printf_t : L.lltype = L.var_arg_function_type i32_t [| pointer_i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in

  let build_gsp builder = function
    | A.String -> L.build_global_stringptr "%s" "strgsp" builder
    | A.Int -> L.build_global_stringptr "%d" "intgsp" builder
    | A.Float -> L.build_global_stringptr "%f" "floatgsp" builder
    | A.Bool -> L.build_global_stringptr "%d" "boolgsp" builder
    | _ -> raise (Failure "not supported")
  in

  let global_string_pointer builder =
    L.build_global_stringptr "%s" "gsp" builder
  in

  let func_declarations : (L.llvalue * sstmt) StringMap.t =
    let func_decl m sstmt =
      match sstmt with
      | SFunction (fname, formals, rtyp, _) ->
          let name = fname
          and formal_types =
            Array.of_list (List.map (fun (_, t) -> ltype_of_typ t) formals)
          in
          let ftype = L.function_type (ltype_of_typ rtyp) formal_types in
          StringMap.add name (L.define_function name ftype the_module, sstmt) m
      | _ -> m
    in
    List.fold_left func_decl StringMap.empty sprogram
  in
  (* LLVM insists each basic block end with exactly one "terminator"
     instruction that transfers control.  This function runs "instr builder"
     if the current block does not already have a terminator.  Used,
     e.g., to handle the "fall off the end of the function" case. *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in

  (* Given a function name, get the builder to that function *)
  let get_function_builder name =
    let the_function, _ = StringMap.find name func_declarations in
    L.builder_at_end context (L.entry_block the_function)
  in

  (* More should be filled in here *)
  let rec build_IR_on_expr builder ((_, e) : sexpr) =
    match e with
    | SIntLit i -> L.const_int i32_t i
    | SFloatLit i -> L.const_float f32_t i
    | SStringLit s ->
        L.build_global_stringptr
          (String.sub s 1 (String.length s - 2))
          "" builder
    | SBoolLit b -> L.const_int i1_t (if b = true then 1 else 0)
    | SArrayLit l -> (let head = List.hd l in
    let head_type, _ = head in
    let size = List.length l in
    let arr = Array.of_list l in
    let sarr = Array.map (build_IR_on_expr builder) arr in
    match head_type with
    | Int -> L.const_array i32_t sarr
    | Float -> L.const_array f32_t sarr
    | Bool -> L.const_array i1_t sarr
    (*| String -> L.const_array (L.array_type (s32_t size)*)
    (*| Tuple*)
    | _ -> raise (Failure "Multi-Dim Array"))
    | STupleLit e -> raise (Failure " Unimplemented")
    | SBinop (e1, op, e2) -> (
        let ty = fst e1 in
        let e1' = build_IR_on_expr builder e1
        and e2' = build_IR_on_expr builder e2 in
        match ty with
        | Int ->
            (match op with
            | A.Add -> L.build_add
            | A.Sub -> L.build_sub
            | A.Mult -> L.build_mul
            | A.Div -> L.build_sdiv
            | A.Mod -> L.build_srem
            | A.Eq -> L.build_icmp L.Icmp.Eq
            | A.Neq -> L.build_icmp L.Icmp.Ne
            | A.Gt -> L.build_icmp L.Icmp.Sgt
            | A.Lt -> L.build_icmp L.Icmp.Slt
            | A.Geq -> L.build_icmp L.Icmp.Sge
            | A.Leq -> L.build_icmp L.Icmp.Sle
            | _ -> raise (Failure "Developer Error"))
              e1' e2' "tmp" builder
        | Float ->
            (match op with
            | A.Add -> L.build_fadd
            | A.Sub -> L.build_fsub
            | A.Mult -> L.build_fmul
            | A.Div -> L.build_fdiv
            | A.Mod -> L.build_frem
            | A.Eq -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq -> L.build_fcmp L.Fcmp.One
            | A.Gt -> L.build_fcmp L.Fcmp.Ogt
            | A.Lt -> L.build_fcmp L.Fcmp.Olt
            | A.Geq -> L.build_fcmp L.Fcmp.Oge
            | A.Leq -> L.build_fcmp L.Fcmp.Ole
            | _ -> raise (Failure "Developer Error"))
              e1' e2' "tmp" builder
        | Bool ->
            (match op with
            | A.Eq -> L.build_icmp L.Icmp.Eq
            | A.Neq -> L.build_icmp L.Icmp.Ne
            | A.And -> L.build_and
            | A.Or -> L.build_or
            | _ -> raise (Failure "Developer Error"))
              e1' e2' "tmp" builder
        | _ -> raise (Failure "Type Error"))
    | SId s -> (
        try
          let lv = Hashtbl.find local_variables s in
          L.build_load (snd lv) s builder
        with Not_found -> (
          let gv = Hashtbl.find global_variables s in
          match fst gv with
          | A.String ->
              let zero = L.const_int i32_t 0 in
              L.build_in_bounds_gep (snd gv) [| zero; zero |] s builder
          | _ -> L.build_load (snd gv) s builder))
    | SAsn (_, _) -> raise (Failure " Unimplemented")
    | SAugAsn (_, _, _) -> raise (Failure " Unimplemented")
    | SNot _ -> raise (Failure " Unimplemented")
    | SIn (_, _) -> raise (Failure " Unimplemented")
    | SNotIn (_, _) -> raise (Failure " Unimplemented")
    | SCall ("print", [ e ]) ->
        let llval = build_IR_on_expr builder e in
        L.build_call printf_func [| llval |] "printf" builder
    | SCall (_, _) -> raise (Failure " Unimplemented")
  in
  let rec build_IR_on_stmt (builder : L.llbuilder) = function
    (* match sstmt*)
    | SBreak -> raise (Failure "Unimplemented")
    | SExpr e ->
        ignore (build_IR_on_expr builder e);
        builder
    | SFunction _ -> builder
    | SReturn e ->
        (*
           e.code ||
           L.build_ret
        *)
        ignore (L.build_ret (build_IR_on_expr builder e) builder);
        builder
    | SIf (pred, stmt1, stmt2) ->
        let expr_addr = build_IR_on_expr builder pred in

        let the_function = L.block_parent (L.insertion_block builder) in

        (* TODO: get the function name that is the parent of the If block *)

        (*
           def f1 (x : int, y : int ) -> int
             if (x > 8):
             else:

           If(e, s1, s2)
           F1: <- the_function
             e.code
             cond_br e.addr IF ELSE
             IF:
               s1.code  <- builder
               jmp END
             ELSE:
               s2.code
               jmp END
             END:
        *)
        let then_bb = L.append_block context "then" the_function in
        let else_bb = L.append_block context "else" the_function in
        let end_bb = L.append_block context "end_if" the_function in

        ignore (L.build_cond_br expr_addr then_bb else_bb builder);

        let then_builder = L.builder_at_end context then_bb in
        ignore (build_IR_on_stmt_list then_builder stmt1);

        let else_builder = L.builder_at_end context else_bb in
        ignore (build_IR_on_stmt_list else_builder stmt2);

        let build_br_end = L.build_br end_bb in
        (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        L.builder_at_end context end_bb
    | SWhile (e, sl) ->
        (*
      FNAME:
        jmp COND   
        COND: 
          e.code
          br_cond e.addr BODY END 
        BODY: 
          sl.code
          jmp COND 
        END: 
      *)

        (* TODO: Create three blocks: COND, BODY, END*)
        let the_function = L.block_parent (L.insertion_block builder) in

        let cond_bb = L.append_block context "cond" the_function in
        let body_bb = L.append_block context "body" the_function in
        let end_bb = L.append_block context "end" the_function in

        let cond_builder = L.builder_at_end context cond_bb in
        let expr_addr = build_IR_on_expr cond_builder e in

        ignore (L.build_cond_br expr_addr body_bb end_bb cond_builder);

        let body_builder = L.builder_at_end context body_bb in
        let body_builder = build_IR_on_stmt_list body_builder sl in
        let build_br_end = L.build_br end_bb in
        (* partial function *)
        add_terminal (L.builder_at_end context body_bb) build_br_end;

        L.builder_at_end context end_bb
    | SFor (st, expr, stmts) ->
        raise (Failure "Unimplemented")
        (*
            for i in [1. 2. 3]
            st index, 0
            st val, expr[index]
        *)
    | SDecl (id, typ, expr_opt) ->
        (if Option.is_some expr_opt then (
         let expr = Option.get expr_opt in
         let e_addr = build_IR_on_expr builder expr in
         let local_variable = L.build_alloca (L.type_of e_addr) id builder in
         Hashtbl.add local_variables id (typ, local_variable);
         ignore (L.build_store e_addr local_variable builder)
         (* %store %e %s_mem *))
        else
          let local_variable = L.build_alloca (ltype_of_typ typ) id builder in
          Hashtbl.add local_variables id (typ, local_variable));

        builder
  and build_IR_on_function = function
    | SFunction (name, formals, rtyp, sl) ->
        let f_builder = get_function_builder name in
        (*
      FNAME: 
      -> builder   
    *)
        let func_builder = build_IR_on_stmt_list f_builder sl in
        add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
    | SDecl (name, typ, expr_opt) ->
        let default_const = function
          | A.Int -> L.const_int i32_t 0
          | A.Float -> L.const_float f32_t 0.
          | A.String -> L.const_stringz context ""
          | A.Bool -> L.const_int i1_t 0
          | _ -> raise (Failure "wrong default type")
        in
        let initialized_const = function
          | SIntLit i -> L.const_int i32_t i
          | SFloatLit f -> L.const_float f32_t f
          | SStringLit s ->
              L.const_stringz context (String.sub s 1 (String.length s - 2))
          | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
          | _ -> raise (Failure "wrong constant type")
        in
        let initial_value =
          match expr_opt with
          | None -> default_const typ
          | Some sexpr -> initialized_const (snd sexpr)
        in
        Hashtbl.add global_variables name
          (typ, L.define_global name initial_value the_module)
    | _ -> ()
  and build_IR_on_stmt_list builder sl =
    List.fold_left build_IR_on_stmt builder sl
  in
  (* Unsure the usage of L.builder here but it helps compile for now*)
  (*List.map (build_IR_on_stmt L.builder) sprogram;*)
  List.iter build_IR_on_function sprogram;

  (* the_module is a mutable ptr *)
  the_module
(* return Llvm.module *)