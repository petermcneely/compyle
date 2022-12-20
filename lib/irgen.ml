(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR
*)

module L = Llvm (* LLVM module *)
module A = Ast (* Written by us *)
open Sast

module StringMap = Map.Make(String)

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
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type context 
  and f32_t      = L.double_type  context
  and i1_t       = L.i1_type     context 
  and pointer_i8_t = L.pointer_type (L.i8_type context) in

  (* Return the LLVM type for a MicroC type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> f32_t
    | A.String -> pointer_i8_t
    | A.NoneType -> L.void_type context
    | A.Array (t, length) -> L.array_type (ltype_of_typ t) length
    | _ -> raise (Failure "Unimplemented")
  in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| pointer_i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  let local_variables = Hashtbl.create 100 in
  let global_variables = Hashtbl.create 100 in
  let global_formatters = Hashtbl.create 100 in

  let get_or_add_global_formatter gsp key builder =
    if not (Hashtbl.mem global_formatters key) then
      let ret = gsp key builder in
      Hashtbl.add global_formatters key ret;
      ret
    else
      Hashtbl.find global_formatters key in

  let build_gsp builder = function
    | A.Int -> get_or_add_global_formatter (L.build_global_stringptr "%d") "intgsp" builder
    | A.Float -> get_or_add_global_formatter (L.build_global_stringptr "%f") "floatgsp" builder
    | A.Bool -> get_or_add_global_formatter (L.build_global_stringptr "%d") "boolgsp" builder
    | typ -> raise (Failure ("the built in formatting string for " ^ (A.string_of_typ typ) ^ " is not supported")) in

  let func_declarations : (L.llvalue * sstmt) StringMap.t = 
    let func_decl m sstmt = 
      match sstmt with 
      | SFunction(fname, formals, rtyp, _) -> 
        let name = fname  
        and formal_types = 
          Array.of_list (List.map (fun (_, t) -> ltype_of_typ t) formals)  
        in let ftype = L.function_type (ltype_of_typ rtyp) formal_types in
        StringMap.add name (L.define_function name ftype the_module, sstmt) m
      | _ -> m in  
    List.fold_left func_decl StringMap.empty sprogram  
  in 
  (* LLVM insists each basic block end with exactly one "terminator"
  instruction that transfers control.  This function runs "instr builder"
  if the current block does not already have a terminator.  Used,
  e.g., to handle the "fall off the end of the function" case. *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) in
  
  (* Given a function name, get the builder to that function *)  
  let get_function_builder name = 
    let (the_function, _) = StringMap.find name func_declarations in 
    L.builder_at_end context (L.entry_block the_function)
  in

  (* More should be filled in here *)
  let rec build_IR_on_expr builder ((_, e) : sexpr) local_variables (global_variables) =
    match e with
    | SIntLit i -> L.const_int i32_t i 
    | SFloatLit i -> L.const_float f32_t i
    | SStringLit s -> L.build_global_stringptr (String.sub s 1 ((String.length s) - 2)) "" builder
    | SBoolLit b -> L.const_int i1_t (if b = true then 1 else 0)
    | SArrayLit l -> if List.length l = 0 then L.const_array i32_t [||] else (
      let head = List.hd l in
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
    | STupleLit _ -> raise (Failure " Unimplemented")
    | SBinop (e1, op, e2) ->
      (let ty = fst e1 in  
      let e1' = build_IR_on_expr builder e1 local_variables global_variables
      and e2' = build_IR_on_expr builder e2 local_variables global_variables in
      match ty with
        | Int -> (match op with
            A.Add     -> L.build_add
            | A.Sub     -> L.build_sub
            | A.Mult    -> L.build_mul
            | A.Div     -> L.build_sdiv
            | A.Mod     -> L.build_srem
            | A.Eq       -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.Gt      -> L.build_icmp L.Icmp.Sgt
            | A.Lt      -> L.build_icmp L.Icmp.Slt
            | A.Geq      -> L.build_icmp L.Icmp.Sge
            | A.Leq      -> L.build_icmp L.Icmp.Sle
            | _ -> raise(Failure "Developer Error")
            ) e1' e2' "tmp" builder
        | Float ->
            (match op with
            A.Add     -> L.build_fadd
            | A.Sub     -> L.build_fsub
            | A.Mult    -> L.build_fmul
            | A.Div     -> L.build_fdiv
            | A.Mod     -> L.build_frem
            | A.Eq       -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq     -> L.build_fcmp L.Fcmp.One
            | A.Gt      -> L.build_fcmp L.Fcmp.Ogt
            | A.Lt      -> L.build_fcmp L.Fcmp.Olt
            | A.Geq      -> L.build_fcmp L.Fcmp.Oge
            | A.Leq      -> L.build_fcmp L.Fcmp.Ole
            | _ -> raise(Failure "Developer Error")
            ) e1' e2' "tmp" builder
        | Bool -> 
            (match op with
            | A.Eq       -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | _ -> raise(Failure "Developer Error")
            ) e1' e2' "tmp" builder
        | _ -> raise(Failure "Type Error"))
    | SId s -> (
      try
        let lv = Hashtbl.find local_variables s in
        L.build_load (snd lv) s builder
      with Not_found -> (
        let gv = Hashtbl.find global_variables s in
        match fst gv with
        | A.String -> (
          let zero = L.const_int i32_t 0 in
          L.build_in_bounds_gep (snd gv) [| zero; zero |] s builder
        )
        | _ -> L.build_load (snd gv) s builder
      )
    )
    | SAsn (s, e) -> 
      (* Plan: 
        e.code || Gen(s, "=", e.addr)  
      *)
      let e_addr = build_IR_on_expr builder e local_variables global_variables in
      let lv = Hashtbl.find local_variables s in
      ignore(L.build_store e_addr (snd lv) builder); (* %store %e %s_mem *)
      e_addr 
    | SAugAsn (s, ag_op, e) -> 
      (* Plan: 
        s = s + e 
        e.code || Binop(s, e) || assignment   
      *)
      let e_addr = build_IR_on_expr builder e local_variables global_variables in 
      let lv = Hashtbl.find local_variables s in
      let sum_addr = L.build_add e_addr (snd lv) "tmp" builder in 
      ignore(L.build_store sum_addr (snd lv) builder);
      sum_addr
    | SNot e -> 
      let e_addr = build_IR_on_expr builder e local_variables global_variables in  
      L.build_not e_addr "tmp" builder
    | SIn (_, _) -> raise (Failure " Unimplemented")
    | SNotIn (_, _) -> raise (Failure " Unimplemented")
    | SCall ("print", [e]) ->
      let llval = build_IR_on_expr builder e local_variables global_variables in
      let arr = if (fst e) = A.String then
        [| llval |]
      else
        let gsp = build_gsp builder (fst e) in
        [| gsp; llval|]
      in
      L.build_call printf_func arr "printf" builder
    | SCall (fname, args) -> 
      let (f_addr, sstmt) = StringMap.find fname func_declarations in 
      let llargs = List.rev(List.map (fun e -> build_IR_on_expr builder e local_variables global_variables) (List.rev args)) in 
      L.build_call f_addr (Array.of_list llargs) (fname^" result") builder 
  in
  let rec build_IR_on_stmt (builder: L.llbuilder) (local_variables) (global_variables) = function
    (* match sstmt*)
    | SBreak -> ignore(L.build_ret_void builder); builder
    | SExpr e -> 
      ignore(build_IR_on_expr builder e local_variables global_variables); builder
    | SFunction _ -> builder
    | SReturn e -> 
      (* 
        e.code || 
        L.build_ret 
      *)
      ignore(L.build_ret (build_IR_on_expr builder e local_variables global_variables) builder); builder
    | SIf (pred, stmt1, stmt2) -> 
      let expr_addr = build_IR_on_expr builder pred  local_variables global_variables in 

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

      ignore(L.build_cond_br expr_addr then_bb else_bb builder);
      
      let then_builder = L.builder_at_end context then_bb in 
      ignore(build_IR_on_stmt_list then_builder stmt1 local_variables global_variables);

      let else_builder = L.builder_at_end context else_bb in 
      ignore(build_IR_on_stmt_list else_builder stmt2 local_variables global_variables);

      let build_br_end = L.build_br end_bb in (* partial function *)
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

      let while_bb = L.append_block context "while" the_function in
      let build_br_while = L.build_br while_bb in
      ignore(build_br_while builder);
      let while_builder = L.builder_at_end context while_bb in 
      let bool_val = build_IR_on_expr while_builder e local_variables global_variables in

      let body_bb = L.append_block context "while_body" the_function in
      add_terminal (build_IR_on_stmt_list (L.builder_at_end context body_bb) sl local_variables global_variables) build_br_while;

      let end_bb = L.append_block context "while_end" the_function in 

      ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
      L.builder_at_end context end_bb
      
    | SFor (var_name, itr, stmts) -> 
      (* 
        SFor(var_name, itr, stmts) -> 
        HEAD: 
          itr.code
          jmp LOOP   
        LOOP: 
          - need to store current value of var_name in the symbol table for local variables 
          stmts.code 
          jmp HEAD? 
        END:
      *)

      let the_function = L.block_parent (L.insertion_block builder) in 
      let head_bb = L.append_block context "head" the_function in  
      let loop_bb = L.append_block context "loop" the_function in 
      let end_bb = L.append_block context "end" the_function in 

      let start_val = build_IR_on_expr builder itr local_variables global_variables in (* itr.code *)
      ignore(L.build_br loop_bb builder); (* jmp LOOP *)

      ignore(L.position_at_end loop_bb builder);
      let variable = L.build_phi [(start_val, head_bb)] var_name builder in 

      build_IR_on_stmt_list builder stmts local_variables global_variables
    | SDecl (id, typ, expr_opt) ->
      if Option.is_some expr_opt then (
          let expr = Option.get expr_opt in
          let e_addr = build_IR_on_expr builder expr local_variables global_variables in
          let local_variable = L.build_alloca (L.type_of e_addr) id builder in
          Hashtbl.add local_variables id (typ, local_variable);
          ignore(L.build_store e_addr local_variable builder) (* %store %e %s_mem *)
      )
      else (
        let local_variable = L.build_alloca (ltype_of_typ typ) id builder in
        Hashtbl.add local_variables id (typ, local_variable)
      );

      builder
  and build_IR_on_function = function
  | SFunction (name, formals, rtyp, sl) -> 
    let f_builder = get_function_builder name in
    let (the_function, _) = StringMap.find name func_declarations in
    (*
      FNAME: 
      -> builder   
    *)

    let scoped_local_variables = Hashtbl.copy local_variables in

    let add_formal (hash_table: (string, A.typ * L.llvalue) Hashtbl.t) (t: A.typ) (name: string) (pointer: L.llvalue) =
      L.set_value_name name pointer;
      let local = L.build_alloca (ltype_of_typ t) name f_builder in
      ignore (L.build_store pointer local f_builder);
      Hashtbl.add hash_table name (t, local) in 
    List.iter2 (fun decl -> add_formal scoped_local_variables (snd decl) (fst decl) ) formals (Array.to_list (L.params the_function));
    
    let func_builder = build_IR_on_stmt_list f_builder sl scoped_local_variables global_variables in
    
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))  
  | SDecl (name, typ, expr_opt) ->
    let default_const = function
      | A.Int -> L.const_int i32_t 0
      | A.Float -> L.const_float f32_t 0.
      | A.String -> L.const_stringz context ""
      | A.Bool -> L.const_int i1_t 0
      | _ -> raise (Failure "wrong default type") in
    let initialized_const = function
      | SIntLit i -> L.const_int i32_t i
      | SFloatLit f -> L.const_float f32_t f
      | SStringLit s -> L.const_stringz context (String.sub s 1 ((String.length s) - 2))
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | _ -> raise (Failure "wrong constant type") in
    let initial_value = match expr_opt with
      | None -> default_const typ
      | Some sexpr -> initialized_const (snd sexpr) in
    Hashtbl.add global_variables name (typ, (L.define_global name initial_value the_module));
  | _ -> ()
  and build_IR_on_stmt_list builder sl local_variables global_variables = 
      List.fold_left (fun b s -> build_IR_on_stmt b local_variables global_variables s) builder sl 
  in
  (* Unsure the usage of L.builder here but it helps compile for now*)
  (*List.map (build_IR_on_stmt L.builder) sprogram;*)
  List.iter build_IR_on_function sprogram;

  (* the_module is a mutable ptr *)
  the_module
(* return Llvm.module *)