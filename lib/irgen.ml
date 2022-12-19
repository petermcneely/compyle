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
  and f32_t      = L.float_type  context
  and i1_t       = L.i1_type     context in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> f32_t
    | _ -> raise (Failure "Unimplemented")
  in

  let global_vars : L.llvalue StringMap.t = 
    let global_var m (sstmt) = 
      (match sstmt with 
      | SDecl (name, typ, e) -> 
        let i = 
        (
          match e with 
          | None -> 0
          | Some(e') -> 
            let (t, sx) = e' in 
            (
              match sx with
              | SIntLit i -> i 
              | SBoolLit b -> (if b = true then 1 else 0)
              | _ -> 0 
            ) 
        ) in 
          let init = L.const_int (ltype_of_typ typ) i in 
          let gl_addr = L.define_global name init the_module in 
          StringMap.add name gl_addr m
      | _ -> m)
    in
    List.fold_left (global_var) StringMap.empty sprogram 
  in 
  (* TODO: lookup: string -> llvalue 
    Description: Searches for a variable 'n' first locally and then globally and returns its value if found   
  *)
  let lookup n = 
    L.const_int i32_t 3 
  in 

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
  let rec build_IR_on_expr builder ((_, e) : sexpr) =
    match e with
    | SIntLit i -> L.const_int i32_t i 
    | SFloatLit i -> L.const_float f32_t i
    | SStringLit s -> L.const_string context s
    | SBoolLit b -> L.const_int i1_t (if b = true then 1 else 0)
    | SArrayLit l -> raise (Failure "Unimplemented")
    | STupleLit _ -> raise (Failure " Unimplemented")
    | SBinop (e1, op, e2) -> 
      (let ty = fst e1 in  
      let e1' = build_IR_on_expr builder e1
      and e2' = build_IR_on_expr builder e2 in
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
    | SId s -> L.build_load (lookup s) s builder (* %s = load %(value of s)*)
    | SAsn (s, e) -> 
      (* Plan: 
        e.code || Gen(s, "=", e.addr)  
      *)
      let e_addr = build_IR_on_expr builder e in 
      ignore(L.build_store e_addr (lookup s) builder); (* %store %e %s_mem *)
      e_addr 
    | SAugAsn (s, ag_op, e) -> 
      (* Plan: 
        s = s + e 
        e.code || Binop(s, e) || assignment   
      *)
      let e_addr = build_IR_on_expr builder e in 
      let sum_addr = L.build_add e_addr (lookup s) "tmp" builder in 
      ignore(L.build_store sum_addr (lookup s) builder);
      sum_addr
    | SNot e -> 
      let e_addr = build_IR_on_expr builder e in  
      L.build_not e_addr "tmp" builder  
    | SIn (_, _) -> raise (Failure " Unimplemented")
    | SNotIn (_, _) -> raise (Failure " Unimplemented")
    | SCall (fname, args) -> 
      let (f_addr, sstmt) = StringMap.find fname func_declarations in 
      let llargs = List.rev(List.map (fun e -> build_IR_on_expr builder e) (List.rev args)) in 
      L.build_call f_addr (Array.of_list llargs) (fname^" result") builder 
  in
  let rec build_IR_on_stmt (builder: L.llbuilder) = function
    (* match sstmt*)
    | SBreak -> ignore(L.build_ret_void builder); builder 
    | SContinue -> raise (Failure "Unimplemented")
    | SExpr e -> 
      ignore(build_IR_on_expr builder e); builder
    | SFunction _ -> builder
    | SReturn e -> 
      (* 
        e.code || 
        L.build_ret 
      *)
      ignore(L.build_ret (build_IR_on_expr builder e) builder); builder
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

      ignore(L.build_cond_br expr_addr then_bb else_bb builder);
      
      let then_builder = L.builder_at_end context then_bb in 
      ignore(build_IR_on_stmt_list then_builder stmt1);

      let else_builder = L.builder_at_end context else_bb in 
      ignore(build_IR_on_stmt_list else_builder stmt2);

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

      let cond_bb = L.append_block context "cond" the_function in 
      let body_bb = L.append_block context "body" the_function in 
      let end_bb = L.append_block context "end" the_function in 

      let cond_builder = L.builder_at_end context cond_bb in 
      let expr_addr = build_IR_on_expr cond_builder e in  
      
      ignore(L.build_cond_br expr_addr body_bb end_bb cond_builder); 

      let body_builder = L.builder_at_end context body_bb in 
      let body_builder = build_IR_on_stmt_list body_builder sl in 
      let build_br_end = L.build_br end_bb in (* partial function *)
      add_terminal (L.builder_at_end context body_bb) build_br_end;
      
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

      let start_val = build_IR_on_expr builder itr in (* itr.code *)
      ignore(L.build_br loop_bb builder); (* jmp LOOP *)
      
      ignore(L.position_at_end loop_bb builder);
      let variable = L.build_phi [(start_val, head_bb)] var_name builder in 

      build_IR_on_stmt_list builder stmts 

    | SPrint _ -> raise (Failure "Unimplemented")
    | SDecl (_, _, _) -> raise (Failure "Unimplemented")
  and build_IR_on_function = function
  | SFunction (name, formals, rtyp, sl) -> 
    let f_builder = get_function_builder name in  
    (*
      FNAME: 
      -> builder   
    *)
    let func_builder = build_IR_on_stmt_list f_builder sl in
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
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