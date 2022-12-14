open Ast
open Sast
module StringMap = Map.Make (String)

let rec check ?(top_level : bool = true) (program : program) : sprogram =
  (* in-place mutation *)
  let var_decls_global = Hashtbl.create 100 in
  let func_decls_global = Hashtbl.create 100 in
  let add_func
      ( (decl_vars : ('a, 'b) Hashtbl.t),
        (decl_funcs : ('a, 'c) Hashtbl.t),
        (fd : stmt) ) =
    match fd with
    | Function (fname, _, _, _) -> (
        if Hashtbl.mem decl_vars fname then
          raise (Failure (fname ^ " already declared as variables"))
        else
          try
            let _ = Hashtbl.find decl_funcs fname in
            raise (Failure ("Duplicate function " ^ fname))
          with Not_found -> Hashtbl.add decl_funcs fname fd)
    | _ -> raise (Failure "Developer Error")
  in
  let find_func ((decl_funcs : ('a, 'c) Hashtbl.t), (fname : string)) : stmt =
    try Hashtbl.find decl_funcs fname
    with Not_found ->
      if fname = "main" then
        raise (Failure "main function required but not defined")
      else raise (Failure ("Unbound function " ^ fname))
  in

  (* add built in print function
   * we actually do not care about the parameters since we'll semantically
   * work around this below
  *)
  let print_function = Function ("print", [], Int, []) in
  add_func (var_decls_global, func_decls_global, print_function);

  let add_var
      ( (decl_vars : ('a, 'b) Hashtbl.t),
        (decl_funcs : ('a, 'c) Hashtbl.t),
        (id : string),
        (t : typ) ) =
    try
      if Hashtbl.mem decl_funcs id then
        raise (Failure (id ^ "already declared as function"))
      else
        let _ = Hashtbl.find decl_vars id in
        raise (Failure ("Duplicate variable " ^ id))
    with Not_found -> Hashtbl.add decl_vars id t
  in
  let find_var_type ((decl_vars : ('a, 'b) Hashtbl.t), (id : string)) : typ =
    try Hashtbl.find decl_vars id
    with Not_found -> raise (Failure ("Unbound variable " ^ id))
  in
  let rec raise_error_if_array_has_elems_with_diff_types (l : sexpr list) =
    match l with
    | [] -> ()
    | (t1, _) :: (t2, _) :: _ when not (t1 = t2) ->
        raise
          (Failure
             ("found array of mixed types (" ^ string_of_typ t1 ^ ", and "
            ^ string_of_typ t2))
    | _ :: t -> raise_error_if_array_has_elems_with_diff_types t
  in
  let args_has_same_type ((params : decl list), (args : sexpr list)) : bool =
    List.fold_left2
      (fun result param arg ->
        match (param, arg) with
        | (_, t1), (t2, _) when t1 = t2 -> true && result
        | _ -> false && result)
      true params args
  in
  let rec check_expr
      ( (decl_vars : ('a, 'b) Hashtbl.t),
        (decl_funcs : ('a, 'c) Hashtbl.t),
        (e : expr) ) : sexpr =
    match e with
    | IntLit i -> (Int, SIntLit i)
    | FloatLit f -> (Float, SFloatLit f)
    | StringLit s -> (String, SStringLit s)
    | BoolLit b -> (Bool, SBoolLit b)
    | ArrayLit a ->
        let check_expr_with_decls ex = check_expr (decl_vars, decl_funcs, ex) in
        let s_el = List.map check_expr_with_decls a in
        let s_stmt =
          match s_el with
          | [] -> (EmptyArray, SArrayLit s_el)
          | _ ->
              let _ = raise_error_if_array_has_elems_with_diff_types s_el in
              let head = List.hd s_el in
              let head_type, _ = head in
              let resolve_array_type = function
              | Array (array_typ, i) -> Array (array_typ, List.length s_el)
              | _ as s -> Array (s, List.length s_el)
              in
              (resolve_array_type head_type, SArrayLit s_el)
        in
        s_stmt
    | TupleLit t ->
        let check_expr_with_decls ex = check_expr (decl_vars, decl_funcs, ex) in
        let sexprs = List.map check_expr_with_decls t in
        let typs = List.map (fun (t, se) -> t) sexprs in
        (Tuple(typs), STupleLit (List.map check_expr_with_decls t))
    | Binop (e1, bin_op, e2) ->
        let t1, e1' = check_expr (decl_vars, decl_funcs, e1)
        and t2, e2' = check_expr (decl_vars, decl_funcs, e2) in
        if t1 = t2 then
          let t =
            match bin_op with
            | (Add | Sub | Mult | Div | Mod ) when t1 = Int || t1 = Float
              ->
                t1
            | (And | Or) when t1 = Bool -> t1
            | Eq | Neq -> Bool
            | (Gt | Lt | Geq | Leq) when t1 = Float || t1 = Int -> Bool
            | _ ->
                raise
                  (Failure
                     ("Cannot perform '" ^ string_of_bin_op bin_op ^ "' on '"
                    ^ string_of_typ t1 ^ "'"))
          in
          (t, SBinop ((t1, e1'), bin_op, (t2, e2')))
        else
          raise
            (Failure
               ("Both operands need to be the same type when performing a '"
              ^ string_of_bin_op bin_op ^ "'. Received '" ^ string_of_typ t1
              ^ "' and '" ^ string_of_typ t2 ^ "'"))
    | Id var -> (find_var_type (decl_vars, var), SId var)
    | Asn (var, e) ->
        let t1 = find_var_type (decl_vars, var)
        and t2, e' = check_expr (decl_vars, decl_funcs, e) in(
        match (t1, t2) with
        | _, NoneType -> raise (Failure "Cannot assign variable to nonetype")
        | Array (t1, len1), Array (t2, len2) when t1 = t2 -> (t1, SAsn (var, (t2, e')))
        | Array(t1, len1), EmptyArray when len1 = 0 -> (t1, SAsn (var, (t2, e')))
        | EmptyArray, Array(t2, len2) when len2 = 0 -> (t1, SAsn (var, (t2, e')))
        | Tuple(typs1), Tuple(typs2) when typs1 = typs2 -> (t1, SAsn (var, (t2, e')))
        | t1, t2 when t1 = t2 -> (t1, SAsn (var, (t2, e')))        
        | _ -> raise (Failure ("Incompatible type. Expected Var type: " ^ string_of_typ t1
          ^ " Received expression type: " ^ string_of_typ (t2))))
    | AugAsn (var, aug_op, e) ->
        let t1 = find_var_type (decl_vars, var)
        and t2, e' = check_expr (decl_vars, decl_funcs, e) in
        if t2 = NoneType then
          raise (Failure "Cannot assign variable to nonetype")
        else if t1 = t2 then (t1, SAugAsn (var, aug_op, (t2, e')))
        else raise (Failure "Incompatible type")
    | Not e ->
        let t, e' = check_expr (decl_vars, decl_funcs, e) in
        if t = Bool then (t, SNot (t, e'))
        else raise (Failure "Expect boolean expression")
    | Call (called_fname, args) ->
        let s_stmt =
          match find_func (decl_funcs, called_fname) with
          | Function (fname, params, frtyp, _) ->
              let check_expr_with_decls ex =
                check_expr (decl_vars, decl_funcs, ex)
              in
              let s_args = List.map check_expr_with_decls args in
              if (not (fname = "print")) && args_has_same_type (params, s_args) then
                (frtyp, SCall (fname, s_args))
              else if fname = "print" then
                if not (List.length args = 1) then
                  raise (Failure ("You can only print one expression. Received a call to print " ^ string_of_int (List.length args) ^ " arguments"))
                else
                  (frtyp, SCall (fname, s_args))
              else raise (Failure "Incompatible argument types")
          | _ -> raise (Failure "Expect function")
        in
        s_stmt
  in
  let rec check_stmt
      ( (decl_vars : ('a, 'b) Hashtbl.t),
        (decl_funcs : ('a, 'c) Hashtbl.t),
        (stmt : stmt),
        (top_level : bool),
        (func_details : string * typ) ) : sstmt =
    match stmt with
    | Break -> SBreak
    | Expr e -> SExpr (check_expr (decl_vars, decl_funcs, e))
    | Function (fname, params, rtyp, fbody) ->
        if top_level = false then
          raise
            (Failure
               ("Nested function definitions are not allowed. Received a \
                 definition for a function named: " ^ fname));
        add_func (decl_vars, decl_funcs, Function (fname, params, rtyp, fbody));
        let decl_vars_in_scope = Hashtbl.copy decl_vars in
        let decl_funcs_in_scope = Hashtbl.copy decl_funcs in

        let add_var2 (id, t) =
          add_var (decl_vars_in_scope, decl_funcs_in_scope, id, t)
        in
        List.iter add_var2 params;
        let check_stmt_with_decls st =
          check_stmt
            (decl_vars_in_scope, decl_funcs_in_scope, st, false, (fname, rtyp))
        in
        let s_fbody = List.map check_stmt_with_decls fbody in
        SFunction (fname, params, rtyp, s_fbody)
    | Return e ->
        let sexpr_ret = check_expr (decl_vars, decl_funcs, e) in
        let ret_typ = fst sexpr_ret in
        let func_typ = snd func_details in
        let func_name = fst func_details in
        if (not (String.empty = func_name)) && ret_typ != func_typ then
          raise
            (Failure
               ("Function '" ^ func_name ^ "' expects a return type of "
              ^ string_of_typ func_typ ^ ", but currently returns type "
              ^ string_of_typ ret_typ));
        SReturn sexpr_ret
    | If (e, if_block, else_block) ->
        let check_stmt2 stmt =
          check_stmt (decl_vars, decl_funcs, stmt, false, func_details)
        in
        let t, e' = check_expr (decl_vars, decl_funcs, e)
        and s_if_block = List.map check_stmt2 if_block
        and s_else_block = List.map check_stmt2 else_block in
        if t = Bool then SIf ((t, e'), s_if_block, s_else_block)
        else raise (Failure "Expect boolean expression")
    | While (e, while_block) ->
        let check_stmt2 stmt =
          check_stmt (decl_vars, decl_funcs, stmt, false, func_details)
        in
        let t, e' = check_expr (decl_vars, decl_funcs, e)
        and s_while_block = List.map check_stmt2 while_block in
        if t = Bool then SWhile ((t, e'), s_while_block)
        else raise (Failure "Expect boolean expression")
    | For (id, e, for_block) ->
        let check_stmt2 stmt =
          check_stmt (decl_vars, decl_funcs, stmt, false, func_details)
        in
        let t, e' = check_expr (decl_vars, decl_funcs, e) in
        let s_stmt =
          match t with
          | Array (array_elem_typ, _) ->
              add_var (decl_vars, decl_funcs, id, array_elem_typ);
              SFor (id, (t, e'), List.map check_stmt2 for_block)
          | _ -> raise (Failure "Expect Array")
        in
        s_stmt
    | Decl (id, t, expr_opt) ->
        add_var (decl_vars, decl_funcs, id, t);
        let some_sexpr =
          match expr_opt with
          | None -> None
          | Some e ->
              let sexpr = check_expr (decl_vars, decl_funcs, e) in
              match (t, fst sexpr) with
              | Array (t1, len1), Array (t2, len2) when t1 = t2 ->
                  Some sexpr
              | Array(t1, len1), EmptyArray when len1 = 0 -> Some sexpr
              | EmptyArray, Array(t2, len2) when len2 = 0 -> Some sexpr
              | Tuple(typs1), Tuple(typs2) when typs1 = typs2 -> Some sexpr
              | _ ->              
              if t = fst sexpr then Some sexpr
              else
                raise
                  (Failure
                     ("Incompatible type. Expected Var type: " ^ string_of_typ t
                    ^ " Received expression type: "
                     ^ string_of_typ (fst sexpr)))
        in
        SDecl (id, t, some_sexpr)
  in
  let check_stmt_with_decls st =
    let dummy_function_details = ("", NoneType) in
    check_stmt
      ( var_decls_global,
        func_decls_global,
        st,
        top_level,
        dummy_function_details )
  in
  let checked_program = List.map check_stmt_with_decls program in

  if top_level then
    let main_stmt = find_func (func_decls_global, "main") in

    (* check if main function is defined *)
    let _ =
      match main_stmt with
      | Function (fname, params, rtyp, fbody) ->
          if List.length params > 0 || rtyp != Int then
            raise
              (Failure
                 "The main function should take zero arguments and return an \
                  int")
      | _ -> raise (Failure "Developer error")
    in

    let check_top_level_stmt sstmt =
      match sstmt with
      | SFunction _ -> sstmt
      | SDecl (id, t, expr_opt) -> (
          match expr_opt with
          | None -> sstmt
          | Some sexpr -> (
              match snd sexpr with
              | SIntLit _ | SBoolLit _ | SFloatLit _ | SStringLit _ -> sstmt
              | _ ->
                  raise
                    (Failure
                       ("Global variables can only be assigned to constant \
                         literals. Received: " ^ string_of_sstmt sstmt))))
      | _ ->
          raise
            (Failure
               ("Top level statements can only be global variable declarations \
                 or function definitions. Received: " ^ string_of_sstmt sstmt))
    in

    List.map check_top_level_stmt checked_program
  else checked_program
