open Ast
open Sast
module StringMap = Map.Make (String)

let rec check (program : program) : sprogram =
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
      if (fname = "main") then raise (Failure ("main function required but not defined")) 
      else raise (Failure ("Unbound function " ^ fname))
  in

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
              |  Array (array_typ, dimension) -> Array (array_typ, dimension+1)
              |  _ as s -> Array (s,1) 
              in
              (resolve_array_type head_type, SArrayLit s_el)
        in
        s_stmt
    | TupleLit t ->
        let check_expr_with_decls ex = check_expr (decl_vars, decl_funcs, ex) in
        (Tuple, STupleLit (List.map check_expr_with_decls t))
    | Binop (e1, bin_op, e2) ->
        let t1, e1' = check_expr (decl_vars, decl_funcs, e1)
        and t2, e2' = check_expr (decl_vars, decl_funcs, e2) in
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
    | Id var -> (find_var_type (decl_vars, var), SId var)
    | Asn (var, e) ->
        let t = find_var_type (decl_vars, var)
        and t', e' = check_expr (decl_vars, decl_funcs, e) in
        if t = t' then (t, SAsn (var, (t', e')))
        else raise (Failure "Incompatible type")
    | AugAsn (var, aug_op, e) ->
        let t1 = find_var_type (decl_vars, var)
        and t2, e' = check_expr (decl_vars, decl_funcs, e) in
        if t1 = t2 then (t1, SAugAsn (var, aug_op, (t2, e')))
        else raise (Failure "Incompatible type")
    | Not e ->
        let t, e' = check_expr (decl_vars, decl_funcs, e) in
        if t = Bool then (t, SNot (t, e'))
        else raise (Failure "Expect boolean expression")
    | NotIn (e1, e2) ->
        let t1, e1' = check_expr (decl_vars, decl_funcs, e1)
        and t2, e2' = check_expr (decl_vars, decl_funcs, e2) in
        let sexpr =
          match t2 with
          | Tuple | EmptyArray -> (Bool, SNotIn ((t1, e1'), (t2, e2')))
          | Array (elem_typ, _) when elem_typ = t1 ->
              (Bool, SNotIn ((t1, e1'), (t2, e2')))
          | Array (elem_typ, _) when elem_typ != t1 ->
              raise (Failure "Expect array's element type matches")
          | _ -> raise (Failure "Expect iterables")
        in
        sexpr
    | In (e1, e2) ->
        let t1, e1' = check_expr (decl_vars, decl_funcs, e1)
        and t2, e2' = check_expr (decl_vars, decl_funcs, e2) in
        let sexpr =
          match t2 with
          | Tuple | EmptyArray -> (Bool, SIn ((t1, e1'), (t2, e2')))
          | Array (elem_typ, _) when elem_typ = t1 ->
              (Bool, SIn ((t1, e1'), (t2, e2')))
          | Array (elem_typ, _) when elem_typ != t1 ->
              raise (Failure "Expect array's element type matches")
          | _ -> raise (Failure "Expect iterables")
        in
        sexpr
    | Call (called_fname, args) ->
        let s_stmt =
          match find_func (decl_funcs, called_fname) with
          | Function (fname, params, frtyp, _) ->
              let check_expr_with_decls ex =
                check_expr (decl_vars, decl_funcs, ex)
              in
              let s_args = List.map check_expr_with_decls args in
              if args_has_same_type (params, s_args) then
                (frtyp, SCall (fname, s_args))
              else raise (Failure "Incompatible argument types")
          | _ -> raise (Failure "Expect function")
        in
        s_stmt
  in
  let rec check_stmt
      ( (decl_vars : ('a, 'b) Hashtbl.t),
        (decl_funcs : ('a, 'c) Hashtbl.t),
        (stmt : stmt) ) : sstmt =
    match stmt with
    | Break -> SBreak
    | Continue -> SContinue
    | Expr e -> SExpr (check_expr (decl_vars, decl_funcs, e))
    | Function (fname, params, rtyp, fbody) ->
        add_func (decl_vars, decl_funcs, Function (fname, params, rtyp, fbody));
        let decl_vars_in_scope = Hashtbl.copy decl_vars in
        let decl_funcs_in_scope = Hashtbl.copy decl_funcs in
        let add_var2 (id, t) =
          add_var (decl_vars_in_scope, decl_funcs_in_scope, id, t)
        in
        List.iter add_var2 params;
        let check_stmt_with_decls st =
          check_stmt (decl_vars_in_scope, decl_funcs_in_scope, st)
        in
        let s_fbody = List.map check_stmt_with_decls fbody in
        SFunction (fname, params, rtyp, s_fbody)
    | Return e -> SReturn (check_expr (decl_vars, decl_funcs, e))
    | If (e, if_block, else_block) ->
        let t, e' = check_expr (decl_vars, decl_funcs, e)
        and s_if_block = check if_block
        and s_else_block = check else_block in
        if t = Bool then SIf ((t, e'), s_if_block, s_else_block)
        else raise (Failure "Expect boolean expression")
    | While (e, while_block) ->
        let t, e' = check_expr (decl_vars, decl_funcs, e)
        and s_while_block = check while_block in
        if t = Bool then SWhile ((t, e'), s_while_block)
        else raise (Failure "Expect boolean expression")
    | For (id, e, for_block) ->
        let t, e' = check_expr (decl_vars, decl_funcs, e) in
        let s_stmt =
          match t with
          | Array (array_elem_typ, _) ->
              add_var (decl_vars, decl_funcs, id, array_elem_typ);
              SFor (id, (t, e'), check for_block)
          | _ -> raise (Failure "Expect Array")
        in
        s_stmt
    | Print e -> SPrint (check_expr (decl_vars, decl_funcs, e))
    | Decl (id, t, expr_opt) ->
        add_var (decl_vars, decl_funcs, id, t);
        let some_sexpr = match expr_opt with
            None -> None
          | Some e ->
              let sexpr = check_expr (decl_vars, decl_funcs, e) in
              if t = fst sexpr then
                Some sexpr
              else
                raise (Failure ("Incompatible type. Expected Var type:" ^ string_of_typ t ^ " Received expression type: " ^ string_of_typ (fst sexpr))) in
        SDecl (id, t, some_sexpr)
  in
  let check_stmt_with_decls st =
    check_stmt (var_decls_global, func_decls_global, st)
  in
  let _ = find_func (func_decls_global, "main") in (* check if main function is defined *)
  List.map check_stmt_with_decls program