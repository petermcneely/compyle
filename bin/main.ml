type action = Ast | Sast

let () =
  let action = ref Sast in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
  ] in
  let usage_msg = "usage: compyle [-a|-s] [file.cmpy]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let indentation_manager = Compyle.Lexing_stack.create_indentation_manager() in
  let ast = Compyle.Parser.program (Compyle.Scanner.token indentation_manager) lexbuf in
  
  match !action with
    Ast -> print_string (Compyle.Ast.pretty_string_of_program ast)
    | Sast ->
        let sast = Compyle.Semant.check ast in
        print_string (Compyle.Sast.pretty_string_of_sprogram sast);
