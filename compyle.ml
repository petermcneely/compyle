let () =
  let lexbuf = Lexing.from_channel (open_in "./example.cmpy") in
  let indentation_manager = Lexing_stack.create_indentation_manager() in
  let ast = Parser.program (Scanner.token indentation_manager) lexbuf in
  print_string (Ast.pretty_string_of_program ast);
