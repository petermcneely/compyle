let () =
  let lexbuf = Lexing.from_channel (open_in "./example.cmpy") in
  let indentation_manager = Lexing_stack.create_indentation_manager() in
  let ast = Parser.program (Scanner.token indentation_manager) lexbuf in
  let sast = Semant.check ast in
  print_string (Sast.pretty_string_of_sprogram sast);
