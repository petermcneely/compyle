let () =
  let lexbuf = Lexing.from_channel stdin in
  let _ = Parser.program Scanner.token lexbuf in
  print_newline();