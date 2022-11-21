pen SAst

(*
Boiler plate set up for running tests   
*)
let passed_tests = ref 0;;
let count_tests = ref 0;;

let parse_program (input: string) =
  let lexbuf = Lexing.from_string input in
  let indentation_manager = Lexing_stack.create_indentation_manager() in
  let ast = Parser.program (Scanner.token indentation_manager) lexbuf in
  let sast = Semant.check ast in
  SAst.string_of_sprogram sast in

let pass_test () =
  passed_tests := (!passed_tests + 1);
  print_endline "\tYAY" in

let run_test ?(debug: bool = false) (test_case: string) (input: string) (expected: string list): unit =
  print_endline (test_case ^ ":");
  count_tests := (!count_tests + 1);
  let parsed_program = parse_program input in
  if debug then print_endline ("parsed_program:\n" ^ parsed_program);
  let program_lines = String.split_on_char '\n' parsed_program in
  if debug then (print_endline ("parsed_program line count: " ^ string_of_int (List.length program_lines)));
  if debug then (print_endline ("expected line count: " ^ string_of_int (List.length expected)));
  if List.equal (fun x y -> if debug then print_endline ("x: " ^ x ^ " | y: " ^ y); x = y) program_lines expected then
    pass_test()
  else
    print_endline("\tOOPS") in
