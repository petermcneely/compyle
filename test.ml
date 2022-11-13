open Ast

(*
Boiler plate set up for running tests   
*)
let passed_tests = ref 0;;
let count_tests = ref 0;;

let parse_program (input: string) =
  let lexbuf = Lexing.from_string input in
  let indentation_manager = Lexing_stack.create_indentation_manager() in
  let ast = Parser.program (Scanner.token indentation_manager) lexbuf in
  Ast.string_of_program ast in

let pass_test () =
  passed_tests := (!passed_tests + 1);
  print_endline "\tYAY" in

let run_test ?(debug: bool = false) (test_case: string) (input: string) (output: string list): unit =
  print_endline (test_case ^ ":");
  count_tests := (!count_tests + 1);
  let parsed_program = parse_program input in
  if debug then print_endline (parsed_program);
  let program_lines = String.split_on_char '\n' parsed_program in
  if List.equal (fun x y -> if debug then print_endline ("x: " ^ x ^ " | y: " ^ y); x = y) program_lines output then
    pass_test()
  else
    print_endline("\tOOPS") in

(*
The actual test cases   
*)
let test_case = "Scans, parses, and generates the ast for the hello world program" in
let program = "\
def add(x, y):\r\n\
\"\"\"\r\n
this adds two integers\r\n
\"\"\"\r\n
\treturn x + y\r\n
print(add(x,y))\r\n" in
let output = ["def add(x, y):"; "return x + y"; "print(add(x, y))"; "" ] in
run_test ~debug:false test_case program output;

let test_case = "Parses literals being added together" in
let addition = "3 + 5" in
let output = [addition] in
run_test ~debug:false test_case (addition ^ "\n") output;

(*
Tests basic logical operators   
*)
let test_case = "Parses literals being compared" in
let comparing = "8 > 5 and 3 < 9" in
let output = [comparing] in 
run_test ~debug:false test_case (comparing ^ "\n") output;

(*
Boiler plate set up for processing the results of the tests   
*)
print_newline();
print_int !passed_tests; print_string "/"; print_int(!count_tests); print_endline " tests passed!";
let percentage_passed = (float_of_int !passed_tests) /. (float_of_int !count_tests) *. 100. in
print_endline (string_of_float percentage_passed ^ "%");
let output = open_out "test_percentage.txt" in
Printf.fprintf output "%.2f" percentage_passed;
close_out output;
