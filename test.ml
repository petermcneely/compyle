open Ast

(*
Boiler plate set up for running tests   
*)
let passed_tests = ref 0;;
let count_tests = ref 0;;

let parse_program (input: string) =
  let lexbuf = Lexing.from_string input in
  Parser.program Scanner.token lexbuf in

let pass_test () =
  passed_tests := (!passed_tests + 1);
  print_endline "\tYAY" in

let run_test ?(debug: bool = false) (test_case: string) (input: string) (output: string list): unit =
  print_endline (test_case ^ ":");
  count_tests := (!count_tests + 1);
  let parsed_program = parse_program input in
  if debug then print_endline (string_of_program parsed_program);
  if List.equal (fun x y -> if debug then print_endline ("x: " ^ x ^ "y: " ^ y); x = y) parsed_program output then
    pass_test()
  else
    print_endline("\tOOPS") in

(*
The actual test cases   
*)
let test_case = "Parses literals being added together" in
let addition = "3 + 5" in
let output = ["LITERAL: 3";"PLUS";"LITERAL: 5"] in
run_test test_case addition output;

(*
Tests basic logical operators   
*)
let test_case = "Parses literals being compared" in
let comparing = "8 > 5 and 3 < 9" in
let output = ["LITERAL: 8";"GT";"LITERAL: 5";"AND";"LITERAL: 3";"LT";"LITERAL: 9"] in
run_test test_case comparing output;

let test_case = "Parses literals being compared" in
let comparing = "8 >= 5 or 3 <= 9" in
let output = ["LITERAL: 8";"GEQ";"LITERAL: 5";"OR";"LITERAL: 3";"LEQ";"LITERAL: 9"] in
run_test test_case comparing output;

let test_case = "Parses literals being compared" in
let comparing = "8 == 5 and 3 != 9" in
let output = ["LITERAL: 8";"EQ";"LITERAL: 5";"AND";"LITERAL: 3";"NEQ";"LITERAL: 9"] in
run_test test_case comparing output;

let test_case = "Parses not operator" in
let comparing = "not x" in
let output = ["NOT";"ID: x";] in
run_test test_case comparing output;

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
