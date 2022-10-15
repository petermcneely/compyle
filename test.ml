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
  if List.equal (fun x y -> if debug then print_endline ("actual: " ^ x ^ "; excepted: " ^ y); x = y) parsed_program output then
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
Parses for loop
*)
let test_case = "Parses for loop" in
let for_loop = "count = 0\n\
                for i in range(1, 10):\n\
                  \tcount = count + 1" in
let output = [
  "ID: count"; "ASSIGN"; "LITERAL: 0"; "NEWLINE";
  "FOR"; "ID: i"; "IN"; "RANGE"; "LPAREN"; "LITERAL: 1"; "COMMA"; "LITERAL: 10"; "RPAREN"; "COLON"; "NEWLINE";
  "INDENT"; "ID: count"; "ASSIGN"; "ID: count"; "PLUS"; "LITERAL: 1"
] in
run_test test_case for_loop output;

(*
Parses while loop
*)
let test_case = "Parses while loop" in
let while_loop = "count = 0\n\
                  while count < 10:\n\
                    \tcount = count + 1" in
let output = [
  "ID: count"; "ASSIGN"; "LITERAL: 0"; "NEWLINE";
  "WHILE"; "ID: count"; "LT"; "LITERAL: 10"; "COLON"; "NEWLINE";
  "INDENT"; "ID: count"; "ASSIGN"; "ID: count"; "PLUS"; "LITERAL: 1"
] in
run_test test_case while_loop output;

(*
Parses break
*)
let test_case = "Parses break" in
let break = "count = 0\n\
            while True:\n\
              \tif count == 5:\n\
                \t\tbreak\n\
              \tcount = count + 1" in
let output = [
  "ID: count"; "ASSIGN"; "LITERAL: 0"; "NEWLINE";
  "WHILE"; "BOOL: true"; "COLON"; "NEWLINE";
  "INDENT"; "IF"; "ID: count"; "EQ"; "LITERAL: 5"; "COLON"; "NEWLINE";
  "INDENT"; "INDENT"; "BREAK"; "NEWLINE";
  "INDENT"; "ID: count"; "ASSIGN"; "ID: count"; "PLUS"; "LITERAL: 1"
] in
run_test test_case break output;

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
