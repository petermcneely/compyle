(*
Boiler plate set up for running tests   
*)
let passed_tests = ref 0
let count_tests = ref 0;;

let parse_program (input : string) =
  let lexbuf = Lexing.from_string input in
  let indentation_manager =
    Compyle.Lexing_stack.create_indentation_manager ()
  in
  let ast =
    Compyle.Parser.program (Compyle.Scanner.token indentation_manager) lexbuf
  in
  Compyle.Ast.string_of_program ast
in

let pass_test () =
  passed_tests := !passed_tests + 1;
  print_endline "\tYAY";
  assert true
in

let fail_test () =
  print_endline "\tOOPS";
  assert false
in

let run_test ?(debug : bool = false) (test_case : string) (input : string)
    (expected : string list) : unit =
  print_endline (test_case ^ ":");
  count_tests := !count_tests + 1;
  let parsed_program = parse_program input in
  if debug then print_endline ("parsed_program:\n" ^ parsed_program);
  let program_lines = String.split_on_char '\n' parsed_program in
  if debug then
    print_endline
      ("parsed_program line count: " ^ string_of_int (List.length program_lines));
  if debug then
    print_endline
      ("expected line count: " ^ string_of_int (List.length expected));
  if
    List.equal
      (fun x y ->
        if debug then print_endline ("x: " ^ x ^ " | y: " ^ y);
        x = y)
      program_lines expected
  then pass_test ()
  else fail_test ()
in

(*
The actual test cases   
*)
let test_case =
  "Scans, parses, and generates the ast for the hello world program"
in
let program =
  "def add(x: int, y: int) -> int:\r\n\
   \"\"\"\r\n\n\
   this adds two integers\r\n\n\
   \"\"\"\r\n\n\
   \tsum: int\r\n\n\
   \tsum = x + y\r\n\n\
   \treturn sum\r\n\n\
   print(add(x,y))\r\n"
in
let expected =
  [
    "def add(x: int, y: int) -> int:";
    "sum: int";
    "sum = x + y";
    "return sum";
    "print(add(x, y))";
    "";
  ]
in
run_test ~debug:false test_case program expected;

let test_case = "Parses literals being added together" in
let addition = "3 + 5" in
let expected = [ addition; "" ] in
run_test ~debug:false test_case (addition ^ "\n") expected;

(*
Tests basic logical operators   
*)
let test_case = "Parses literals being compared" in
let comparing = "8 == 5 or 3 != 9" in
let expected = [ comparing; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses string literals" in
let comparing = "\"hello world\"" in
let expected = [ comparing; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses tuples" in
let comparing = "x: tuple\nx = (1, \"hello world\", true)" in
let expected = [ "x: tuple"; "x = (1, \"hello world\", true)"; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses arrays" in
let comparing = "x: bool[]\nx = [True, False]" in
let expected = [ "x: bool[]"; "x = [True, False]"; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses Ints" in
let comparing = "x: int\nx = 40" in
let expected = [ "x: int"; "x = 40"; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses Floats" in
let comparing = "y: float\ny = 40.\ny = 40.40\ny = 40E3\ny = 40.3e-2" in
let expected =
  [ "y: float"; "y = 40."; "y = 40.4"; "y = 40000."; "y = 0.403"; "" ]
in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses Strings" in
let comparing = "x: string\nx = \"Hello World\"" in
let expected = [ "x: string"; "x = \"Hello World\""; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses Bools" in
let comparing = "x: bool\nx = True" in
let expected = [ "x: bool"; "x = True"; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses Certain Math Operations" in
let comparing = "4 + 0 - 9 * 4 / 10 % 6 ** 9" in
let expected = [ comparing; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses Assign Operations" in
let comparing = "x: int\nx /= 4" in
let expected = [ "x: int"; "x /= 4"; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses Not Operations" in
let comparing = "not x" in
let expected = [ "not x"; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses In/Not IN Operations" in
let comparing = "True not in x" in
let expected = [ "True not in x"; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses If Statements" in
let comparing =
  "if x = 4:\r\n\n\
   \ty = 5\r\n\n\
   elif y = 6:\r\n\n\
   \tz = 6\r\n\n\
   else:\r\n\n\
   \tq = 7\r\n"
in
let expected =
  [ "if x = 4:"; "y = 5"; "else:"; "if y = 6:"; "z = 6"; "else:"; "q = 7"; "" ]
in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses For Loop" in
let comparing = "for x in [1, 2, 3]:\r\n\n\ty = 5\r\n\n\tbreak\r\n" in
let expected = [ "for x in [1, 2, 3]:"; "y = 5"; "break"; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses While Loop" in
let comparing = "while x > 5:\r\n\n\ty = 5\r\n\n\tcontinue\r\n" in
let expected = [ "while x > 5:"; "y = 5"; "continue"; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses multi-dimensional arrays" in
let comparing =
  "x: bool[][]\n\
   x = [[True, True], [False, False], [True, False], [False, True]]"
in
let expected =
  [
    "x: bool[][]";
    "x = [[True, True], [False, False], [True, False], [False, True]]";
    "";
  ]
in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses declaring and assigning on the same line" in
let comparing = "sum: int = 3 + 4" in
let expected = [ comparing; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses nonetype function call" in
let comparing = "def foo() -> None:\r\n\n\tprint(\"foo\")\r\n" in
let expected = [ "def foo() -> None:"; "print(\"foo\")"; "" ] in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Parses negative ints and floats" in
let comparing =
  "a: int = -3\nb: float = -3.0\nc: int = 3 - -1\nd: float = -3.0 - -3.0"
in
let expected =
  [
    "a: int = -3";
    "b: float = -3.";
    "c: int = 3 - -1";
    "d: float = -3. - -3.";
    "";
  ]
in
run_test ~debug:false test_case (comparing ^ "\n") expected;

(*
Boiler plate set up for processing the results of the tests   
*)
print_newline ();
print_int !passed_tests;
print_string "/";
print_int !count_tests;
print_endline " tests passed!";
let percentage_passed =
  float_of_int !passed_tests /. float_of_int !count_tests *. 100.
in
print_endline (string_of_float percentage_passed ^ "%")
