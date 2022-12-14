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
  let sast = Compyle.Semant.check ast in
  Compyle.Sast.string_of_sprogram sast
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

let build_main body =
  "def main() -> int:\r\n\t" ^ body ^ "\r\n\treturn 0\r\n"
in

let build_expected_main expected =
  [ "def main() -> int:" ] @ expected @ [ "return (int : 0)"; "" ]
in

(*
The actual test cases   
*)
let test_case =
  "Scans, parses, semantically checks, and generates the sast for the hello world program"
in
let actual =
  "def add(x: int, y: int) -> int:\r\n\
   \"\"\"\r\n\n\
   this adds two integers\r\n\n\
   \"\"\"\r\n\n\
   \tsum: int\r\n\n\
   \tsum = x + y\r\n\n\
   \treturn sum\r\n\n\
   \r\n\n\
   def main() -> int:\r\n\
   \tx: int = 5\r\n\n\
   \ty: int = 6\r\n\n\
   \tprint(add(x,y))\r\n"
in
let expected =
  [
    "def add(x: int, y: int) -> int:";
    "sum: int";
    "(int : sum = (int : (int : x) + (int : y)))";
    "return (int : sum)";
    "def main() -> int:";
    "x: int(int : 5)";
    "y: int(int : 6)";
    "(int : print((int : add((int : x), (int : y)))))";
    "";
  ]
in
run_test ~debug:false test_case actual expected;

let test_case = "Semantically check variables in different scopes" in
let actual =
  "c: int = 4\r\n\n\
   def main() -> int:\r\n\
   \tb: int = 3\r\n\n\
   \tif True:\r\n\n\
   \t\ta: int = 2\r\n\n\
   \t\treturn a\r\n\n\
   \telse:\r\n\n\
   \t\treturn b\r\n"
in
let expected =
  [
    "c: int(int : 4)";
    "def main() -> int:";
    "b: int(int : 3)";
    "if (bool : True):";
    "a: int(int : 2)";
    "return (int : a)";
    "else:";
    "return (int : b)";
    "";
  ]
in
run_test ~debug:false test_case actual expected;

let test_case = "Semantically checks int literal" in
let actual = build_main "5" in
let expected = build_expected_main [ "(int : 5)" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks float literal" in
let actual = build_main "40.3e-2" in
let expected = build_expected_main [ "(float : 0.403)" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks string literal" in
let actual = build_main "\"Hello World\"" in
let expected = build_expected_main [ "(string : \"Hello World\")" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks bool literal" in
let actual = build_main "True" in
let expected = build_expected_main [ "(bool : True)" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks array literal" in
let actual = build_main "[2, 3]" in
let expected = build_expected_main [ "(int[] of length 2 : [(int : 2), (int : 3)])" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks tuple literal declaration of correct types" in
let actual = build_main "x: tuple<int, float> = (4, 4.)" in
let expected = build_expected_main [ "x: tuple<int, float>(tuple<int, float> : ((int : 4), (float : 4.)))" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks reject tuple literal declaration of incorrect types" in
let actual = build_main "x: tuple<int, float> = (True, 4.)" in
try run_test ~debug:false test_case (actual ^ "\n") []
with
| Failure e when e = "Incompatible type. Expected Var type: tuple<int, float> Received expression type: tuple<bool, float>"
-> pass_test ();

let test_case = "Semantically checks tuple literal assignment of correct types" in
let actual = build_main "x: tuple<int, float>\n\tx = (4, 4.0)" in
let expected = build_expected_main [ "x: tuple<int, float>";"(tuple<int, float> : x = (tuple<int, float> : ((int : 4), (float : 4.))))" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks reject tuple literal assignment of incorrect types" in
let actual = build_main "x: tuple<int, float>\n\tx = (True, 4.0)" in
try run_test ~debug:false test_case (actual ^ "\n") []
with
| Failure e when e = "Incompatible type. Expected Var type: tuple<int, float> Received expression type: tuple<bool, float>"
-> pass_test ();

let test_case = "Semantically checks math binary operation for int" in
let actual = build_main "4 + 4" in
let expected = build_expected_main [ "(int : (int : 4) + (int : 4))" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks math binary operation for float" in
let actual = build_main "4. + 4." in
let expected =
  build_expected_main [ "(float : (float : 4.) + (float : 4.))" ]
in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks and/or binary operation" in
let actual = build_main "True and False or True" in
let expected =
  build_expected_main
    [ "(bool : (bool : (bool : True) and (bool : False)) or (bool : True))" ]
in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks eq/neq binary operation" in
let actual = build_main "True==True" in
let expected =
  build_expected_main [ "(bool : (bool : True) == (bool : True))" ]
in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks gt/lt/geq/leq binary operation" in
let actual = build_main "4 < 4" in
let expected = build_expected_main [ "(bool : (int : 4) < (int : 4))" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks ids and assignment" in
let actual = build_main "x: float = 4." in
let expected = build_expected_main [ "x: float(float : 4.)" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks id" in
let actual = build_main "x: float" in
let expected = build_expected_main [ "x: float" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks AugAsn" in
let actual = build_main "x: int = 5\n\tx += 5" in
let expected =
  build_expected_main [ "x: int(int : 5)"; "(int : x += (int : 5))" ]
in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks Not operator" in
let actual = build_main "not True" in
let expected = build_expected_main [ "(bool : not (bool : True))" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks If Statement" in
let comparing =
  "def main() -> int:\r\n\
   \tif True:\r\n\
   \t\treturn 0\r\n\
   \telif False:\r\n\
   \t\treturn 1\r\n\
   \telse:\r\n\
   \t\treturn 2\r\n"
in
let expected =
  [
    "def main() -> int:";
    "if (bool : True):";
    "return (int : 0)";
    "else:";
    "if (bool : False):";
    "return (int : 1)";
    "else:";
    "return (int : 2)";
    "";
  ]
in
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Semantically checks while loop" in
let actual = build_main "while True:\r\n\t\treturn 11" in
let expected =
  build_expected_main [ "while (bool : True):"; "return (int : 11)" ]
in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks for loop" in
let actual = build_main "for x in [1, 2, 3]:\r\n\t\ty: int = 5\r\n\t\tbreak" in
let expected =
  build_expected_main
    [
      "for x in (int[] of length 3 : [(int : 1), (int : 2), (int : 3)]):";
      "y: int(int : 5)";
      "break";
    ]
in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks print" in
let actual = build_main "print(\"string\")" in
let expected = build_expected_main [ "(int : print((string : \"string\")))" ] in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks nonetype function call" in
let addition =
  "def foo() -> None:\r\n\tprint(\"foo\")\r\ndef main() -> int:\r\n\tfoo()\r\n"
in
let expected =
  [
    "def foo() -> None:";
    "(int : print((string : \"foo\")))";
    "def main() -> int:";
    "(None : foo())";
    "";
  ]
in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks assignment to nonetype" in
let addition =
  "def foo() -> None:\r\n\n\tprint(\"foo\")\r\n\nx: int\r\n\nx = foo()\r\n"
in
let expected = [ "def foo() -> None:"; "(int : print((string : \"foo\")))"; "" ] in
try run_test ~debug:false test_case (addition ^ "\n") expected
with Failure e when e = "Cannot assign variable to nonetype" -> pass_test ();

let test_case = "Semantically checks augmented assignment to nonetype" in
let addition =
  "def foo() -> None:\r\n\n\
    \tprint(\"foo\")\r\n\n\
    x: int = 4\r\n\n\
    x += foo()\r\n"
in
let expected = [ "def foo() -> None:"; "(int : print((string : \"foo\")))"; "" ] in
try run_test ~debug:false test_case (addition ^ "\n") expected
with Failure e when e = "Cannot assign variable to nonetype" -> pass_test ();

let test_case = "Semantically checks declaration to nonetype" in
let addition =
  "def foo() -> None:\r\n\
    \tprint(\"foo\")\r\n\
    def main() -> int:\r\n\
    \tx: int = foo()\r\n"
in
try run_test ~debug:false test_case (addition ^ "\n") []
with
| Failure e
when e
      = "Incompatible type. Expected Var type: int Received expression type: \
        None"
-> pass_test ();

let test_case = "Prevents nested function definitions" in
let actual =
  "def main() -> int:\r\n\
    \tdef foo() -> None:\r\n\
    \t\tprint(\"yay\")\r\n\
    \treturn 0\r\n"
in
try run_test ~debug:true test_case (actual ^ "\n") []
with
| Failure e
when e
      = "Nested function definitions are not allowed. Received a \
        definition for a function named: foo"
-> pass_test ();

let test_case = "Enforces that main should have zero parameters" in
let actual = "def main(x: int) -> int:\r\n\treturn 0\r\n" in
try run_test ~debug:false test_case (actual ^ "\n") []
with
| Failure e
when e
      = "The main function should take zero arguments and return an int"
-> pass_test ();

let test_case = "Enforces that main should return an int" in
let actual = "def main() -> bool:\r\n\treturn True\r\n" in
try run_test ~debug:false test_case (actual ^ "\n") []
with
| Failure e
when e
      = "The main function should take zero arguments and return an \
        int"
-> pass_test ();

let test_case = "Parses negative ints and floats" in
let actual =
  build_main
    "a: int = -3\n\
      \tb: float = -3.0\n\
      \tc: int = 3 - -1\n\
      \td: float = -3.0 - -3.0"
in
let expected =
  build_expected_main
    [
      "a: int(int : -3)";
      "b: float(float : -3.)";
      "c: int(int : (int : 3) - (int : -1))";
      "d: float(float : (float : -3.) - (float : -3.))";
    ]
in
run_test ~debug:false test_case (actual ^ "\n") expected;

let test_case = "Semantically checks return type of function" in
let actual = "def main() -> int:\r\n\treturn 1.0" in
try run_test ~debug:true test_case (actual ^ "\n") []
with
| Failure e
when e
      = "Function 'main' expects a return type of int, but \
        currently returns type float"
-> pass_test ();

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
