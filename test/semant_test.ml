(*
Boiler plate set up for running tests   
*)
let passed_tests = ref 0;;
let count_tests = ref 0;;

let parse_program (input: string) =
  let lexbuf = Lexing.from_string input in
  let indentation_manager = Compyle.Lexing_stack.create_indentation_manager() in
  let ast = Compyle.Parser.program (Compyle.Scanner.token indentation_manager) lexbuf in
  let sast = Compyle.Semant.check ast in
  Compyle.Sast.string_of_sprogram sast in

let pass_test () =
  passed_tests := (!passed_tests + 1);
  print_endline "\tYAY";
  assert true
in
  
let fail_test () =
  print_endline "\tOOPS";
  assert false
in
  
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
    fail_test() in

(*
The actual test cases   
*)
let test_case = "Scans, parses, and generates the ast for the hello world program" in
let program = "\
def add(x: int, y: int) -> int:\r\n\
\"\"\"\r\n
this adds two integers\r\n
\"\"\"\r\n
\tsum: int\r\n
\tsum = x + y\r\n
\treturn sum\r\n
x: int = 5\r\n
y: int = 6\r\n
print(add(x,y))\r\n" in
let expected = ["def add(x: int, y: int) -> int:"; "sum: int"; "(int : sum = (int : (int : x) + (int : y)))"; "return (int : sum)"; "x: int(int : 5)"; "y: int(int : 6)"; "print((int : add((int : x), (int : y))))"; "" ] in
run_test ~debug:false test_case program expected;

let test_case = "Semantically checks int literal" in
let addition = "5" in
let expected = ["(int : 5)"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks float literal" in
let addition = "40.3e-2" in
let expected = ["(float : 0.403)"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks string literal" in
let addition = "\"Hello World\"" in
let expected = ["(string : \"Hello World\")"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks bool literal" in
let addition = "True" in
let expected = ["(bool : True)"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks array literal" in
let addition = "[2, 3]" in
let expected = ["(int[] : [(int : 2), (int : 3)])"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks multi-dim array literal" in
let addition = "[[2,3],[3,4]]" in
let expected = ["(int[][] : [(int[] : [(int : 2), (int : 3)]), (int[] : [(int : 3), (int : 4)])])"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks multi-dim tuple literal" in
let addition = "((4,True),(5,False))" in
let expected = ["(tuple : ((tuple : ((int : 4), (bool : True))), (tuple : ((int : 5), (bool : False)))))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks tuple literal" in
let addition = "(4, 4.)" in
let expected = ["(tuple : ((int : 4), (float : 4.)))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks math binary operation for int" in
let addition = "4 + 4" in
let expected = ["(int : (int : 4) + (int : 4))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks math binary operation for float" in
let addition = "4. + 4." in
let expected = ["(float : (float : 4.) + (float : 4.))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks and/or binary operation" in
let addition = "True and False or True" in
let expected = ["(bool : (bool : (bool : True) and (bool : False)) or (bool : True))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks eq/neq binary operation" in
let addition = "True==True" in
let expected = ["(bool : (bool : True) == (bool : True))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks gt/lt/geq/leq binary operation" in
let addition = "4 < 4" in
let expected = ["(bool : (int : 4) < (int : 4))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks ids and assignment" in
let addition = "x: float = 4." in
let expected = ["x: float(float : 4.)"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks id" in
let addition = "x: float" in
let expected = ["x: float"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks AugAsn" in
let addition = "x: int = 5\n x += 5" in
let expected = ["x: int(int : 5)"; "(int : x += (int : 5))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks Not operator" in
let addition = "not True" in
let expected = ["(bool : not (bool : True))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks NotIn operator" in
let addition = "5 not in [5, 2]" in
let expected = ["(bool : (int : 5) not in (int[] : [(int : 5), (int : 2)]))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks In operator" in
let addition = "5 in [5, 2]" in
let expected = ["(bool : (int : 5) in (int[] : [(int : 5), (int : 2)]))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks function definition" in
let addition = "def add(x: int, y: int) -> int:\r\n
\treturn y\r\n" in
let expected = ["def add(x: int, y: int) -> int:"; "return (int : y)"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks Return" in
let addition = "x: int = 5\n return x" in
let expected = ["x: int(int : 5)"; "return (int : x)"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks If Statement" in
let comparing = "if True:\r\n
\treturn 0\r\n
elif False:\r\n
\treturn 1\r\n
else:\r\n
\treturn 2\r\n" in
let expected = ["if (bool : True):"; "return (int : 0)"; "else:"; "if (bool : False):"; "return (int : 1)"; "else:"; "return (int : 2)"; ""] in 
run_test ~debug:false test_case (comparing ^ "\n") expected;

let test_case = "Semantically checks while loop" in
let addition = "while True:\r\n
\treturn 0\r\n" in
let expected = ["while (bool : True):"; "return (int : 0)"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks while loop" in
let addition = "for x in [1, 2, 3]:\r\n
\ty: int = 5\r\n
\tbreak\r\n" in
let expected = ["for x in (int[] : [(int : 1), (int : 2), (int : 3)]):"; "y: int(int : 5)"; "break"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks print" in
let addition = "print(\"string\")" in
let expected = ["print((string : \"string\"))"; ""] in
run_test ~debug:false test_case (addition ^ "\n") expected;

let test_case = "Semantically checks nonetype function call" in
let addition = "def foo() -> nonetype:\r\n
\tprint(\"foo\")\r\n" in
let expected = ["def foo() -> nonetype:"; "print((string : \"foo\"))"; ""] in
run_test ~debug:true test_case (addition ^ "\n") expected;

(*
Boiler plate set up for processing the results of the tests   
*)
print_newline();
print_int !passed_tests; print_string "/"; print_int(!count_tests); print_endline " tests passed!";
let percentage_passed = (float_of_int !passed_tests) /. (float_of_int !count_tests) *. 100. in
print_endline (string_of_float percentage_passed ^ "%");
