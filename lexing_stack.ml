(*
  https://docs.python.org/3/reference/lexical_analysis.html#indentation
  The Python LRM states that blocks are delimited by an INDENT token and a DEDENT token.
  A stack is used to manage the logic as to when to generate an INDENT or DEDENT token during
  lexical analysis.
*)

type indentation_manager = {
  stack: int Stack.t;
  mutable curr_indentation: int;
}

let create_indentation_manager () =
  let new_stack = Stack.create() in
  (*
    From the LRM: Before the first line of the file is read, a single zero is pushed on the stack;
    this will never be popped off again.
  *)
  Stack.push 0 new_stack; 
  let manager = {
    stack = new_stack;
    curr_indentation = 0;
  } in manager
