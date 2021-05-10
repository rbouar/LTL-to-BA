(* main.ml *)

(* programme principal *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let formula = Parser.start Lexer.main lexbuf in
  let buchi_automaton = Buchi.create_automaton formula in
  Export.buchi_to_dot buchi_automaton "buchi.dot"
