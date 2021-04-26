(* main.ml *)

(* programme principal *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let formula = Parser.start Lexer.main lexbuf in
  let _ = Printf.printf "%s\n" (Ltl.ltlFormula_to_string formula) in
  let states  = Buchi.get_states formula in
  Printf.printf "%s\n" (Buchi.states_to_string states)
