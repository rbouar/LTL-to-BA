(* main.ml *)

(* programme principal *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let formula = Parser.start Lexer.main lexbuf in
  let buchi_automaton = Buchi.create_automaton formula in
  
  let _ = Printf.printf "%s\n" (Ltl.ltlFormula_to_string formula) in
  Printf.printf "Q = %s\nQ0 = %s\nF = %s\n"
    (Buchi.states_to_string buchi_automaton.states)
    (Buchi.states_to_string buchi_automaton.init_states)
    (Buchi.states_to_string buchi_automaton.final_states)
