(* main.ml *)

(* programme principal *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let formula = Parser.start Lexer.main lexbuf in
  let _ = Printf.printf "%s\n" (Ltl.ltlFormula_to_string formula) in
  let states  = Buchi.get_states formula in
  let _ = Printf.printf "Q=%s\n" (Buchi.states_to_string states) in
  let init_states = Buchi.get_initial_states formula states in
  let final_states = Buchi.get_final_states formula states in
  Printf.printf "Q0=%s\nF=%s\n" (Buchi.states_to_string init_states)
    (Buchi.states_to_string final_states)
