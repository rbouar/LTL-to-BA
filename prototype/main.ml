(* main.ml *)

(* programme principal *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let formula = Parser.start Lexer.main lexbuf in
  let buchi_automaton = Buchi.create_automaton formula in
  let _ = Printf.printf "Nombre d'états: %d\n"
      (List.length buchi_automaton.states) in
  let _ = Printf.printf "Nombre de transitions: %d\n"
      (Hashtbl.fold (fun _ y c -> c + (Hashtbl.length y)) buchi_automaton.eval 0) in
  Export.buchi_to_dot buchi_automaton "buchi.dot"
