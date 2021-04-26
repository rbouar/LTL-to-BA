(* main.ml *)

(* programme principal *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let formula = Parser.start Lexer.main lexbuf in
  Printf.printf "%s\n" (Ltl.ltlFormula_to_string formula)
