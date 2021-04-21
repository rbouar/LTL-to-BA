(* main.ml *)

(* programme principal *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  Parser.start Lexer.main lexbuf
