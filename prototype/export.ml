open Buchi
open Ltl

let setstring_to_string set =
  "{" ^ String.concat ", " (SetString.elements set) ^ "}"

let remove_if_exist filename =
  if Sys.file_exists filename then Sys.remove filename

let one_transition_to_string from_state to_state alpha_set =
  "\""
  ^ state_to_string from_state
  ^ "\" -> \""
  ^ state_to_string to_state
  ^ "\" [label=\"" ^ setstring_to_string alpha_set ^ "\"];\n"

let export_transitions transitions channel =
  Hashtbl.iter (fun from hash_dest ->
      (Hashtbl.iter
         (fun dest alph -> output_string channel (one_transition_to_string from dest alph))
         hash_dest))
        transitions

let buchi_to_dot buchi filename =
  let _ = remove_if_exist filename in
  let c_out = open_out filename in
  let _ = output_string c_out "digraph buchi_automaton {\n" in
  let _ = export_transitions buchi.eval c_out in
  let _ = output_string c_out "}\n" in
  close_out c_out

