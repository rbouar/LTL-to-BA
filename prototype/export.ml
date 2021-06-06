open Buchi
open Ltl

let setstring_to_string set =
  "{" ^ String.concat ", " (SetString.elements set) ^ "}"

let remove_if_exist filename =
  if Sys.file_exists filename then Sys.remove filename

let split_states buchi =
  let state_equal = List.for_all2 equals in
  let is_in_states s = List.exists (state_equal s) in
  let rec fill_all states basic inits finals both = match states with
    | [] -> basic, inits, finals, both
    | s :: states ->
      if is_in_states s buchi.init_states then
        if is_in_states s buchi.final_states then
          fill_all states basic inits finals (s :: both)
        else fill_all states basic (s :: inits) finals both
      else if is_in_states s buchi.final_states then
        fill_all states basic inits (s :: finals) both
      else fill_all states (s :: basic) inits finals both
  in fill_all buchi.states [] [] [] []

let states_to_export_string buchi =
  let rec export_states_aux states color = match states with
    | [] -> ""
    | s :: states -> "\"" ^ state_to_string s ^ "\"" ^ "[color=" ^ color ^ "]\n" ^
    export_states_aux states color  in
  let basic, inits, finals, both = split_states buchi in
  export_states_aux basic "black" ^
  export_states_aux inits "cyan" ^
  export_states_aux finals "yellow" ^
  export_states_aux both "green"



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
(* Exporte un automate de Büchi au format dot *)
let buchi_to_dot buchi filename =
  let _ = remove_if_exist filename in
  let c_out = open_out filename in
  let _ = output_string c_out "digraph buchi_automaton {\n" in
  let _ = output_string c_out (states_to_export_string buchi) in
  let _ = export_transitions buchi.eval c_out in
  let _ = output_string c_out "}\n" in
  close_out c_out
