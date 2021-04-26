open Ltl

type state = ltlFormula list;;

let rec is_in_state s ltlf = match s with
  | [] -> false
  | f :: s -> if equals f ltlf then true
    else is_in_state s ltlf

let add_ltl_to_state s ltlf =
  if is_in_state s ltlf then s
  else ltlf :: s

let rec add_all_ltl_to_state s l = match l with
  | [] -> s
  | f :: l -> add_all_ltl_to_state (add_ltl_to_state s f) l

type buchi = {
  alphabet : SetString.t;
  eval : (state, (state, SetString.t) Hashtbl.t) Hashtbl.t;
  states : state list;
  final_states : state list;
  init_states : state list;
}

(* let rec get_sub_formula f = *)
  (* let rec get_sub_formula_aux f acc = match f with *)
    (* | Const b -> add_all_ltl_to_state acc [(Const b); (Const !b)] *)
    (* | Var s -> add_all_ltl_to_state acc [(Var s); (Not Var s)] *)
    (* | Or f1, f2 -> *)
      (* let acc = add_all_ltl_to_state acc *)
          (* [f1; f2; Not f1; Not f2; f1 Or f2; Not (f1 Or f2)] in *)
      (* let acc = get_sub_formula_aux f1 acc in *)
      (* get_sub_formula_aux f2 acc *)
    (* | And f1, f2 -> *)
      (* let acc = add_all_ltl_to_state acc *)
          (* [f1; f2; Not f1; Not f2; f1 And f2; Not (f1 And f2)] in *)
      (* let acc = get_sub_formula_aux f1 acc in *)
      (* get_sub_formula_aux f2 acc *)
    (* | Until f1, f2 -> *)
      (* let acc = add_all_ltl_to_state acc *)
          (* [f1; f2; Not f1; Not f2; f1 Until f2; Not (f1 Until f2)] in *)
      (* let acc = get_sub_formula_aux f1 acc in *)
      (* get_sub_formula_aux f2 acc *)
    (* | Next f -> get_sub_formula_aux f *)
                  (* (add_all_ltl_to_state acc [Next f; Not (Next f)]) *)
    (* | Not f -> get_sub_formula_aux f *)
                 (* (add_all_ltl_to_state acc [f; Not f]) *)
  (* in get_sub_formula_aux f [] *)


let add_const_to_states b states =
  let rec add_const_to_states_aux b states acc = match states with
    | [] -> acc
    | s :: states' ->
      if is_in_state s (Const b) then acc
      else if is_in_state s (Const (not b)) then acc
      else let acc = (Const b :: s) :: ((Const (not b)) :: s) :: acc in
        add_const_to_states_aux b states' acc
  in if states = [] then [[Const b]; [Const (not b)]]
  else add_const_to_states_aux b states []

let add_var_to_states v states =
  let rec add_var_to_states_aux v states acc = match states with
    | [] -> acc
    | s :: states' ->
      if is_in_state s (Var v) then acc
      else if is_in_state s (Not (Var v)) then acc
      else let acc = (Var v :: s) :: ((Not (Var v)) :: s) :: acc in
        add_var_to_states_aux v states' acc
  in if states = [] then [[Var v]; [Not (Var v)]]
  else add_var_to_states_aux v states []

let add_or_to_states o states =
  let rec add_or_to_states_aux o states acc = match states with
    | [] -> acc
    | s :: states' ->
      if is_in_state s o then acc
      else if is_in_state s (Not o) then acc
      else match o with
        | Or (o1, o2) ->
          let acc = if is_in_state s o1 || is_in_state s o2
            then (o :: s) :: acc
            else ((Not o) :: s) :: acc
          in add_or_to_states_aux o states' acc
        | _ -> raise (Invalid_argument "Impossible")
  in add_or_to_states_aux o states []

let add_and_to_states a states =
  let rec add_and_to_states_aux a states acc = match states with
    | [] -> acc
    | s :: states' ->
      if is_in_state s a then acc
      else if is_in_state s (Not a) then acc
      else match a with
        | And (a1, a2) ->
          let acc = if is_in_state s a1 && is_in_state s a2
            then (a :: s) :: acc
            else ((Not a) :: s) :: acc
          in add_and_to_states_aux a states' acc
        | _ -> raise (Invalid_argument "Impossible")
  in add_and_to_states_aux a states []

let add_next_to_states n states =
  let rec add_next_to_states_aux n states acc = match states with
    | [] -> acc
    | s :: states' ->
      if is_in_state s n then acc
      else if is_in_state s (Not n) then acc
      else let acc = (n :: s) :: ((Not n) :: s) :: acc in
        add_next_to_states_aux n states' acc
  in add_next_to_states_aux n states []

let add_until_to_states u states =
  let rec add_until_to_states_rec u states acc = match states with
    | [] -> acc
    | s :: states' ->
      if is_in_state s u then acc
      else if is_in_state s u then acc
      else match u with
        | Until (u1, u2) ->
          let acc = if is_in_state s u2
            then (u :: s) :: acc
            else if not (is_in_state s u1)
            then ((Not u) :: s) :: acc
            else (u :: s) :: ((Not u) :: s) :: acc
          in add_until_to_states_rec u states' acc
        | _ -> raise (Invalid_argument "Impossible")
  in add_until_to_states_rec u states []



let rec add_to_all_states f states = match f with
  | Const b -> add_const_to_states b states
  | Var v -> add_var_to_states v states
  | Or (o1, o2) ->
    let states = add_to_all_states o2 (add_to_all_states o1 states) in
    add_or_to_states f states
  | And (a1, a2) ->
    let states = add_to_all_states a2 (add_to_all_states a1 states) in
    add_and_to_states f states
  | Not n -> add_to_all_states n states
  | Next n -> add_next_to_states f (add_to_all_states n states)
  | Until (u1, u2) ->
    let states = add_to_all_states u2 (add_to_all_states u1 states) in
    add_until_to_states f states


let get_states f = add_to_all_states f []

let state_to_string s =
  let rec state_to_string_aux s res = match s with
    | [] -> res ^ "}"
    | f :: s -> state_to_string_aux s (res ^ ", " ^ (ltlFormula_to_string f))
  in state_to_string_aux s "{"

let states_to_string states =
  let rec states_to_string_aux states res = match states with
    | [] -> res ^ "}"
    | s :: states -> states_to_string_aux states (res ^ ", " ^ (state_to_string s))
  in states_to_string_aux states "{"
