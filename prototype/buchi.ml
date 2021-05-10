open Ltl

type state = ltlFormula list;;

exception InvalidTransition of state * ltlFormula * state;;


let state_to_string state =
  "{" ^ String.concat ", " (List.map ltlFormula_to_string state) ^ "}"

let states_to_string states =
  "{" ^ String.concat ", " (List.map state_to_string states) ^ "}"  




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

(* Automate de Buchi (non déterministe) *)
type buchi = {
    (* Ensemble fini représentant l'alphabet *)
    alphabet : SetString.t;    
    eval : (state, (state, SetString.t) Hashtbl.t) Hashtbl.t;
    
    (* Ensemble fini contenant tous les états *)
    states : state list;
    final_states : state list;
    init_states : state list;
  }

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

let get_initial_states f states =
  let rec get_initial_states_aux f states acc = match states with
    | [] -> acc
    | s :: states ->
      let acc = if is_in_state s f
        then s :: acc
        else acc in
      get_initial_states_aux f states acc in
  get_initial_states_aux f states []



let get_final_states f states =
  let untils = get_list_of_untils f in
  let rec is_final_states s l = match l with
    | [] -> true
    | (Until (_, f2) as f) :: l ->
      if is_in_state s f && (not (is_in_state s f2)) then false
      else is_final_states s l
    | _ -> failwith "Impossible" in
  let rec get_final_states_aux states acc = match states with
    | [] -> acc
    | s :: states ->
      if is_final_states s untils
      then get_final_states_aux states (s :: acc)
      else get_final_states_aux states acc in
  get_final_states_aux states []


let create_transition_state_to_state from_state to_state =
  let step set ltl =
    match ltl with
    | Const false as f -> raise (InvalidTransition (from_state, f, to_state))
    | Var s -> SetString.add s set
    | Or (f1, f2) as f-> if (List.mem f1 to_state) || (List.mem f2 to_state)
                         then set
                         else raise (InvalidTransition (from_state, f, to_state))
    | And (f1, f2) as f-> if (List.mem f1 to_state) && (List.mem f2 to_state)
                          then set
                          else raise (InvalidTransition (from_state, f, to_state))
    | Next f as f' -> if List.mem f to_state
                      then set
                      else raise (InvalidTransition (from_state, f', to_state))
    | Until (f1, f2) as f -> if (List.mem f2 to_state) || ((List.mem f1 from_state) && (List.mem f to_state))
                             then set
                             else raise (InvalidTransition (from_state, f, to_state))
    
    | _ -> set in
  List.fold_left step SetString.empty from_state

let create_all_transitions states =
  let n = List.length states in
  let transitions = Hashtbl.create n in
  let _ = List.iter (fun from_state -> let transition' = Hashtbl.create n in
                               let _ = List.iter (fun to_state -> try let set = create_transition_state_to_state from_state to_state in
                                                                      Hashtbl.add transition' to_state set;
                                                                  with _ -> ())
                                         states in
                               Hashtbl.add transitions from_state transition';)
            states in
  transitions;;
                                                                  
let create_automaton ltl =
  let alphabet = get_variables ltl in
  let states = get_states ltl in
  let init_states = get_initial_states ltl states in
  let final_states = get_final_states ltl states in
  let eval = create_all_transitions states in
  { alphabet; eval; states; final_states; init_states }  
