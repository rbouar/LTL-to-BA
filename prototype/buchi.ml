open Hashtbl
open Ltl

type state = ltlFormula list;;


type buchi = {
  alphabet : SetString.t;
  eval : (state, (state, SetString.t) Hashtbl.t) Hashtbl.t;
  final_states : state list ;
  init_states : state list;
}
