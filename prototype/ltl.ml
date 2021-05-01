module SetString = Set.Make(String);;

type ltlFormula =
  | Const of bool
  | Var of string
  | Or of ltlFormula * ltlFormula
  | And of ltlFormula * ltlFormula
  | Not of ltlFormula
  | Next of ltlFormula
  | Until of ltlFormula * ltlFormula

let rec ltlFormula_to_string = function
  | Const b -> string_of_bool b
  | Var v -> v
  | Or (f1, f2) -> "(" ^ ltlFormula_to_string f1 ^ " OR " ^ ltlFormula_to_string f2 ^ ")"
  | And (f1, f2) -> "(" ^ ltlFormula_to_string f1 ^ " AND " ^ ltlFormula_to_string f2 ^ ")"
  | Not f -> "(NOT " ^ ltlFormula_to_string f ^ ")"
  | Next f -> "(NEXT " ^ ltlFormula_to_string f ^ ")"
  | Until (f1, f2) -> "(" ^ ltlFormula_to_string f1 ^ " UNTIL " ^ ltlFormula_to_string f2 ^ ")"


let rec get_variables = function
  | Const _ -> SetString.empty
  | Var v -> SetString.singleton v
  | Or (f1,f2) | And (f1,f2) | Until (f1,f2) -> SetString.union (get_variables f1) (get_variables f2)
  | Next f | Not f -> get_variables f

let rec equals ltlf1 ltlf2 = match ltlf1, ltlf2 with
  | Const b1, Const b2 -> b1 = b2
  | Var s1, Var s2 -> s1 = s2
  | Or (ol1, or1), Or (ol2, or2) -> (equals ol1 ol2) && (equals or1 or2)
  | And (al1, ar1), And (al2, ar2) -> (equals al1 al2) && (equals ar1 ar2)
  | Not no1, Not no2 -> equals no1 no2
  | Next ne1, Next ne2 -> equals ne1 ne2
  | Until (ul1, ur1), Until (ul2, ur2) -> (equals ul1 ul2) && (equals ur1 ur2)
  | _, _ -> false

let get_list_of_untils f =
  let rec get_list_of_untils_aux f acc = match f with
    | Or (f1, f2)
    | And (f1, f2) ->
      get_list_of_untils_aux f1 (get_list_of_untils_aux f2 acc)
    | Next f
    | Not f -> get_list_of_untils_aux f acc
    | Until (f1, f2) ->
      let acc = get_list_of_untils_aux f1 (f :: acc) in
      get_list_of_untils_aux f2 acc
    | _ -> acc in
  get_list_of_untils_aux f []
