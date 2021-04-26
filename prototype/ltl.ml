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
