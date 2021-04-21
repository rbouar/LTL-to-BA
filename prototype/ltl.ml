type ltlFormula =
  | Var of string
  | Const of bool
  | Or of ltlFormula * ltlFormula
  | And of ltlFormula * ltlFormula
  | Not of ltlFormula
  | Next of ltlFormula
  | Until of ltlFormula * ltlFormula
