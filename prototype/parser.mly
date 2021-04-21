%{
    open Ltl
%}

%token RPAREN LPAREN UNTIL NEXT NOT AND OR TRUE FALSE EOF
%token<string> VAR

%start <Ltl.ltlFormula> start

%left OR
%left AND
%left UNTIL
%nonassoc NOT NEXT

%%

start:
  | f=formula EOF { f }

formula:
  | TRUE   { Const true }
  | FALSE  { Const false }
  | v=VAR  { Var v }
  | LPAREN f=formula RPAREN { f }
  | f1=formula OR f2=formula    { Or (f1, f2) }
  | f1=formula AND f2=formula   { And (f1, f2) }
  | f1=formula UNTIL f2=formula { Until (f1, f2) }
  | NEXT f=formula { Next f }
  | NOT f=formula  { Not f }
