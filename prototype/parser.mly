%{
    open Ltl

    (* F. = True U . *)
    let rec future f =
      Until (Const true, f)

    (* G. = !(F !.) *)
    and globally f =
      Not (future (Not f))

    (* f1 W f2 = (f1 U f2) v G f1 *)
    and weak_until f1 f2 =
      Or (Until (f1, f2), globally f1)

    (* f1 R f2 = !(!f1 U !f2) *)
    and release f1 f2 =
      Not (Until (Not f1, Not f2));;

    (* f1 => f2 = !f1 v f2 *)
    let rec imply f1 f2 =
      Or (Not f1, f2)

    (* f1 <=> f2 = (f1 => f2) && (f2 => f1) *)
    and equivalent f1 f2 =
      And (imply f1 f2, imply f2 f1);;
%}

%token RPAREN LPAREN EOF

%token UNTIL NEXT NOT AND OR TRUE FALSE

/* opérateur LTL définit en tant que "macro" */
%token GLOBALLY FUTURE WEAK_UNTIL RELEASE

/* De même pour l'implication et l'équivalance */
%token IMPLY EQUIVALENT

%token<string> VAR

%start <Ltl.ltlFormula> start

%left OR
%left AND
%left UNTIL WEAK_UNTIL RELEASE
%left IMPLY EQUIVALENT
%nonassoc NOT NEXT GLOBALLY FUTURE

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
  | f1=formula WEAK_UNTIL f2=formula { weak_until f1 f2 }
  | f1=formula RELEASE f2=formula { release f1 f2 }
  | f1=formula IMPLY f2=formula { imply f1 f2 }
  | f1=formula EQUIVALENT f2=formula { equivalent f1 f2 }
  | FUTURE f=formula { future f } 
  | GLOBALLY f=formula { globally f } 
  | NEXT f=formula { Next f }
  | NOT f=formula  { Not f }
