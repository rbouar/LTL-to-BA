{
   open Parser
}

let formatage	= [ ' ' '\t' '\n' ]
let minus_char 	= [ 'a'-'z' ]

let variable_name = minus_char+

rule main = parse
  | formatage	   { main lexbuf }
  | ')'		   { RPAREN }
  | '('		   { LPAREN }
  | 'U'		   { UNTIL }
  | 'X'		   { NEXT }
  | 'F'		   { FUTURE }
  | 'G'		   { GLOBALLY }
  | 'R'		   { RELEASE }
  | 'W'		   { WEAK_UNTIL }
  | "=>"	   { IMPLY }
  | "<=>"	   { EQUIVALENT }
  | '!'		   { NOT }
  | "&&"	   { AND }
  | "||"	   { OR }
  | '1'		   { TRUE }
  | '0'		   { FALSE }
  | variable_name  { VAR(Lexing.lexeme lexbuf) }
  | eof		   { EOF }
  | _		   { failwith "Unexpected character" }