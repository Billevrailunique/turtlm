{
  open Parser
  exception Error of string
}

let layout = [ ' ' '\t' ]
let num = ['0'-'9']

rule token = parse
  | layout  { token lexbuf }  
  | '\n' {Lexing.new_line lexbuf; token lexbuf}
  | "BaisserPinceau"  { DRAW_ON }
  | "LeverPinceau"  { DRAW_OFF }
  | "Avancer"  { MOVE }
  | "Tourner"  { TURN }
  | num+  { NUM (Lexing.lexeme lexbuf) }
  | ')'	{ RPAREN }
  | '('	{ LPAREN }
  | '+' {PLUS}
  | '-' {MINUS}
  | '*' {TIME}
  | '/' {DIVIDE}
  | ';' {SEMICOLON}
  | eof { EOF }
  | _			{ raise (Error (Printf.sprintf "caractère inattendu : %c" (Lexing.lexeme_char lexbuf 0))) }