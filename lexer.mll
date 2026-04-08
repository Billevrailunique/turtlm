{
  open Parser
}

let layout = [ ' ' '\t' '\n' ]
let num = ['0'-'9']

rule token = parse
  | layout  { main lexbuf }  (* TODO *)
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
  | _			{ failwith "unexpected character" }