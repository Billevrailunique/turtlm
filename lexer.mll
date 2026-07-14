{
  open Parser
  exception Error of string
}

let layout = [ ' ' '\t' ]
let chars = ['a'-'z' '0'-'9' 'A'-'Z']
let str = ['A'-'Z' '0'-'9' 'a'-'z' ' ' ''']+

rule token = parse
  | layout  { token lexbuf }  
  | '\n' {Lexing.new_line lexbuf; token lexbuf}
  | "BaisserPinceau"  { DRAW_ON }
  | "GenN" { GENN }
  | "Afficher" { PRINT }
  | "<" str ">" { TXT (Lexing.lexeme lexbuf) }
  | "GenC" { GENC }
  | "LeverPinceau"  { DRAW_OFF }
  | "Avancer"  { MOVE }
  | "Tourner"  { TURN }
  | "Vrais" 
  | "Faux"  { VALBOOL (Lexing.lexeme lexbuf) }
  | "Var" { VAR }
  | ","  {COMA}
  | "Def" { DEF }
  | "Retourn"  {RETURN}
  | "Si" { IF }
  | "==" {BOOL_EQUAL}
  | "<=" {LESS_EQUAL}
  | ">=" {MORE_EQUAL}
  | "!=" {NOT_EQUAL}
  | "<"  { LESS }
  | ">"  {MORE}
  | "mod" {MODULO}
  | "Faire" { DO }
  | "Alors" {THEN}
  | "Sinon" {ELSE}
  | "Repeter"  {REPEAT}
  | "Fois" { MANY_TIMES }
  | "Tant que" {WHILE}
  | "Et" {AND}
  | "Non" {NOT}
  | "Ou" {OR}
  | "Debut" {START}
  | "Fin"  {END}
  | "=" {EGALE }
  | "LargeurPinceau" { WIDTH_CHANGE }
  | "CouleurPinceau" { COLOR_CHANGE }
  | "vert" 
  | "blanc"
  | "bleu" 
  | "rouge" 
  | "jaune" 
  | "noir" { COLOR (Lexing.lexeme lexbuf) }
  | ')'	{ RPAREN }
  | '('	{ LPAREN }
  | '[' { LCROCHET }
  | ']' {RCROCHET}
  | '.' {POINT}
  | '+' {PLUS}
  | '-' {MINUS}
  | '*' {TIME}
  | '/' {DIVIDE}
  | ';' {SEMICOLON}
  | chars+ { CHARS (Lexing.lexeme lexbuf)}
  | eof { EOF }
  | _			{ raise (Error (Printf.sprintf "caractère inattendu : %c" (Lexing.lexeme_char lexbuf 0))) }