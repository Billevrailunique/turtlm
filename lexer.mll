{
  open Parser
  exception Error of string
}

let layout = [ ' ' '\t' ]
let num = ['0'-'9']
let hexcode = ['A'-'F' '0'-'9']
let id = ['a'-'z']['A'-'Z' '0'-'9' 'a'-'z']*

rule token = parse
  | layout  { token lexbuf }  
  | '\n' {Lexing.new_line lexbuf; token lexbuf}
  | "BaisserPinceau"  { DRAW_ON }
  | "LeverPinceau"  { DRAW_OFF }
  | "Avancer"  { MOVE }
  | "Tourner"  { TURN }
  | "Vrais"  {TRUE}
  | "Faux"  {FALSE}
  | "Var" { VAR }
  | "Si" { IF }
  | "==" {BOOL_EQUAL}
  | "<=" {LESS_EQUAL}
  | ">=" {MORE_EQUAL}
  | "!=" {NOT_EQUAL}
  | "<"  { LESS }
  | ">"  {MORE}
  | "Faire" { DO }
  | "Alors" {THEN}
  | "Sinon" {ELSE}
  | "Repeter"  {REPEAT}
  | "Fois" { MANY_TIMES }
  | "Tant que" {WHILE}
  | "Et" {AND}
  | "Not" {NOT}
  | "Ou" {OR}
  | "Debut" {START}
  | "Fin"  {END}
  | "=" {EGALE }
  | num+  { NUM (Lexing.lexeme lexbuf) }
  | "LargeurPinceau" { WIDTH_CHANGE }
  | "CouleurPinceau" { COLOR_CHANGE }
  | hexcode hexcode hexcode hexcode hexcode hexcode {HEX (Lexing.lexeme lexbuf)}
  | "rouge"  {RED}
  | "bleu" {BLUE}
  | "vert" {GREEN}
  | "noir" {BLACK}
  | "jaune" {YELLOW}
  | ')'	{ RPAREN }
  | '('	{ LPAREN }
  | '+' {PLUS}
  | '-' {MINUS}
  | '*' {TIME}
  | '/' {DIVIDE}
  | ';' {SEMICOLON}
  | id { ID (Lexing.lexeme lexbuf)}
  | eof { EOF }
  | _			{ raise (Error (Printf.sprintf "caractère inattendu : %c" (Lexing.lexeme_char lexbuf 0))) }