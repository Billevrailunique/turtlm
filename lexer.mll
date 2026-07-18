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
  | "<" str ">" { TXT (Lexing.lexeme lexbuf) }
  | "BaisserPinceau"  
  | "LeverPinceau"  { DRAW (Lexing.lexeme lexbuf) }
  | "Afficher" 
  | "Avancer"  
  | "Tourner" 
  | "Retourn"  
  | "LargeurPinceau" 
  | "CouleurPinceau"  { SIMPLE (Lexing.lexeme lexbuf) } 
  | "Ou" 
  | '+'  {ADDITIF (Lexing.lexeme lexbuf)}
  | '-' {MINUS}
  | "Et" 
  | "mod" 
  | '*' 
  | '/' {MULTIPLICATIF (Lexing.lexeme lexbuf)}
  | "==" 
  | "<="
  | ">=" 
  | "!=" 
  | "<" 
  | ">"  {ORDRE (Lexing.lexeme lexbuf)}
  | "vert" 
  | "blanc"
  | "bleu" 
  | "rouge" 
  | "jaune" 
  | "noir" { COLOR (Lexing.lexeme lexbuf) }
  | "Vrais" 
  | "Faux"  { VALBOOL (Lexing.lexeme lexbuf) }
  | "GenN" { GENN }
  | "GenC" { GENC }
  | "Var" { VAR }
  | ","  {COMA}
  | "Def" { DEF }
  | "Si" { IF }
  | "Faire" { DO }
  | "Alors" {THEN}
  | "Sinon" {ELSE}
  | "Repeter"  {REPEAT}
  | "Fois" { MANY_TIMES }
  | "Tant que" {WHILE}
  | "Non" {NOT}
  | "Debut" {START}
  | "Fin"  {END}
  | "=" {EGALE }
  | ')'	{ RPAREN }
  | '('	{ LPAREN }
  | '[' { LCROCHET }
  | ']' {RCROCHET}
  | '.' {POINT}
  | ';' {SEMICOLON}
  | chars+ { CHARS (Lexing.lexeme lexbuf)}
  | eof { EOF }
  | _			{ raise (Error (Printf.sprintf "caractère inattendu : %c" (Lexing.lexeme_char lexbuf 0))) }