%{
open Ast
%}

%token EOF GENC GENN POINT MINUS LCROCHET RCROCHET COMA DEF RPAREN LPAREN SEMICOLON VAR EGALE IF THEN ELSE WHILE DO REPEAT MANY_TIMES START END NOT
%token<string> CHARS TXT COLOR VALBOOL DRAW SIMPLE ADDITIF MULTIPLICATIF ORDRE

(*déclaration de la prédéscence*)
%left ADDITIF MINUS
%left MULTIPLICATIF
%nonassoc ORDRE
%right NOT 
%nonassoc POINT

(*axiome de la grammaire*)
%start<bloc_instruction> programme

(*si erreur et que le lookahead montre une expression, la réduire -> meilleur message d'erreur*)
%on_error_reduce expression

%%

(*règle*)

programme: b=bloc_instruction EOF { b }

bloc_instruction:
  |    { [] }
  | i=instruction SEMICOLON b=bloc_instruction { i::b }

sous_bloc :
  | START b=bloc_instruction END { b }

instruction: 
  | s=DRAW { Draw s }
  | s=SIMPLE e=expression { Simple (s,e,$startpos) }
  | WHILE e=expression DO s=sous_bloc { While (e,s,$startpos) }
  | IF e=expression THEN s=sous_bloc{ IfThen (e,s,$startpos) }
  | IF e=expression THEN s1=sous_bloc ELSE s2=sous_bloc{ IfThenElse (e,s1,s2,$startpos) }
  | REPEAT e=expression MANY_TIMES s=sous_bloc { Repeat (e,s,$startpos) }
  | VAR str=CHARS { VarDecla (str, $startpos) } 
  | VAR str=CHARS EGALE t=expression  { VarDeclaInit (str, t, $startpos) }
  | str=CHARS EGALE t=expression  { VarInit (str, t, $startpos) }
  | DEF n=CHARS LPAREN a=separated_list(COMA, CHARS) RPAREN i=sous_bloc { FunDecla (n, a, i, $startpos) }
  | n=CHARS LPAREN a=separated_list(COMA, expression) RPAREN  {ProcCall (n,a, $startpos)}
  |str=CHARS POINT i=expression EGALE e=expression { Set (str,i,e,$startpos) }

expression: 
  | e=minus_unaire_possible { e }
  | MINUS e=minus_unaire_possible { Op (NumOrVarOrHexa ("0", $startpos), Add "-", e, $startpos) }
  | l=expression op=op_bin r=expression { Op (l, op, r, $startpos) } 
  | b=VALBOOL { ValBool b }
  | NOT c=expression  { Not (c,$startpos)}
  | c=COLOR {Color c}
  | GENC LPAREN a=option(expression) RPAREN  { GenC (a,$startpos) }
  | str=TXT { Text str } 
  | LCROCHET a=separated_list(COMA, expression) RCROCHET  { Liste a }

minus_unaire_possible:
  | str=CHARS { NumOrVarOrHexa (str,$symbolstartpos) } 
  | LPAREN e=expression RPAREN { e }
  | GENN LPAREN a=separated_list(COMA, expression) RPAREN {GenN (a,$startpos)}
  | n=CHARS LPAREN a=separated_list(COMA, expression) RPAREN {FunCall (n,a, $startpos)}
  | str=CHARS POINT e=expression { Get (str, e, $startpos) }

%inline op_bin:
  | o=ADDITIF {Add o}
  | MINUS {Add "-"}
  | o=MULTIPLICATIF {Mult o}
  | o=ORDRE {Ordre o}