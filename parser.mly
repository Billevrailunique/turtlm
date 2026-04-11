%{
open Ast
%}

%token EOF DRAW_OFF DRAW_ON DIVIDE TIME MINUS PLUS MOVE TURN TRUE AND OR FALSE RPAREN LPAREN SEMICOLON RED BLUE GREEN BLACK YELLOW COLOR_CHANGE WIDTH_CHANGE VAR EGALE IF THEN ELSE WHILE DO REPEAT MANY_TIMES NOT_EQUAL LESS_EQUAL MORE_EQUAL BOOL_EQUAL LESS MORE START END NOT
%token<string> HEX NUM ID



%left PLUS MINUS OR
%left TIME DIVIDE AND
%left NOT

%start<bloc_instruction> programme

%%

programme: b=bloc_instruction EOF { b }

bloc_instruction: l=separated_list(SEMICOLON, instruction) { l }

instruction: 
  | DRAW_ON { Draw_on }
  | DRAW_OFF { Draw_off }
  | MOVE c=expression { Move (c, $startpos) }
  | TURN c=expression { Turn c }
  | COLOR_CHANGE c=color { CouleurPinceau c }
  | WIDTH_CHANGE e=expression { LargeurPinceau (e,$startpos) }
  | VAR str=ID { VarDecla (str, $startpos) } 
  | VAR str=ID EGALE e=expression  { VarDeclaInit (str, e, $startpos) }
  | str=ID EGALE e=expression  { VarInit (str, e, $startpos) }
  | IF c=condition THEN START i1=bloc_instruction END ELSE START i2=bloc_instruction END  { IfThenElse (c, i1, i2) } 
  | IF c=condition THEN START i=bloc_instruction END { IfThen (c,i) }
  | WHILE c=condition DO START i=bloc_instruction END  { While (c,i) }
  | REPEAT n=NUM MANY_TIMES START i=bloc_instruction END { Repeat (n,i) }
  

%inline color: 
  | RED { Red }
  | BLUE { Blue }
  | GREEN { Green }
  | BLACK { Black }
  | YELLOW  { Yellow }
  | v=HEX  { Hexcode v }

condition: 
  | TRUE {True}
  | FALSE {False}
  | e1=expression op=op_num e2=expression {TestBool (e1, op, e2)}
  | NOT c=condition  { Not c}
  | c1=condition AND c2=condition  {And (c1, c2)} 
  | c1=condition OR c2=condition {Or (c1, c2)}
  | LPAREN c=condition RPAREN  {c}

%inline op_num:
  | LESS {Less}
  | MORE {More}
  | LESS_EQUAL {Less_equal}
  | MORE_EQUAL {More_equal}
  | BOOL_EQUAL {Bool_equal}
  | NOT_EQUAL  {Not_equal}

expression: 
  | n=NUM { Valeur n }
  | LPAREN e=expression RPAREN { e }
  | l=expression op=operateur r=expression { Op (l, op, r, $startpos) }
  | str=ID  { Var (str,$startpos) }

%inline operateur:
  | PLUS { Plus }
  | MINUS { Minus }
  | TIME { Time }
  | DIVIDE { Divided }