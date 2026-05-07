%{
open Ast
%}

%token EOF DRAW_OFF DRAW_ON GENC DIVIDE TIME GENN PRINT MINUS MODULO PLUS MOVE COMA TURN TRUE DEF RETURN AND OR FALSE RPAREN LPAREN SEMICOLON RED BLUE GREEN BLACK YELLOW COLOR_CHANGE WIDTH_CHANGE VAR EGALE IF THEN ELSE WHILE DO REPEAT MANY_TIMES NOT_EQUAL LESS_EQUAL MORE_EQUAL BOOL_EQUAL LESS MORE START END NOT
%token<string> HEX NUM ID TXT

%left PLUS MINUS OR
%left TIME DIVIDE AND MODULO
%nonassoc LESS MORE LESS_EQUAL MORE_EQUAL BOOL_EQUAL NOT_EQUAL
%right NOT

%start<bloc_instruction> programme

%%

programme: b=bloc_instruction EOF { b }

bloc_instruction:
  |    { [] }
  | i=instruction SEMICOLON b=bloc_instruction { i::b }

instruction: 
  | DRAW_ON { Draw_on }
  | DRAW_OFF { Draw_off }
  | MOVE c=expression { Move (c, $startpos) }
  | TURN c=expression { Turn (c, $startpos) }
  | COLOR_CHANGE c=expression { CouleurPinceau (c, $startpos) }
  | WIDTH_CHANGE e=expression { LargeurPinceau (e,$startpos) }
  | VAR str=ID { VarDecla (str, $startpos) } 
  | VAR str=ID EGALE t=expression  { VarDeclaInit (str, t, $startpos) }
  | str=ID EGALE t=expression  { VarInit (str, t, $startpos) }
  | IF c=expression THEN START i1=bloc_instruction END ELSE START i2=bloc_instruction END  { IfThenElse (c, i1, i2, $startpos) } 
  | IF c=expression THEN START i=bloc_instruction END { IfThen (c,i, $startpos) }
  | WHILE c=expression DO START i=bloc_instruction END  { While (c,i, $startpos) }
  | REPEAT e=expression MANY_TIMES START i=bloc_instruction END { Repeat (e,i, $startpos) }
  | RETURN e=expression { Return (e,$startpos) }
  | DEF n=ID LPAREN a=separated_list(COMA, ID) RPAREN START i=bloc_instruction END { FunDecla (n, a, i) }
  | n=ID LPAREN a=separated_list(COMA, expression) RPAREN  {ProcCall (n,a, $startpos)}
  | PRINT e=expression { Print e }

expression: 
  | signe=option(MINUS) n=NUM { Valeur (signe, n) } 
  | LPAREN e=expression RPAREN { e }
  | l=expression op=operateur r=expression { Op (l, op, r, $startpos) }
  | str=ID  { Var (str,$startpos) }
  | GENN LPAREN a=separated_list(COMA, expression) RPAREN {GenN (a,$startpos)}
  | n=ID LPAREN a=separated_list(COMA, expression) RPAREN  {FunCall (n,a, $startpos)}
  | TRUE {True}
  | FALSE {False}
  | e1=expression op=op_num e2=expression {TestBool (e1, op, e2, $startpos)}
  | NOT c=expression  { Not (c,$startpos)}
  | c1=expression AND c2=expression  {And (c1, c2,$startpos)} 
  | c1=expression OR c2=expression {Or (c1, c2,$startpos)}
  | RED { Red }
  | BLUE { Blue }
  | GREEN { Green }
  | BLACK { Black }
  | YELLOW  { Yellow }
  | v=HEX  { Hexcode v }
  | GENC LPAREN a=option(expression) RPAREN  { GenC (a,$startpos) }
  | str=TXT { Text str } 

%inline operateur:
  | PLUS { Plus }
  | MINUS { Minus }
  | TIME { Time }
  | DIVIDE { Divided }
  | MODULO {Mod}

%inline op_num:
  | LESS {Less}
  | MORE {More}
  | LESS_EQUAL {Less_equal}
  | MORE_EQUAL {More_equal}
  | BOOL_EQUAL {Bool_equal}
  | NOT_EQUAL  {Not_equal}
