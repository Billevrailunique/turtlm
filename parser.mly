%{
open Ast
%}

%token EOF DRAW_OFF DRAW_ON GENC DIVIDE TIME GENN PRINT MINUS MODULO PLUS MOVE COMA TURN DEF RETURN AND OR RPAREN LPAREN SEMICOLON COLOR_CHANGE WIDTH_CHANGE VAR EGALE IF THEN ELSE WHILE DO REPEAT MANY_TIMES NOT_EQUAL LESS_EQUAL MORE_EQUAL BOOL_EQUAL LESS MORE START END NOT
%token<string> HEX NUM ID TXT COLOR VALBOOL

%left PLUS MINUS OR
%left TIME DIVIDE AND MODULO
%nonassoc LESS MORE LESS_EQUAL MORE_EQUAL BOOL_EQUAL NOT_EQUAL
%right NOT

%start<bloc_instruction> programme

%on_error_reduce expression

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
  | DEF n=ID LPAREN a=separated_list(COMA, ID) RPAREN START i=bloc_instruction END { FunDecla (n, a, i, $startpos) }
  | n=ID LPAREN a=separated_list(COMA, expression) RPAREN  {ProcCall (n,a, $startpos)}
  | PRINT e=expression { Print e }

expression: 
  | signe=option(MINUS) n=NUM { Valeur (signe, n) } 
  | LPAREN e=expression RPAREN { e }
  | l=expression op=op_bin r=expression { Op (l, op, r, $startpos) }
  | str=ID  { Var (str,$startpos) }
  | GENN LPAREN a=separated_list(COMA, expression) RPAREN {GenN (a,$startpos)}
  | n=ID LPAREN a=separated_list(COMA, expression) RPAREN  {FunCall (n,a, $startpos)}
  | b=VALBOOL { ValBool b }
  | NOT c=expression  { Not (c,$startpos)}
  | c=COLOR {Color c}
  | v=HEX  { Hexcode v }
  | GENC LPAREN a=option(expression) RPAREN  { GenC (a,$startpos) }
  | str=TXT { Text str } 

%inline op_bin:
  | PLUS { Plus }
  | MINUS { Minus }
  | TIME { Time }
  | DIVIDE { Divided }
  | MODULO {Mod}
  | LESS {Less}
  | MORE {More}
  | LESS_EQUAL {Less_equal}
  | MORE_EQUAL {More_equal}
  | BOOL_EQUAL {Bool_equal}
  | NOT_EQUAL  {Not_equal}
  | AND {And} 
  | OR {Or}
