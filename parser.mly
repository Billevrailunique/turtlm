%{
open Ast
%}

%token EOF DRAW_OFF DRAW_ON DIVIDE TIME MINUS PLUS MOVE TURN RPAREN LPAREN SEMICOLON
%token<string> ID NUM

%left PLUS MINUS
%left TIME DIVIDE

%start<bloc_instruction> programme

%%

programme: b=bloc_instruction EOF { b }


(*cf doc menhir option -> epsilon *)
bloc_instruction: l=separated_list(SEMICOLON, instruction) { l }

instruction: 
  | DRAW_ON { Draw_on }
  | DRAW_OFF { Draw_off }
  | MOVE c=expression { Move c }
  | TURN c=expression { Turn c }

expression: 
  | n=NUM { Valeur n }
  | LPAREN e=expression RPAREN { e }
  | l=expression op=operateur r=expression { Op (l, op, r) }

%inline operateur:
  | PLUS { Plus }
  | MINUS { Minus }
  | TIME { Time }
  | DIVIDE { Divided }