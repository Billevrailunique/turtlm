(* parser *)
 
type bloc_instruction = instruction list
and instruction = Draw_on
                | Draw_off
                | Move of expression
                | Turn of expression
and expression = Valeur of string
                | Op of expression * operateur * expression
and operateur = Plus 
                | Minus
                | Time 
                | Divided 