type bloc_instruction = instruction list
and instruction = Draw_on
                | Draw_off
                | Move of expression
                | Turn of expression
                | CouleurPinceau of color
                | LargeurPinceau of expression
and expression = Valeur of string
                | Op of expression * operateur * expression * Lexing.position
and operateur = Plus 
                | Minus
                | Time 
                | Divided 
and color = Hexcode of string
            | Black
            | Blue
            | Red 
            | Yellow
            | Green


