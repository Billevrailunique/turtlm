type bloc_instruction = instruction list
and instruction = Draw_on
                | Draw_off
                | Move of expression * Lexing.position
                | Turn of expression
                | CouleurPinceau of color
                | LargeurPinceau of expression * Lexing.position
                | VarDecla of string * Lexing.position
                | VarDeclaInit of string * expression * Lexing.position
                | VarInit of string * expression * Lexing.position
and expression = Valeur of string
                | Op of expression * operateur * expression * Lexing.position
                | Var of string * Lexing.position
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

type declared =  (string * expression option ) ref list


