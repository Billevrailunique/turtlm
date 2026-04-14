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
                | Repeat of string * bloc_instruction
                | While of condition * bloc_instruction
                | IfThenElse of condition * bloc_instruction * bloc_instruction
                | IfThen of condition * bloc_instruction
                | FunDecla of string * string list * bloc_instruction 
                | Return of expression * Lexing.position
                | ProcCall of string * expression list * Lexing.position
and expression = Valeur of string
                | Op of expression * operateur * expression * Lexing.position
                | Var of string * Lexing.position
                | FunCall of string * expression list * Lexing.position
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
and condition = True 
            | False 
            | TestBool of expression * op_num * expression
            | Not of condition
            | And of condition * condition
            | Or of condition * condition
and op_num = Less 
            | More 
            | Less_equal
            | More_equal   
            | Not_equal
            | Bool_equal 

type variable = (string * float option ) ref 
type declared =  variable list ref 
type environnement = declared list ref 

type fonction = (string * variable list * bloc_instruction) ref

type contex = Global 
            | Fonction


exception ReturnValue of float
exception TooManyArgsException
exception ArgsMissingException


