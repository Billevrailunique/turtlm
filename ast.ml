type bloc_instruction = instruction list
and instruction = Draw_on
                | Draw_off
                | Move of expression * Lexing.position
                | Turn of expression * Lexing.position
                | CouleurPinceau of expression *Lexing.position
                | LargeurPinceau of expression * Lexing.position
                | VarDecla of string * Lexing.position
                | VarDeclaInit of string * expression * Lexing.position
                | VarInit of string * expression * Lexing.position
                | Repeat of expression * bloc_instruction * Lexing.position
                | While of expression * bloc_instruction * Lexing.position
                | IfThenElse of expression * bloc_instruction * bloc_instruction * Lexing.position
                | IfThen of expression * bloc_instruction * Lexing.position
                | FunDecla of string * string list * bloc_instruction * Lexing.position
                | Print of expression
                | Return of expression * Lexing.position
                | ProcCall of string * expression list * Lexing.position
and expression = NumOrVarOrHexa of unit option * string * Lexing.position
                | Op of expression * op_bin * expression * Lexing.position
                | FunCall of string * expression list * Lexing.position
                | GenN of expression list * Lexing.position
                | ValBool of string
                | Not of expression * Lexing.position
                | Color of string
                | GenC of expression option * Lexing.position
                | Text of string
and op_bin = Plus 
                | Minus
                | Time 
                | Divided 
                | Mod
                | And
                | Or
                |Less 
                | More 
                | Less_equal
                | More_equal   
                | Not_equal
                | Bool_equal

type value =
    | VFloat of float
    | VBool of bool
    | VCool of Graphics.color
    | VText of string
    | Unsure of float * string 
    | No

type var = string * (value option)
type scope = var list
type fonction = (string * scope * bloc_instruction)
type scopeFun = fonction list     

type state = {
    draw : bool;
    val_angle : float;
    env : scope list;
    env_fun : scopeFun list;
    deep : int; (* 0 -> global ; > 0 -> dans une fonction *)
    seed_init : bool;
}

type check_state = {
    state : state;
    black_list : string  list
}

type flow =
    | Continue of state
    | Returned of value * state

exception ReturnValue of value
exception TooManyArgsException of string * Lexing.position
exception ArgsMissingException of string * Lexing.position
exception Division_by_zero of Lexing.position
exception EnvEmpty
exception OutOfBoundsCursor of Lexing.position 
exception OutOfBoundsPencilWidth of Lexing.position 
exception AlreadyDeclaredVar of string * Lexing.position 
exception AlreadyDeclaredFun of string * Lexing.position 
exception UnknownFun of string * Lexing.position
exception UnknownVar of string * Lexing.position 
exception OutOfContextReturn of Lexing.position
exception NotYetInitVar of string * Lexing.position
exception Invalid_argumentGenN of Lexing.position
exception NegativeRepeat of Lexing.position
exception FloatWaited of Lexing.position
exception BoolWaited of Lexing.position
exception ColorWaited of Lexing.position