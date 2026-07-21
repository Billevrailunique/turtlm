(*valeur sémantique renvoyé par le parse*)
type bloc_instruction = instruction list
and instruction = Draw of string
                | Simple of string * expression * Lexing.position
                | VarDecla of string * Lexing.position
                | VarDeclaInit of string * expression * Lexing.position
                | VarInit of string * expression * Lexing.position
                | Repeat of expression * bloc_instruction * Lexing.position
                | While of expression * bloc_instruction * Lexing.position
                | IfThenElse of expression * bloc_instruction * bloc_instruction * Lexing.position
                | IfThen of expression * bloc_instruction * Lexing.position
                | FunDecla of string * string list * bloc_instruction * Lexing.position
                | ProcCall of string * expression list * Lexing.position
                | Set of string * expression * expression * Lexing.position
and expression = NumOrVarOrHexa of string * Lexing.position
                | Op of expression * op_bin * expression * Lexing.position
                | FunCall of string * expression list * Lexing.position
                | GenN of expression list * Lexing.position
                | ValBool of string
                | Not of expression * Lexing.position
                | Color of string
                | GenC of expression option * Lexing.position
                | Text of string
                | Liste of expression list 
                | Get of string * expression * Lexing.position
and op_bin = Add of string
            | Mult of string
            | Ordre of string

(*permet aux variables de pouvoir traiter plusieurs type*)
type value =
    | VFloat of float
    | VBool of bool
    | VCool of Graphics.color
    | VText of string
    | Unsure of string     (*cas où ça peut être un nombre ou une couleur en héxa*)
    | Vliste of value list
    | No

(*une var, c'est un nom (unique) et peut-être un valeur si ça a été initialisé*)
type var = string * (value option)

(*un scope est l'ensemble des variables accessibles localement*)
type scope = var list

(*une func, c'est un nom (unique), un scope (ses paramètres), et le bloc d'instruction à exécuter si on l'appele*)
type fonction = (string * scope * bloc_instruction)

(*scope des fonctions*)
type scopeFun = fonction list     

(*représente l'état d'un programme à un moment donnée *)
type state = {
    draw : bool;   (*true : pinceau baissé; false :pinceau levé*)
    val_angle : float;   (*angle en degré du pinceau (0 à droite)*)
    env : scope list;     (*l'ensemble des variables accessibles*)
    env_fun : scopeFun list;   (*l'ensemble des fonctions accessibles*)
    deep : int; (* 0 -> global ; > 0 -> dans une fonction *)
    seed_init : bool;
}

(*comme state mais uniquement pendant le prétraitement*)
type check_state = {
    state : state;
    black_list : string  list   (*liste des fonctions déjà rencontrés (donc déjà checkés) pour éviter la récursion infini*)
}

(*brise l'enchainement naturelle des instructions si on retourn une valeur*)
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
exception OutOfBoundsList of string * Lexing.position
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
exception ListWaited of Lexing.position