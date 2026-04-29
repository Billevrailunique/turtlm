open Interp
open Ast
open Eval

let pretty_printing_instr = function
  | Draw_on -> "Draw_on"
  | Draw_off -> "Draw_off"
  | Move (e,_) -> "Move " ^ exp_as_string e
  | Turn (e,_) -> "Turn " ^ exp_as_string e
  | CouleurPinceau _ -> "CouleurPinceau"
  | LargeurPinceau _ -> "LargeurPinceau"
  | VarDecla (x,_) -> "VarDecla : " ^ x
  | VarDeclaInit (x,e,_) -> "VarDeclaInit : " ^ x ^ exp_as_string e
  | VarInit (x,e,_) -> "VarInit : " ^ x ^ exp_as_string e
  | Repeat (n,_,_) -> "Repeter " ^ exp_as_string n ^ " Fois"
  | While (c,_,_) -> "Tant tque " ^ exp_as_string c 
  | IfThenElse (c,_,_,_) -> "Si/Sinon " ^ exp_as_string c 
  | IfThen (c,_,_) -> "Si " ^ exp_as_string c
  | FunDecla (name,l,_) -> "FunDecla : " ^ name ^ "(" ^ List.fold_left (fun str x -> str ^ "," ^ x) "" l  ^ ")" 
  | Print e -> "Print " ^ exp_as_string e
  | Return (e,_) -> "Return " ^ exp_as_string e
  | ProcCall (name,l,_) -> "ProcCall : " ^ name ^ "(" ^ List.fold_left (fun str x -> str ^ "," ^ exp_as_string x) "" l  ^ ")"