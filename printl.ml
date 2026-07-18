open Ast
open Printf

let afficherScope declared = let rec aux = function 
                | (name,_) :: l -> Printf.printf "%s\n" name ; aux l
                | [] -> () 
        in aux declared
        
let afficherfun func = match func with (name, _, _) -> Printf.printf "%s\n" name

let rec afficherScopeFun scopefun = match scopefun with
        | a :: l -> afficherfun a; afficherScopeFun l
        | [] -> ()
let afficherEnv state =  
        Printf.printf "affichage var début : \n";
        (let rec aux = function
                | a :: m -> afficherScope a ; aux m
                | [] -> ()
        in aux state.env);
        Printf.printf "fin\n";
        Printf.printf "affichage fonction début : \n";
        let rec aux = (function
        | a :: m -> afficherScopeFun a; aux m
        | [] -> ()) 
        in aux state.env_fun;
        Printf.printf "fin\n"

let afficherInstr = function 
  | Draw_on -> print_endline "draw on"
  | Draw_off -> print_endline "draw off"
  | Move _-> print_endline "move"
  | Turn _-> print_endline "turn"
  | CouleurPinceau _-> print_endline "couleur pinceau"
  | LargeurPinceau _-> print_endline "largeur pinceau"
  | VarDecla _-> print_endline "var decla"
  | VarDeclaInit _ -> print_endline "var decla init"
  | VarInit _ -> print_endline "var init "
  | Repeat _ ->print_endline "repeat "
  | While _ -> print_endline "while "
  | IfThenElse _ -> print_endline "if then else"
  | IfThen _-> print_endline "if then "
  | FunDecla _ -> print_endline "fun decla"
  | Print _-> print_endline "print "
  | Return _-> print_endline "return"
  | ProcCall _ -> print_endline "proc call"
  | Set _ -> print_endline "liste set"

let afficherBlocInstr = List.iter afficherInstr 