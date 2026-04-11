open Ast 
open Env 
open Eval
open Graphics

let draw = ref false

let val_angle = ref 90. 
let angle () = !val_angle *. Float.pi /. 180. 


let init_graphics () = open_graph " 800x800";
        set_window_title "projet GAS6";
        set_line_width 1;
        set_color black;
        moveto 400 400

let rec decode bloc = env := ref [] :: !env ;
                debloc bloc;
                match !env with 
                    | _ :: l -> env := l
                    | [] -> 
                            begin
                                Printf.eprintf "Erreur, dépiler env vide \n" ;
                                exit 1
                            end

and debloc bloc =  match bloc with 
                    | i :: suite -> next_action i; debloc suite
                    | [] -> ()

and next_action = function
    | Draw_on -> draw := true
    | Draw_off -> draw := false
    | Move (e,pos) -> let distance = eval_exp e in let a = angle () in 
                let dx = (int_of_float (distance *. cos a)) in
                let dy = (int_of_float (distance *. sin a)) 
                 in if current_x () + dx <= size_x () && current_y () + dy <= size_y () then
                     (if !draw then rlineto dx dy
                        else rmoveto dx dy)
                    else 
                begin
                    Printf.eprintf "Erreur curseur en dehors de l'écran à la ligne %d\n" pos.Lexing.pos_lnum;
                    exit 1
                end
    | Turn e -> val_angle := !val_angle +. eval_exp e
    | CouleurPinceau c -> let couleur = eval_couleur c in set_color couleur
    | LargeurPinceau (e, pos) -> let value = int_of_float (eval_exp e) in if value < 878 then set_line_width value else
                begin 
                    Printf.eprintf "Erreur largeur pinceau trop grande à la ligne %d\n" pos.Lexing.pos_lnum;
                    exit 1
                    end
    | VarDecla (name, pos) -> if not (already_declared name (flatten_spe !env)) then let a = ref (name, None) in 
                                (match !env with 
                                | vars :: _ -> vars := a :: !vars  
                                | [] -> 
                                    begin 
                                        Printf.eprintf "Erreur, env n'est pas sencé être vide";
                                        exit 1
                                    end)
                 
                else begin
                    Printf.eprintf "Erreur var %s already declared quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum;
                    exit 1 
                end
    | VarDeclaInit (name, value, pos) -> if not (already_declared name (flatten_spe !env)) then let a = ref (name, Some (eval_exp value)) in 
                                            (match !env with 
                                            | vars :: _ -> vars := a :: !vars
                                            | [] -> 
                                                begin 
                                                    Printf.eprintf "Erreur, env n'est pas sencé être vide";
                                                    exit 1
                                                end)
                else begin
                    Printf.eprintf "Erreur var %s already declared quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum;
                    exit 1 
                end
    | VarInit (name, value, pos) -> let v = (eval_exp value) in if (change_val name v (flatten_spe !env)) then () else 
                begin
                    Printf.eprintf "Erreur var %s not declared yet quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum;
                    exit 1 
                end
    | Repeat (x,i) -> let n = int_of_string x in for _ = 1 to n do 
                        decode i
                    done
    | While (c,i) -> while (eval_bool c) do 
                       decode i 
                    done 
    | IfThen (c,i) -> if eval_bool c then decode i 
    | IfThenElse (c,i1,i2) -> if eval_bool c then decode i1 else decode i2
