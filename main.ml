open Ast
open Graphics
exception Division_by_zero

let lexbuf = Lexing.from_channel stdin 

let ast =  try Parser.programme Lexer.token lexbuf
            with 
            | Lexer.Error a -> Printf.eprintf "Erreur lexicale %s\n" a; exit 1
            | Parser.Error -> let pos = lexbuf.Lexing.lex_curr_p in Printf.eprintf "Erreur syntaxique à la ligne : %d et à la colonne %d\n" pos.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol ); exit 1

let draw = ref false

let val_angle = ref 90. 
let angle () = !val_angle *. Float.pi /. 180. 

let (vars:declared ref) = ref []

let init_graphics () = open_graph " 800x800";
        set_window_title "projet GAS6";
        set_line_width 1;
        set_color black;
        moveto 400 400

let rec next_action = function
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
    | VarDecla (name, pos) -> if not (already_declared name !vars) then let a = ref (name, None) in vars := a :: !vars else 
                begin
                    Printf.eprintf "Erreur var %s already declared quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum;
                    exit 1 
                end
    | VarDeclaInit (name, value, pos) -> if not (already_declared name !vars) then let a = ref (name, Some value) in vars := a :: !vars else
                 begin
                    Printf.eprintf "Erreur var %s already declared quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum;
                    exit 1 
                end
    | VarInit (name, value, pos) -> if (change_val name value !vars) then () else 
                begin
                    Printf.eprintf "Erreur var %s not declared yet quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum;
                    exit 1 
                end

and already_declared name = function 
    | [] -> false     
    | a :: otre -> match !a with (str,_) -> (if String.equal name str then true else already_declared name otre)
    

and change_val name value = function 
    | [] -> false
    | r :: otre -> match !r with 
                    | (a,_) -> if String.equal name a
                                 then (r := (a, Some value) ; true)
                                 else change_val name value otre
    

and eval_exp = function 
    | Valeur a -> float_of_string a
    | Op (l, op, r, pos) -> eval_op pos l r op 
    | Var (name, pos) -> match get_val name !vars with 
                            | Some e -> eval_exp e
                            | None -> 
                begin 
                    Printf.eprintf "Erreur var %s not initialisé yet quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum ;
                    exit 1
                end

and eval_op pos l r = function 
    | Plus -> (eval_exp l) +. (eval_exp r)
    | Minus -> (eval_exp l) -. (eval_exp r)
    | Time -> (eval_exp l) *. (eval_exp r)
    | Divided -> let q = (eval_exp r) in if q <> 0. then (eval_exp l) /. q else 
            begin
                Printf.eprintf "Erreur division par 0 à la ligne %d\n" pos.Lexing.pos_lnum ;
                exit 1
            end

and eval_couleur = function
    | Red  -> red
    | Blue  -> blue
    | Green  -> green
    | Yellow -> yellow 
    | Black  -> black 
    | Hexcode v  ->  let v1 = int_of_string ("0X" ^ String.sub v 0 2 ) and v2 = int_of_string ("0X" ^ String.sub v 2 2 ) and v3 = int_of_string ("0X" ^ String.sub v 4 2 ) in rgb v1 v2 v3

and get_val name = function 
    | [] -> None
    | r :: l  -> match !r with (a,b) -> if String.equal a name then b else get_val name l
    

let rec decode bloc =  match bloc with 
                    | i :: suite -> next_action i; decode suite
                    | [] -> ()
                


let () = init_graphics () ;
         decode ast;

        ignore(read_key());
        close_graph () ;