open Ast
open Graphics
exception Division_by_zero

let lexbuf = Lexing.from_channel stdin 

let ast =  try Parser.programme Lexer.token lexbuf
            with 
            | Lexer.Error a -> Printf.eprintf "Erreur lexicale %s\n" a; exit 1
            | Parser.Error -> let pos = lexbuf.Lexing.lex_curr_p in Printf.eprintf "Erreur syntaxique à la ligne : %d. et à la colonne %d\n " pos.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol ); exit 1

let draw = ref false

let val_angle = ref 90. 
let angle () = !val_angle *. Float.pi /. 180. 

let init_graphics () = open_graph " 800x800";
        set_window_title "projet GAS6";
        set_line_width 5;
        set_color black;
        moveto 400 400

let rec next_action = function
    | Draw_on -> draw := true
    | Draw_off -> draw := false
    | Move e -> let distance = eval_exp e in let a = angle () in 
                let dx = (int_of_float (distance *. cos a)) in
                let dy = (int_of_float (distance *. sin a)) 
                 in if !draw then rlineto dx dy
                    else rmoveto dx dy
    | Turn e -> val_angle := !val_angle +. eval_exp e

and eval_exp = function 
    | Valeur a -> float_of_string a
    | Op (l, op, r, pos) -> match op with 
                        | Plus -> (eval_exp l) +. (eval_exp r)
                        | Minus -> (eval_exp l) -. (eval_exp r)
                        | Time -> (eval_exp l) *. (eval_exp r)
                        | Divided -> let q = (eval_exp r) in if q <> 0. then (eval_exp l) /. q else 
                                begin
                                    Printf.eprintf "Erreur division par 0 à la ligne %d\n" pos.Lexing.pos_lnum ;
                                    exit 1
                                end

let rec decode bloc =  match bloc with 
                    | i :: suite -> next_action i; decode suite
                    | [] -> ()
                


let () = init_graphics () ;
         decode ast;

        ignore(read_key());
        close_graph () ;