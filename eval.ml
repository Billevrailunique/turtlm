open Ast 
open Env
open Graphics

let rec eval_bool = function    
    | True -> true 
    | False -> false 
    | And (c1,c2) -> (eval_bool c1) && (eval_bool c2)
    | Or (c1,c2) -> (eval_bool c1) || (eval_bool c2)
    | Not c -> not (eval_bool c)
    | TestBool (e1, op, e2) ->  let a = (eval_exp e1) and  b = (eval_exp e2) in (match op with 
                                | Less -> a < b 
                                | More -> a > b
                                | Less_equal -> a <= b 
                                | More_equal -> a >= b 
                                | Bool_equal -> a = b 
                                | Not_equal -> a <> b )

and eval_exp = function 
    | Valeur a -> float_of_string a
    | Op (l, op, r, pos) -> eval_op pos l r op 
    | Var (name, pos) -> match get_val name (flatten_spe !env) with 
                            | Some e -> e
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

