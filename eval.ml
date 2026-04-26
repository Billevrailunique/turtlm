open Ast 
open Env
open Graphics
open Random 

let decode_ref : (?initial_decla:variable list -> instruction list -> variable list ) ref = ref (fun ?initial_decla:_ _ -> [])

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
    | Valeur (s,a) -> (match s with 
                        | Some _ -> -1. *. float_of_string a
                        | None -> float_of_string a)
    | Op (l, op, r, pos) -> eval_op pos l r op 
    | Var (name, pos) -> (match get_val name !env with 
                            | Some e -> e
                            | None -> 
                begin 
                    Printf.eprintf "Erreur var %s not initialisé yet quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum ;
                    exit 1
                end)
    | FunCall (n, argsValue ,pos) ->  let rec aux liste = match liste with
                                        | a :: l -> (match !a with (str, param, instr) -> if String.equal str n 
                                                    then (try
                                                        let x = eval_list argsValue in
                                                        setFonction param x ;
                                                        ignore(!decode_ref ~initial_decla:param instr); 
                                                        0.
                                                        with 
                                                            | ReturnValue v -> unsetFonction () ; v  )
                                                    else aux l)  
                                        | [] -> begin
                                                Printf.eprintf "Erreur fonction inconnu à la ligne %d\n" pos.Lexing.pos_lnum;
                                                exit 1
                                                end
                                    in aux !envFun
    | GenN (args, pos) -> try match args with
                            | a :: b :: c :: [] -> Random.init (int_of_float (eval_exp a)) ; seed_init := true ; float_of_int (Random.int_in_range ~min:(int_of_float (eval_exp b)) ~max:(int_of_float (eval_exp c))) 
                            | a :: b :: [] -> (if not !seed_init then self_init (); seed_init := true) ; float_of_int (Random.int_in_range ~min:(int_of_float(eval_exp a)) ~max:(int_of_float(eval_exp b)))
                            | a :: [] -> (if not !seed_init then self_init () ; seed_init := true) ; float_of_int (Random.int_in_range ~min:1 ~max:(int_of_float(eval_exp a)))
                            | _ -> invalid_arg ""
                        with 
                            | Invalid_argument _ -> begin
                                                Printf.eprintf "Erreur, argument invalide,probablement min < max; ligne : %d\n" pos.Lexing.pos_lnum;
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
    | Mod -> let le = (eval_exp l) and re = (eval_exp r) in if re <> 0. then ( mod_float le re ) else  
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
    | GenC args -> match args with 
                            | None -> (if not !seed_init then Random.self_init () ; seed_init := true ; 
                                    let r = (Random.int_in_range ~min:0 ~max:255) and g = (Random.int_in_range ~min:0 ~max:255) and b = (Random.int_in_range ~min:0 ~max:255) in rgb r g b)
                            | Some a -> (Random.init(int_of_float (eval_exp a)); seed_init := true; 
                                    let r = (Random.int_in_range ~min:0 ~max:255) and g = (Random.int_in_range ~min:0 ~max:255) and b = (Random.int_in_range ~min:0 ~max:255)  in rgb r g b )
                            
                        

and eval_list = function 
    | [] -> []
    | a :: l -> (eval_exp a) :: eval_list l

