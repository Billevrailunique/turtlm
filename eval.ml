open Ast 
open Env
open Graphics
open Random 

let decode_ref : (?initial_decla:variable list -> instruction list -> variable list ) ref = ref (fun ?initial_decla:_ _ -> [])

let rec eval_exp = function 
    | Valeur (s,a) -> (match s with 
                        | Some _ -> VFloat (-1. *. float_of_string a)
                        | None -> VFloat(float_of_string a))
    | Op (l, op, r, pos) -> VFloat (eval_op pos l r op) 
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
                                                        No
                                                        with 
                                                            | ReturnValue (v:value) -> unsetFonction () ; v  )
                                                    else aux l)  
                                        | [] -> begin
                                                Printf.eprintf "Erreur fonction inconnu à la ligne %d\n" pos.Lexing.pos_lnum;
                                                exit 1
                                                end
                                    in aux !envFun
    | GenN (args, pos) -> (try match args with
                            | a :: b :: c :: [] -> Random.init (int_of_float (as_float(eval_exp a) pos)) ; seed_init := true ; VFloat (float_of_int (Random.int_in_range ~min:(int_of_float (as_float(eval_exp b)pos)) ~max:(int_of_float (as_float(eval_exp c)pos)))) 
                            | a :: b :: [] -> (if not !seed_init then self_init (); seed_init := true) ; VFloat (float_of_int (Random.int_in_range ~min:(int_of_float(as_float(eval_exp a)pos)) ~max:(int_of_float(as_float(eval_exp b)pos))))
                            | a :: [] -> (if not !seed_init then self_init () ; seed_init := true) ; VFloat (float_of_int (Random.int_in_range ~min:1 ~max:(int_of_float(as_float(eval_exp a)pos))))
                            | _ -> invalid_arg ""
                        with 
                            | Invalid_argument _ -> begin
                                                Printf.eprintf "Erreur, argument invalide,probablement min < max; ligne : %d\n" pos.Lexing.pos_lnum;
                                                exit 1 
                                                end) 
    | Red  -> VCool red
    | Blue  -> VCool blue
    | Green  -> VCool green
    | Yellow -> VCool yellow 
    | Black  -> VCool black 
    | Hexcode v  ->  let v1 = int_of_string ("0X" ^ String.sub v 0 2 ) and v2 = int_of_string ("0X" ^ String.sub v 2 2 ) and v3 = int_of_string ("0X" ^ String.sub v 4 2 ) in VCool (rgb v1 v2 v3)
    | GenC (args,pos) -> (match args with 
                            | None -> (if not !seed_init then Random.self_init () ; seed_init := true ; 
                                    let r = (Random.int_in_range ~min:0 ~max:255) and g = (Random.int_in_range ~min:0 ~max:255) and b = (Random.int_in_range ~min:0 ~max:255) in VCool(rgb r g b))
                            | Some a -> (Random.init(int_of_float (as_float(eval_exp a) pos)); seed_init := true; 
                                    let r = (Random.int_in_range ~min:0 ~max:255) and g = (Random.int_in_range ~min:0 ~max:255) and b = (Random.int_in_range ~min:0 ~max:255)  in VCool(rgb r g b) ))
    | True -> VBool true 
    | False -> VBool false 
    | And (c1,c2, pos) -> VBool(as_bool(eval_exp c1) pos && as_bool(eval_exp c2) pos)
    | Or (c1,c2, pos) -> VBool(as_bool(eval_exp c1) pos || as_bool(eval_exp c2) pos)
    | Not (c, pos) -> VBool(not (as_bool(eval_exp c) pos))
    | TestBool (e1, op, e2, pos) ->  let a = as_float(eval_exp e1) pos and  b = as_float(eval_exp e2) pos in let rep = (match op with 
                                | Less -> a < b 
                                | More -> a > b
                                | Less_equal -> a <= b 
                                | More_equal -> a >= b 
                                | Bool_equal -> a = b 
                                | Not_equal -> a <> b ) in VBool rep
    | Text str -> VText str
and eval_op pos l r = function 
    | Plus -> as_float(eval_exp l) pos +. as_float(eval_exp r) pos
    | Minus -> as_float(eval_exp l) pos -. as_float(eval_exp r) pos 
    | Time -> as_float(eval_exp l) pos *. as_float(eval_exp r) pos
    | Divided -> let q = as_float(eval_exp r) pos in if q <> 0. then as_float(eval_exp l) pos /. q else 
            begin
                Printf.eprintf "Erreur division par 0 à la ligne %d\n" pos.Lexing.pos_lnum ;
                exit 1
            end
    | Mod -> let le = as_float(eval_exp l) pos and re = as_float(eval_exp r) pos in if re <> 0. then ( mod_float le re ) else  
               begin
                Printf.eprintf "Erreur division par 0 à la ligne %d\n" pos.Lexing.pos_lnum ;
                exit 1
            end 
            
and as_float v pos = match v with 
            | VFloat a -> a
            | _ -> begin
                Printf.eprintf "Erreur, float attandue à la ligne %d\n" pos.Lexing.pos_lnum ; exit 1
            end 
and as_bool v pos = match v with 
            | VBool a -> a
            | _ -> begin
                Printf.eprintf "Erreur, bool attandue à la ligne %d\n" pos.Lexing.pos_lnum ; exit 1
            end 
and as_color v pos = match v with 
            | VCool a -> a
            | _ -> begin
                Printf.eprintf "Erreur, couleur attandue à la ligne %d\n" pos.Lexing.pos_lnum ; exit 1
            end 
and as_string = function 
            | VText s -> String.sub s 1 ((String.length s) -2)
            | VBool b -> Bool.to_string b
            | VFloat f -> Float.to_string f
            | VCool c -> let r = (c lsr 16) land 0xFF in
                        let g = (c lsr 8) land 0xFF in
                        let b = c land 0xFF in
                        "r:" ^ Int.to_string r ^ " g:" ^ Int.to_string g ^ " b:" ^ Int.to_string b 
            | No -> "NO"

and eval_list = function 
    | [] -> []
    | a :: l ->  (eval_exp a) :: eval_list l

