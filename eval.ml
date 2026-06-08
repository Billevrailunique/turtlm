open Ast 
open Env
open Graphics
open Random 

let decode_ref : (?initial_decla:variable list -> instruction list -> variable list ) ref = ref (fun ?initial_decla:_ _ -> [])

let rec eval_exp = function 
    | Valeur (s,a) -> (match s with 
                        | Some _ -> VFloat (-1. *. float_of_string a)
                        | None -> VFloat(float_of_string a))
    | Op (l, op, r, pos) ->  eval_op pos l r op 
    | Var (name, pos) -> let (flag,res) =  (get_val name !env) in  (match res with 
                            | Some e -> e
                            | None -> if flag then raise (NotYetInitVar (name, pos)) else raise (UnknownVar (name,pos)))
    | FunCall (n, argsValue ,pos) ->  let rec aux liste = match liste with
                                        | a :: l -> (match !a with (str, param, instr) -> if String.equal str n 
                                                    then (try
                                                        let x = eval_list argsValue in
                                                        setFonction n pos param x ;
                                                        ignore(!decode_ref ~initial_decla:param instr); 
                                                        No
                                                        with 
                                                            | ReturnValue (v:value) -> unsetFonction () ; v  )
                                                    else aux l)  
                                        | [] -> raise (UnknownFun (n,pos))
                                    in aux !envFun
    | GenN (args, pos) -> (try match args with
                            | a :: b :: c :: [] -> Random.init (int_of_float (as_float(eval_exp a) pos)) ; seed_init := true ; VFloat (float_of_int (Random.int_in_range ~min:(int_of_float (as_float(eval_exp b)pos)) ~max:(int_of_float (as_float(eval_exp c)pos)))) 
                            | a :: b :: [] -> (if not !seed_init then self_init (); seed_init := true) ; VFloat (float_of_int (Random.int_in_range ~min:(int_of_float(as_float(eval_exp a)pos)) ~max:(int_of_float(as_float(eval_exp b)pos))))
                            | a :: [] -> (if not !seed_init then self_init () ; seed_init := true) ; VFloat (float_of_int (Random.int_in_range ~min:1 ~max:(int_of_float(as_float(eval_exp a)pos))))
                            | _ -> invalid_arg ""
                        with 
                            | Invalid_argument _ -> raise (Invalid_argumentGenN pos))
    | Color str -> let (c:Graphics.color) = (match str with 
                            | "vert" -> green
                            | "bleu" -> blue
                            | "jaune" -> yellow
                            | "rouge" -> red
                            | _ -> black)
                    in VCool c 
    | Hexcode v  ->  let v1 = int_of_string ("0X" ^ String.sub v 0 2 ) and v2 = int_of_string ("0X" ^ String.sub v 2 2 ) and v3 = int_of_string ("0X" ^ String.sub v 4 2 ) in VCool (rgb v1 v2 v3)
    | GenC (args,pos) -> (match args with 
                            | None -> (if not !seed_init then Random.self_init () ; seed_init := true ; 
                                    let r = (Random.int_in_range ~min:0 ~max:255) and g = (Random.int_in_range ~min:0 ~max:255) and b = (Random.int_in_range ~min:0 ~max:255) in VCool(rgb r g b))
                            | Some a -> (Random.init(int_of_float (as_float(eval_exp a) pos)); seed_init := true; 
                                    let r = (Random.int_in_range ~min:0 ~max:255) and g = (Random.int_in_range ~min:0 ~max:255) and b = (Random.int_in_range ~min:0 ~max:255)  in VCool(rgb r g b) ))
    | ValBool b -> (match b with
                    | "Vrais" -> VBool true
                    | _ -> VBool false )
    | Not (c, pos) -> VBool(not (as_bool(eval_exp c) pos))
    | Text str -> VText str
and eval_op pos l r = 
    let le = eval_exp l in let re = eval_exp r in function 
    | Plus -> VFloat (as_float le pos +. as_float re pos)
    | Minus -> VFloat  (as_float le pos -. as_float re pos) 
    | Time ->  VFloat (as_float le pos *. as_float re pos)
    | Divided -> let q = as_float re pos in VFloat (if q <> 0. then as_float le pos /. q else raise (Division_by_zero pos))
    | Mod -> let le = as_float le pos and re = as_float re pos in VFloat (if re <> 0. then ( mod_float le re ) else raise (Division_by_zero pos))
    | And -> VBool (as_bool le pos && as_bool re pos)
    | Or -> VBool (as_bool le pos || as_bool re pos)
    | Less -> VBool (as_float le pos < as_float re pos)
    | Less_equal -> VBool (as_float le pos <= as_float re pos )
    | Bool_equal -> VBool (as_float le pos = as_float re pos)
    | Not_equal -> VBool (as_float le pos <> as_float re pos)
    | More -> VBool (as_float le pos > as_float re pos)
    | More_equal -> VBool (as_float le pos >= as_float re pos)
            
and as_float v pos = match v with 
            | VFloat a -> a
            | _ -> raise (FloatWaited pos)
and as_bool v pos = match v with 
            | VBool a -> a
            | _ -> raise (BoolWaited pos)
and as_color v pos = match v with 
            | VCool a -> a
            | _ -> raise (ColorWaited pos)
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

