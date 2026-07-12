open Ast 
open Env
open Graphics
open Random 

let rec eval_exp (state:Ast.state) ~decode = function 
    | NumOrVarOrHexa (s,a,pos) -> let mult = (match s with 
                        | Some _ -> -1.
                        | None -> 1.) in 
         if is_id a 
        then 
            let name = a in
            let (flag,res) =  (get_val name state.env) in  (match res with 
            | Some e -> if mult == -1. then (match e with VFloat val_ -> (VFloat (-1. *. val_), state) | _ -> raise (FloatWaited(pos)))
                            else (e,state)
            | None -> if flag then raise (NotYetInitVar (name, pos)) else raise (UnknownVar (name,pos)))
        else (Unsure (mult, a),state)
       
    | Op (l, op, r, pos) -> eval_op pos l r state ~decode op
    | FunCall (n, argsValue ,pos) ->  let rec aux_env env = (match env with 
                                        | [] -> raise (UnknownFun (n,pos))
                                        | truc :: otre -> let rec aux_scope scope = (match scope with
                                                    | (str, param, instr) :: _ when String.equal str n 
                                                    -> 
                                                        let x,up_state = eval_list state ~decode argsValue in
                                                        let up_param = setFonction n pos param x in
                                                        let up_state = {
                                                            draw = up_state. draw;
                                                            val_angle = up_state.val_angle;
                                                            env = up_param :: up_state.env;
                                                            env_fun = up_state.env_fun;
                                                            deep = up_state.deep +1;
                                                            seed_init = up_state.seed_init;
                                                        } in begin 
                                                        match decode instr up_state with 
                                                                | Returned (v,_)-> (v,state)
                                                                | Continue _ -> (No,state) 
                                                        end 
                                                    | _ :: l -> aux_scope l
                                                    | [] -> aux_env otre)
                                                    in aux_scope truc)
                                    in aux_env state.env_fun
    | GenN (args, pos) -> (try match args with
                            | a :: b :: c :: [] -> let valA,state = eval_exp state ~decode a in Random.init (int_of_float (as_float valA pos)) ;
                                                    let valB,state = eval_exp state ~decode b in 
                                                    let valC,state = eval_exp state ~decode c in
                                                    (VFloat (float_of_int (Random.int_in_range ~min:(int_of_float (as_float valB pos)) ~max:(int_of_float (as_float valC pos)))), {state with seed_init = true}) 
                            | a :: b :: [] -> let valA,state = eval_exp state ~decode a in 
                                                let valB,state = eval_exp state ~decode b in
                                                (if not state.seed_init then self_init ()) ; (VFloat (float_of_int (Random.int_in_range ~min:(int_of_float(as_float valA pos)) ~max:(int_of_float(as_float valB pos)))),{state with seed_init =true})
                            | a :: [] -> let valA,state = eval_exp state ~decode a in 
                                        (if not state.seed_init then self_init ()) ; (VFloat (float_of_int (Random.int_in_range ~min:1 ~max:(int_of_float(as_float valA pos)))),{state with seed_init = true})
                            | _ -> invalid_arg ""
                        with 
                            | Invalid_argument _ -> raise (Invalid_argumentGenN pos))
    | Color str -> let (c:Graphics.color) = (match str with 
                            | "vert" -> green
                            | "bleu" -> blue
                            | "jaune" -> yellow
                            | "rouge" -> red
                            | _ -> black)
                    in (VCool c,state) 
    | GenC (args,pos) -> (match args with 
                            | None -> (if not state.seed_init then Random.self_init () ;
                                    let r = (Random.int_in_range ~min:0 ~max:255) and g = (Random.int_in_range ~min:0 ~max:255) and b = (Random.int_in_range ~min:0 ~max:255) in VCool(rgb r g b)),state
                            | Some a -> let valA,state = eval_exp state ~decode a in (Random.init(int_of_float (as_float valA pos)); 
                                    let r = (Random.int_in_range ~min:0 ~max:255) and g = (Random.int_in_range ~min:0 ~max:255) and b = (Random.int_in_range ~min:0 ~max:255)  in VCool(rgb r g b)),state) 
    | ValBool b -> let valb = (match b with
                    | "Vrais" -> VBool true
                    | _ -> VBool false ) in (valb,state)
    | Not (c, pos) -> let valb,state = eval_exp state ~decode c in  VBool(not (as_bool valb pos)), state
    | Text str -> VText str,state
    | Get (str, e, pos) -> let v,s = eval_exp state ~decode e in  let (flag,res) =  (get_val str s.env) in (match res with 
            | Some l ->  begin
                match l with 
                | Vliste l -> begin 
                    match List.nth_opt l (int_of_float (as_float v pos)) with 
                        | Some a -> a,s
                        | None -> raise (OutOfBoundsList (str,pos)) 
                             end
                | _ -> raise (ListWaited pos) 
            end 
            | None -> if flag then raise (NotYetInitVar (str, pos)) else raise (UnknownVar (str,pos)))
    | Liste l -> let l,s = eval_list state ~decode l in (Vliste l,s)
and eval_op pos l r (state:state) ~decode = 
    let le,state = eval_exp state ~decode l in let re,state = eval_exp state ~decode r in function 
    | Plus -> VFloat (as_float le pos +. as_float re pos),state
    | Minus -> VFloat  (as_float le pos -. as_float re pos) ,state
    | Time ->  VFloat (as_float le pos *. as_float re pos),state
    | Divided -> let q = as_float re pos in VFloat (if q <> 0. then as_float le pos /. q else raise (Division_by_zero pos)),state
    | Mod -> let le = as_float le pos and re = as_float re pos in VFloat (if re <> 0. then ( mod_float le re ) else raise (Division_by_zero pos)),state
    | And -> VBool (as_bool le pos && as_bool re pos),state
    | Or -> VBool (as_bool le pos || as_bool re pos),state
    | Less -> VBool (as_float le pos < as_float re pos),state
    | Less_equal -> VBool (as_float le pos <= as_float re pos ),state
    | Bool_equal ->  VBool (as_float le pos = as_float re pos),state
    | Not_equal -> VBool (as_float le pos <> as_float re pos),state
    | More -> VBool (as_float le pos > as_float re pos),state
    | More_equal -> VBool (as_float le pos >= as_float re pos),state
            
and as_float (v:value) pos = match v with 
            | (VFloat a) -> a
            | (Unsure (fact,a))-> (fact *. Float.of_string a)
            | _ -> raise (FloatWaited pos)
and as_bool (v:value) pos = match v with 
            | (VBool a)-> a
            | _ -> raise (BoolWaited pos)
and as_color (v:value) pos = match v with 
            | (VCool a) -> a
            | (Unsure (fact,v)) ->if fact == -1. then raise (ColorWaited pos); if is_hexa v then let v1 = int_of_string ("0X" ^ String.sub v 0 2 ) and v2 = int_of_string ("0X" ^ String.sub v 2 2 ) and v3 = int_of_string ("0X" ^ String.sub v 4 2 ) in rgb v1 v2 v3  else raise (ColorWaited pos)
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
            | Vliste l -> String.concat "," (List.map as_string l) 
            |Unsure (fact,a) -> (if fact == 1. then "" else "-") ^ a
            

and eval_list (state:state) ~decode l :value list * state =let rec aux acc state = function 
    | [] -> List.rev acc,state
    | a :: l -> let valuated,state = (eval_exp state ~decode a) in aux (valuated::acc) state l 
    in let result,state = aux [] state l in result,state

and is_id str =
  let n = String.length str in
  if n = 0 then false
  else
    let is_lowercase_alpha c = c >= 'a' && c <= 'z' in
    let is_valid_char c =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c = '_'
    in
    is_lowercase_alpha str.[0] &&
    String.for_all is_valid_char str

and is_hexa str =
  String.length str = 6 &&
  String.for_all (fun c ->
    (c >= '0' && c <= '9') ||
    (c >= 'A' && c <= 'F')
  ) str

let rec check check_instr (state:check_state) e = match e with 
    | NumOrVarOrHexa (_,a,pos) -> 
         if is_id a 
        then 
            let name = a in
            let (flag,res) =  (get_val name state.state.env) in  (match res with 
            | Some _ -> state
            | None -> if flag then raise (NotYetInitVar (name, pos)) else raise (UnknownVar (name,pos)))
        else state 
    | Op (l, _, r, _) -> let s = check check_instr state l  in check check_instr s r 
    | FunCall (n, argsValue ,pos) -> if  List.exists (String.equal n) state.black_list then state else 
        if not (already_declared_fun n state.state.env_fun) then raise (UnknownFun (n,pos)) 
        else let rec aux_env env = (match env with 
                                        | [] -> assert false 
                                        | truc :: otre -> let rec aux_scope scope = (match scope with
                                                    | (str, param, instr) :: _ when String.equal str n 
                                                    -> 
                                                        let valuated = List.map (fun _ -> No) argsValue in 
                                                                let env = setFonction str pos param valuated in 
                                                                let lil_state = {draw = state.state.draw; val_angle = state.state.val_angle; env = env :: state.state.env; env_fun = state.state.env_fun; seed_init = state.state.seed_init; deep = state.state.deep+1} in
                                                                let in_state = {state = lil_state; black_list = n :: state.black_list} in
                                                                let in_state = List.fold_left (check_instr) in_state instr in {state = state.state; black_list = in_state.black_list}
                                                    | _ :: l -> aux_scope l
                                                    | [] -> aux_env otre)
                                                    in aux_scope truc)
                                    in aux_env state.state.env_fun
    | GenN (args, pos) -> (try match args with
                            | a :: b :: c :: [] -> check check_instr (check check_instr(check check_instr state c) b) a   
                            | a :: b :: [] -> check check_instr (check check_instr state b) a   
                            | a :: [] -> check check_instr state a   
                            | _ -> invalid_arg ""
                        with 
                            | Invalid_argument _ -> raise (Invalid_argumentGenN pos))
    | GenC (args,_) -> begin 
                            match args with 
                                | None -> state  
                                | Some a -> check check_instr state a
                        end
    | Not (c, _) -> check check_instr state c
    |  _ -> state
