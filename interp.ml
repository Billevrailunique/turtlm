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

let rec decode ?(initial_decla=[]) bloc  = env := ref initial_decla :: !env ;
                debloc bloc;
                match !env with 
                    | a :: _ -> !a
                    | [] -> raise DepileEnvEmpty
                
                
and depile_env () = match !env with 
                    | _ :: l -> env := l
                    | [] -> raise DepileEnvEmpty
                

and debloc bloc =  match bloc with 
                    | i :: suite ->  next_action i; debloc suite       
                    | [] -> ()

and next_action = function
    | Draw_on -> draw := true
    | Draw_off -> draw := false
    | Move (e,pos) -> let distance = as_float (eval_exp e) pos in let a = angle () in 
                let dx = (int_of_float (distance *. cos a)) in
                let dy = (int_of_float (distance *. sin a)) in 
                let nx = current_x () + dx in 
                let ny = current_y () + dy
                 in if nx <= size_x () && ny <= size_y () && nx >= 0 && ny >= 0 then
                     (if !draw then rlineto dx dy
                        else rmoveto dx dy)
                    else raise (OutOfBoundsCursor pos)
    | Turn (e,pos) -> val_angle := !val_angle +. as_float(eval_exp e) pos 
    | CouleurPinceau (c,pos) -> let couleur = as_color(eval_exp c) pos in set_color couleur
    | LargeurPinceau (e, pos) -> let value = int_of_float (as_float(eval_exp e)pos) in if value < 878 then set_line_width value else raise (OutOfBoundsPencilWidth pos)
    | VarDecla (name, pos) -> if not (already_declared name !env) 
                            then let a = ref (name, None) in 
                                (match !env with 
                                | vars :: _ -> vars := a :: !vars  
                                | [] -> raise DepileEnvEmpty)
                                else raise (AlreadyDeclaredVar (name,pos))
    | VarDeclaInit (name, value, pos) -> if not (already_declared name  !env) then let v = eval_exp value in let a = ref (name, Some v) in 
                                            (match !env with 
                                            | vars :: _ -> vars := a :: !vars
                                            | [] -> raise DepileEnvEmpty)
                else raise (AlreadyDeclaredVar (name, pos))
    | VarInit (name, value, pos) -> let v = eval_exp value
                                in if (change_val name v !env) then () else raise (UnknownVar (name, pos))
    | Repeat (x,i,pos) -> let n = int_of_float (as_float(eval_exp x)pos) in for _ = 1 to n do 
                        ignore (decode i); depile_env ()
                    done
    | While (c,i,pos) -> while (as_bool(eval_exp c)pos) do 
                       ignore (decode i); depile_env ()
                    done 
    | IfThen (c,i,pos) -> if (as_bool(eval_exp c)pos) then (ignore (decode i) ; depile_env ()) 
    | IfThenElse (c,i1,i2,pos) -> if (as_bool(eval_exp c)pos) then ( ignore (decode i1); depile_env ())  else (ignore (decode i2) ; depile_env ()) 
    | FunDecla (name, args, bloc, pos) -> if not (already_declared_fun name !envFun) 
                                            then let vs = setVars args in let a = ref (name, vs, bloc) in envFun := a :: !envFun
                                            else raise (AlreadyDeclaredFun (name, pos))
    | ProcCall (name, argsValue, pos) ->  
                            let rec aux liste = match liste with   
                            | a :: l -> (match !a with (str, vars , instr) -> if String.equal str name 
                                        then ( 
                                            let x = eval_list argsValue in
                                            let fresh_vars = List.map (fun r -> let (n,_) = !r in ref (n, None)) vars in 
                                            setFonction name pos fresh_vars x ; 
                                            ignore(decode ~initial_decla:fresh_vars instr); depile_env ();
                                            unsetFonction ())
                                            
                                        else aux l )
                            | [] -> raise (UnknownFun (name, pos))
                            in aux !envFun

    | Return (e,pos) -> if !context_actuel = Fonction then let v = eval_exp e in raise (ReturnValue v)
                        else raise (OutOfContextReturn pos)
    | Print e -> draw_string (as_string (eval_exp e))

let () = Eval.decode_ref := decode
                                