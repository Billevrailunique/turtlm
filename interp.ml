open Ast 
open Env 
open Eval
open Graphics

let angle val_angle = val_angle *. Float.pi /. 180. 

let init_graphics () = open_graph " 800x800";
        set_window_title "projet GAS6";
        set_line_width 1;
        set_color black;
        moveto 400 400
        
let rec decode bloc (state:state) = let up_state =  {draw = state.draw; val_angle = state.val_angle; env = [] :: state.env; env_fun = [] :: state.env_fun; deep = state.deep; seed_init = false} in 
    let rec carpeDiem state liste = match liste with 
        | [] -> Continue state
        | instr :: reste -> match next_action state instr with 
                            | Continue s -> carpeDiem s reste 
                            | Returned _ as r -> r
in carpeDiem up_state bloc

and next_action (state:Ast.state) instr : flow = match instr with
    | Draw_on -> Continue {state with draw = true} 
    | Draw_off -> Continue {state with draw = false} 
    | Move (e,pos) -> let val_distance,state = (eval_exp state ~decode e) in 
                let distance = as_float val_distance pos in  
                let a = angle state.val_angle in 
                let dx = (int_of_float (distance *. cos a)) in
                let dy = (int_of_float (distance *. sin a)) in 
                let nx = current_x () + dx in 
                let ny = current_y () + dy
                 in if nx <= size_x () && ny <= size_y () && nx >= 0 && ny >= 0 then
                     (if state.draw then (rlineto dx dy; Continue state)
                        else (rmoveto dx dy;Continue state)) 
                    else raise (OutOfBoundsCursor pos)
    | Turn (e,pos) -> let val_val_angle, state = (eval_exp state ~decode e)in 
                        Continue {state with val_angle = state.val_angle +. as_float(val_val_angle) pos} 
    | CouleurPinceau (c,pos) -> let val_couleur,state = (eval_exp state ~decode c) in
                                let couleur = as_color val_couleur pos in set_color couleur; Continue state
    | LargeurPinceau (e, pos) -> let val_largeur,state = (eval_exp state ~decode e) in 
                                let value = int_of_float (as_float val_largeur pos) in if value < 878 then (set_line_width value; Continue state) else raise (OutOfBoundsPencilWidth pos)
    | VarDecla (name, _) -> let a = (name, None) in 
                                (match state.env with 
                                | vars :: other -> Continue {state with env = (a :: vars) :: other }  
                                | [] -> raise EnvEmpty)
    | VarDeclaInit (name, value, _) ->  let v,state = eval_exp state ~decode value in let a = (name, Some v) in 
                                            (match state.env with 
                                            | vars :: other -> Continue {state with env = (a :: vars) :: other }  
                                            | [] -> raise EnvEmpty)
    | VarInit (name, value, _) -> let (v,state) = eval_exp state ~decode value in 
                                let env,_ = change_val name v state.env
                                in Continue {state with env = env}
    | Repeat (x,i,pos) -> let valuated,up_state = eval_exp state ~decode x in 
                        let n = int_of_float (as_float valuated pos) in 
                        if n < 0 then raise (NegativeRepeat pos) else 
                        let rec repeat i state n = 
                            if n == 0 then Continue state else
                                match decode i state  with 
                                    | Continue s -> repeat i s (n-1)
                                    | Returned _ as r -> r 
                            in repeat i up_state n
    | While (c,i,pos) -> let rec tantque c state i = 
                            let valuated,up_state = (eval_exp state ~decode c) in
                            if as_bool valuated pos then 
                                match decode i up_state with
                                    | Continue s -> tantque c s i
                                    | Returned _ as r -> r 
                        else Continue state
                        in tantque c state i 
    | IfThen (c,i,pos) -> let condition,state = eval_exp state ~decode c in if (as_bool condition pos) then decode i state else Continue state
    | IfThenElse (c,i1,i2,pos) -> let condition,state = eval_exp state ~decode c in if (as_bool condition pos) then decode i1 state  else decode i2 state 
    | FunDecla (name, args, bloc, _) ->  let vs = setVars args in let a = (name, vs, bloc) in 
                                (match state.env_fun with 
                                | scope :: otre -> Continue {state with env_fun = (a :: scope) :: otre }  
                                | [] -> raise EnvEmpty)
    | ProcCall (name, argsValue, pos) ->  let rec aux_env env = match env with 
                                        | [] -> assert false
                                        | scope :: otre ->
                                            let rec aux_scope scope = match scope with   
                                                |  (str, vars , instr) :: _ when String.equal str name 
                                                            ->  let valuated,up_state = eval_list state ~decode argsValue in 
                                                                let env = setFonction name pos vars valuated in 
                                                                let up_state  = {draw = up_state.draw; val_angle = up_state.val_angle; env = env :: up_state.env; env_fun = up_state.env_fun; seed_init = up_state.seed_init; deep = up_state.deep+1} in
                                                                ignore(decode instr up_state); Continue state
                                                | _ :: l -> aux_scope l
                                                | [] -> aux_env otre 
                                            in aux_scope scope
                                        in aux_env state.env_fun

    | Return (e,_) -> let v,_ = eval_exp state ~decode e in Returned (v,state)
                        
    | Print e -> let str,state = eval_exp state ~decode e in  draw_string (as_string str); Continue state

(*check le nombre de param aux fonction est correcte, que l'ordre dans lequel les var et les fonctions sont décla, init, used est correcte*)
(*TODO : check pour les doublons des param de la fonction *)
(*TODO : check pour les funcall (dans les expressions)*)
    
let check (state:state) instr : state = match instr with
    | ProcCall (name, argsValue, pos) -> if already_declared_fun name state.env_fun then 
                                let rec aux_env env = match env with 
                                        | [] -> raise (UnknownFun (name, pos))
                                        | scope :: otre ->
                                            let rec aux_scope scope = match scope with   
                                                |  (str, vars , _) :: _ when String.equal str name 
                                                            ->  let valuated = List.map (fun _ -> No) argsValue in 
                                                                let env = setFonction name pos vars valuated in 
                                                                let state = {draw = state.draw; val_angle = state.val_angle; env = env :: state.env; env_fun = state.env_fun; seed_init = state.seed_init; deep = state.deep+1} in
                                                                state
                                                | _ :: l -> aux_scope l
                                                | [] -> aux_env otre 
                                            in aux_scope scope
                                        in aux_env state.env_fun 
                                else raise (UnknownFun (name, pos))
    | FunDecla (name, args, bloc, pos) -> if not (already_declared_fun name state.env_fun) 
                                            then let vs = setVars args in 
                                            let a = (name, vs, bloc) in 
                                            (match state.env_fun with 
                                                | scope :: otre -> {state with env_fun = (a :: scope) :: otre }  
                                                | [] ->  raise EnvEmpty)
                                            else raise (AlreadyDeclaredFun (name, pos))
    | Return (_,pos) -> if state.deep > 0 then state
                        else raise (OutOfContextReturn pos)
    | VarDecla (name, pos) -> if not (already_declared name state.env)  then
                                let a = (name, None) in 
                                (match state.env with 
                                    | scope :: otre -> {state with env = (a :: scope) :: otre}
                                    | [] -> raise EnvEmpty)
                            else raise (AlreadyDeclaredVar (name,pos))
    | VarDeclaInit (name, _, pos) -> if not (already_declared name state.env) then 
                                        let v = No in 
                                        let a = (name, Some v) in 
                                            (match state.env with 
                                                | scope :: otre -> { state with env = (a:: scope) :: otre}
                                                | [] -> raise EnvEmpty)
                                    else raise (AlreadyDeclaredVar (name, pos))
    | VarInit (name, _, pos) -> let v = No in 
                                let (env,check) = change_val name v state.env in 
                                if check then {state with env = env} else raise (UnknownVar (name, pos))
    | _ -> state

let pretraitement bloc = let state = {draw = false; val_angle = 90.; env = [[]]; env_fun = [[]]; deep = 0;seed_init = false} in 
                            List.fold_left check state bloc
                                
