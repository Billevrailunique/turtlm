open Ast 

let (env:environnement) = ref []
let (envFun:fonction list ref) = ref []
let context_actuel = ref Global
let seed_init = ref false

let rec already_declared name = function 
    | [] -> false     
    | l :: otre -> let rec aux y = match y with 
                                | [] -> already_declared name otre
                                | a :: reste -> match !a with (str,_) -> (if String.equal name str then true else aux reste)
                    in aux !l

and already_declared_fun name = function 
                                | [] -> false     
                                | f :: otre -> let (str,_,_) = !f in (if String.equal name str then true else already_declared_fun name otre)
    
and change_val name value = function
                            | [] -> false
                            | l :: otre -> 
                                    let rec aux y = match y with 
                                                    | [] -> change_val name value otre
                                                    | r :: reste -> (let (a,_) = !r in if String.equal name a
                                                                                                then (r := (a, Some value) ; true)
                                                                                                else aux reste)
                                            in aux !l
                                

and get_val name = function 
                    | [] -> false,None
                    | l :: otre -> let rec aux y = match y with 
                                    | [] -> get_val name otre
                                    | r :: z -> (match !r with (a,b) -> if String.equal a name then true,b else aux z)
                                    in aux !l

and setVars (l:string list) = match l with 
                                | [] -> let (vs: variable list) = [] in vs
                                | str :: reste -> ref (str, None) :: setVars reste

and setFonction name pos vars argsValue = context_actuel := Fonction; 
        let rec aux a b  = match (a,b) with 
        | ([] , []) -> ()
        | (r :: l , s :: m) -> let (a,_) = !r in r:= (a,Some s) ; aux l m
        | (_::_, []) -> raise (ArgsMissingException (name, pos))
        | ([], _::_) -> raise (TooManyArgsException (name,pos))
        in aux vars argsValue  

and unsetFonction () = context_actuel := Global

and resetEnv () =  env := []; envFun := []

and afficherDeclared declared = let rec aux = function 
                | a :: l -> (match !a with (name,_) -> Printf.printf "%s\n" name) ; aux l
                | [] -> () 
        in aux !declared
        
and afficherfun (func:fonction) = match !func with (name, _, _) -> Printf.printf "%s\n" name
and afficherEnv () =  
        Printf.printf "affichage var début : \n";
        (let rec aux = function
                | a :: m -> afficherDeclared a ; aux m
                | [] -> ()
        in aux !env);
        Printf.printf "fin\n";
        Printf.printf "affichage fonction début : \n";
        let rec aux = (function
        | a :: m -> afficherfun a; aux m
        | [] -> ()) 
        in aux !envFun;
        Printf.printf "fin\n";
