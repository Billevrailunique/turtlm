open Ast 

let rec already_declared name = function 
    | [] -> false     
    | l :: otre -> let rec aux y = match y with 
                                | [] -> already_declared name otre
                                | (str,_) :: _ when String.equal name str -> true
                                |  _ :: otre -> aux otre
                    in aux l

let rec already_declared_fun name = function 
        | [] -> false     
        | l :: otre -> let rec aux y = match y with
                                | [] -> already_declared_fun name otre
                                | (str,_,_) :: _ when String.equal name str -> true 
                                | _ :: otre -> aux otre
                        in aux l
    

let change_val name (value:value) (env:scope list) : (scope list* bool) = 
        let rec aux_scope env was_change : scope list * bool = match env with
                            | [] -> ([], was_change)
                            | scope :: otre -> 
                                    let rec aux_var y  = match y with 
                                                    | [] -> None
                                                    | (a,_) :: reste when String.equal name a -> Some( (a, Some value) :: reste)
                                                    | var :: reste -> (match aux_var reste with
                                                                        |None -> None
                                                                        |Some thg -> Some (var :: thg))
                            in (match aux_var scope with
                                | Some scope' ->((scope' :: otre), true)
                                | None ->  let (lst,b) = aux_scope otre was_change in  ((scope :: lst), b))
                        in aux_scope env false
                                

let rec get_val name = function 
                    | [] -> false,None
                    | l :: otre -> let rec aux y = match y with 
                                    | [] -> get_val name otre
                                    | (a,b) :: _ when  String.equal a name -> true,b 
                                    | _::z -> aux z
                                    in aux l

let rec  setVars (l:string list) = match l with 
                                | [] -> []
                                | str :: reste -> (str, None) :: setVars reste

let setFonction name pos (vars : (string * value option) list) argsValue : (string * value option) list =  
        let rec aux a b  = match (a,b) with 
                        | ([] , []) -> []
                        | ((a,_) :: l , s :: m) -> (a,Some s) :: aux l m
                        | (_::_, []) -> raise (ArgsMissingException (name, pos))
                        | ([], _::_) -> raise (TooManyArgsException (name,pos))
        in aux vars argsValue  

let afficherScope declared = let rec aux = function 
                | (name,_) :: l -> Printf.printf "%s\n" name ; aux l
                | [] -> () 
        in aux declared
        
let afficherfun func = match func with (name, _, _) -> Printf.printf "%s\n" name

let rec afficherScopeFun scopefun = match scopefun with
        | a :: l -> afficherfun a; afficherScopeFun l
        | [] -> ()
let afficherEnv state =  
        Printf.printf "affichage var début : \n";
        (let rec aux = function
                | a :: m -> afficherScope a ; aux m
                | [] -> ()
        in aux state.env);
        Printf.printf "fin\n";
        Printf.printf "affichage fonction début : \n";
        let rec aux = (function
        | a :: m -> afficherScopeFun a; aux m
        | [] -> ()) 
        in aux state.env_fun;
        Printf.printf "fin\n"

let no_double args pos  = 
        let rec aux acc liste = match liste with 
                | [] -> ()
                | str :: l -> if List.exists (fun arg -> String.equal arg str) acc then raise (AlreadyDeclaredVar (str,pos))
                        else aux (str :: acc) l
        in aux [] args