open Ast 

let (env:environnement) = ref []

let rec already_declared name = function 
    | [] -> false     
    | a :: otre -> match !a with (str,_) -> (if String.equal name str then true else already_declared name otre)
    
and change_val name value = function
                            | [] -> false
                            | r :: otre -> match !r with 
                                    | (a,_) -> if String.equal name a
                                                then (r := (a, Some value) ; true)
                                                else change_val name value otre    

and flatten_spe (env:declared list) = match env with 
                                | [] -> []
                                | r :: suite -> !r @ flatten_spe suite

and get_val name = function 
                    | [] -> None
                    | r :: otre -> match !r with (a,b) -> if String.equal a name then b else get_val name otre
