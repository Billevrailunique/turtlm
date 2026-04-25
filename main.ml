open Interp
open Graphics

exception Division_by_zero

let is_tested = Sys.getenv_opt "NO_WAIT" = None
let is_interactif = Sys.getenv_opt "MODE_INTER" <> None


let () =
        init_graphics () ;
        
        if is_interactif 
        then 
                begin 
                Printf.printf "mode interactif :\n> ";
                let envext = ref [] in 
                let buf = Buffer.create 512 in
                while true do 
                        Buffer.clear buf;
                        try 
                                while true do
                                        Buffer.add_string buf (read_line());
                                done 
                        with 
                        | End_of_file -> Printf.printf "> "; let lexbuf = Lexing.from_string (Buffer.contents buf) in
                                         let ast =  try Parser.programme Lexer.token lexbuf
                                                with 
                                                | Lexer.Error a -> Printf.eprintf "Erreur lexicale %s\n" a; exit 1
                                                | Parser.Error -> let pos = lexbuf.Lexing.lex_curr_p in Printf.eprintf "Erreur syntaxique à la ligne : %d et à la colonne %d\n" pos.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol ); exit 1
                                        in envext := decode ~initial_decla:!envext ast 
                done
                end 
        else 
             begin    
                let lexbuf = Lexing.from_channel stdin in 

                let ast =  try Parser.programme Lexer.token lexbuf
                        with 
                        | Lexer.Error a -> Printf.eprintf "Erreur lexicale %s\n" a; exit 1
                        | Parser.Error -> let pos = lexbuf.Lexing.lex_curr_p in Printf.eprintf "Erreur syntaxique à la ligne : %d et à la colonne %d\n" pos.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol ); exit 1

                 in ignore(decode ast) ; 
                if is_tested then ignore(read_key()) ;
                end ;
        close_graph () 