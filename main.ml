open Interp
open Graphics

exception Division_by_zero

let is_tested = Sys.getenv_opt "NO_WAIT" = None

let lexbuf = Lexing.from_channel stdin 

let ast =  try Parser.programme Lexer.token lexbuf
            with 
            | Lexer.Error a -> Printf.eprintf "Erreur lexicale %s\n" a; exit 1
            | Parser.Error -> let pos = lexbuf.Lexing.lex_curr_p in Printf.eprintf "Erreur syntaxique à la ligne : %d et à la colonne %d\n" pos.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol ); exit 1

let () =
        init_graphics () ;

        decode ast;
        
        if is_tested then ignore(read_key());
        close_graph () ;