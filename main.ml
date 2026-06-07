open Interp
open Graphics
open Ast
open Env

let is_tested = Sys.getenv_opt "NO_WAIT" = None
let is_interactif = Unix.isatty Unix.stdin

let parse lexbuf = try Parser.programme Lexer.token lexbuf
                with 
                | Lexer.Error a -> Printf.eprintf "Erreur lexicale %s\n" a; exit 1
                | Parser.Error -> let pos = lexbuf.Lexing.lex_curr_p in Printf.eprintf "Erreur syntaxique à la ligne : %d et à la colonne %d\n" pos.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol ); exit 1

(*TODO : mode_interactif n'est peut-être plus compatible avec le nouveau sys d'erreur*)
let mode_interactif () = 
                Printf.printf "mode interactif :\n> ";
                let envext = ref [] in 
                let buf = Buffer.create 512 in
                while true do 
                        Buffer.clear buf;
                        (try while true do
                                Buffer.add_string buf (read_line());
                                done 
                        with 
                        | End_of_file -> ());
                        Printf.printf "> "; 
                        let ast = parse (Lexing.from_string (Buffer.contents buf)) in envext := decode ~initial_decla:!envext ast;
                done
                
let mode_fichier () = let ast =  parse (Lexing.from_channel stdin) in
                         pretraitement ast; resetEnv () ; init_graphics () ; ignore(decode ast);
                        if is_tested then ignore(read_key())

let () =
        try (if is_interactif 
                then (init_graphics () ; mode_interactif ())
                else mode_fichier ())
        with
        | Division_by_zero pos -> Printf.eprintf "division par 0 à la ligne %d\n" pos.Lexing.pos_lnum
        | TooManyArgsException (name,pos) -> Printf.eprintf "trop d'argument donné à la fonction %s à la ligne %d\n" name pos.Lexing.pos_lnum
        | ArgsMissingException (name ,pos) -> Printf.eprintf "pas assez d'argument donné à la fonction %s à la ligne %d\n" name pos.Lexing.pos_lnum
        | EnvEmpty -> Printf.eprintf "environnement vide lorsque dépiler"
        | OutOfBoundsCursor pos -> Printf.eprintf "curseur en dehors de l'écran à la ligne %d\n" pos.Lexing.pos_lnum
        | OutOfBoundsPencilWidth pos -> Printf.eprintf "largeur pinceau trop grande à la ligne %d\n" pos.Lexing.pos_lnum
        | AlreadyDeclaredVar (name,pos) -> Printf.eprintf "variable %s déjà déclaré quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum
        | AlreadyDeclaredFun  (name,pos) -> Printf.eprintf "fonction %s déjà déclaré quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum
        | UnknownFun (name,pos) -> Printf.eprintf "fonction %s inconnu à la ligne %d\n" name pos.Lexing.pos_lnum
        | UnknownVar (name, pos) -> Printf.eprintf "variable %s inconnu à la ligne %d\n" name pos.Lexing.pos_lnum
        | OutOfContextReturn pos -> Printf.eprintf "return en dehors d'une donction à la ligne %d\n" pos.Lexing.pos_lnum
        | NotYetInitVar (name,pos) -> Printf.eprintf "variable %s pas encore initialisé à la ligne %d\n" name pos.Lexing.pos_lnum
        | Invalid_argumentGenN pos -> Printf.eprintf "GenN mal utiliser à la ligne %d\n" pos.Lexing.pos_lnum
        | FloatWaited pos-> Printf.eprintf "float attandu à la ligne %d\n" pos.Lexing.pos_lnum
        | BoolWaited pos-> Printf.eprintf "bool attendu à la ligne %d\n" pos.Lexing.pos_lnum
        | ColorWaited pos-> Printf.eprintf "couleur attendu à la ligne %d\n" pos.Lexing.pos_lnum
        | _ -> Printf.eprintf "erreur non pris en charge" 
        ;
        close_graph ()