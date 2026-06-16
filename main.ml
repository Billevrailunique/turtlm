open Interp
open Graphics
open Ast
open Printf

let is_tested = Sys.getenv_opt "NO_WAIT" = None
let is_interactif = Unix.isatty Unix.stdin

module Err = MenhirLib.ErrorReports
module MInter = Parser.MenhirInterpreter

(*n'est executé qu'une fois, lorsqu'on réduit à l'axiome*)
let run v = try ignore(decode v)
with 
        | Division_by_zero pos -> eprintf "division par 0 à la ligne %d\n" pos.Lexing.pos_lnum
        | TooManyArgsException (name,pos) -> eprintf "trop d'argument donné à la fonction %s à la ligne %d\n" name pos.Lexing.pos_lnum
        | ArgsMissingException (name ,pos) -> eprintf "pas assez d'argument donné à la fonction %s à la ligne %d\n" name pos.Lexing.pos_lnum
        | EnvEmpty -> eprintf "environnement vide lorsque dépiler"
        | OutOfBoundsCursor pos -> eprintf "curseur en dehors de l'écran à la ligne %d\n" pos.Lexing.pos_lnum
        | OutOfBoundsPencilWidth pos -> eprintf "largeur pinceau trop grande à la ligne %d\n" pos.Lexing.pos_lnum
        | AlreadyDeclaredVar (name,pos) -> eprintf "variable %s déjà déclaré quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum
        | AlreadyDeclaredFun  (name,pos) -> eprintf "fonction %s déjà déclaré quand on arrive à la ligne %d\n" name pos.Lexing.pos_lnum
        | UnknownFun (name,pos) -> eprintf "fonction %s inconnu à la ligne %d\n" name pos.Lexing.pos_lnum
        | UnknownVar (name, pos) -> eprintf "variable %s inconnu à la ligne %d\n" name pos.Lexing.pos_lnum
        | OutOfContextReturn pos -> eprintf "return en dehors d'une donction à la ligne %d\n" pos.Lexing.pos_lnum
        | NotYetInitVar (name,pos) -> eprintf "variable %s pas encore initialisé à la ligne %d\n" name pos.Lexing.pos_lnum
        | Invalid_argumentGenN pos -> eprintf "GenN mal utiliser à la ligne %d\n" pos.Lexing.pos_lnum
        | FloatWaited pos-> eprintf "float attandu à la ligne %d\n" pos.Lexing.pos_lnum
        | BoolWaited pos-> eprintf "bool attendu à la ligne %d\n" pos.Lexing.pos_lnum
        | ColorWaited pos-> eprintf "couleur attendu à la ligne %d\n" pos.Lexing.pos_lnum
        | _ -> eprintf "erreur non pris en charge" 
        

let env checkpoint =
  match checkpoint with
  | MInter.HandlingError env -> env
  | _ -> assert false

let state checkpoint =
  MInter.current_state_number (env checkpoint)

let syntax_error checkpoint = let num = (state checkpoint) in printf "%s(echec état %d)\n" (ParserMessages.message num) num ;  close_graph () 

let rec parse lexbuf checkpoint =  
  match checkpoint with
    | MInter.InputNeeded _ -> (try (let token = Lexer.token lexbuf in 
                                let startp = lexbuf.lex_start_p
                                and endp = lexbuf.lex_curr_p in
                                let checkpoint = MInter.offer checkpoint (token, startp, endp) in 
                                parse lexbuf checkpoint)
                              with
                              | Lexer.Error msg -> printf "Erreur lexicale %s\n" msg; exit 1)
    | MInter.Shifting _ 
    | MInter.AboutToReduce _ -> let checkpoint = MInter.resume checkpoint in parse lexbuf checkpoint 
    | HandlingError _ -> syntax_error checkpoint
    | Accepted v -> run v 
    | Rejected -> assert false


let mode_fichier () = let lexbuf = Lexing.from_channel stdin in parse lexbuf (Parser.Incremental.programme lexbuf.lex_curr_p);
                      if is_tested then try ignore(read_key ()) 
                                        with | _ -> ()

let () = init_graphics ();
        mode_fichier () ;
        close_graph ()
        