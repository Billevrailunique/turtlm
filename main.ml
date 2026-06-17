open Interp
open Graphics
open Ast
open Printf

let is_tested = Sys.getenv_opt "NO_WAIT" = None
let is_interactif = Unix.isatty Unix.stdin

module Err = MenhirLib.ErrorReports
module MInter = Parser.MenhirInterpreter
module Lex = MenhirLib.LexerUtil

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
        

let get_env checkpoint =
  match checkpoint with
  | MInter.HandlingError env -> env
  | _ -> assert false

let state checkpoint =
  MInter.current_state_number (get_env checkpoint)

let syntax_error checkpoint buffer source = 
  let num = (state checkpoint) in
  let location = Lex.range (Err.last buffer) in
  let indication = sprintf  "Erreur syntaxique (echec in state %d) %s\n" num (Err.show (Err.extract source) buffer) in 
  eprintf "%s%s%s" location indication (ParserMessages.message num);  close_graph () 

let rec parse lexbuf buffer supplier source checkpoint =  
  match checkpoint with
    | MInter.InputNeeded _ -> (try 
                                let checkpoint = MInter.offer checkpoint (supplier ()) in 
                                parse lexbuf buffer supplier source checkpoint
                              with
                              | Lexer.Error msg -> printf "Erreur lexicale %s\n" msg; exit 1)
    | MInter.Shifting _ 
    | MInter.AboutToReduce _ -> let checkpoint = MInter.resume checkpoint in parse lexbuf buffer supplier source checkpoint 
    | HandlingError _ -> syntax_error checkpoint buffer source
    | Accepted v -> run v 
    | Rejected -> assert false

let mode_fichier () = 
  let source,lexbuf = Lex.read (Sys.argv.(1)) in 
  let supplier = MInter.lexer_lexbuf_to_supplier Lexer.token lexbuf in 
  let buffer, supplier = Err.wrap_supplier supplier in 
  parse lexbuf buffer supplier source (Parser.Incremental.programme lexbuf.lex_curr_p); 
  if is_tested 
  then 
    try ignore(read_key ()) 
    with | _ -> ()

let () = init_graphics ();
        mode_fichier () ;
        close_graph ()
        