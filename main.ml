open Interp
open Graphics
open Ast
open Printf

let is_tested = Sys.getenv_opt "NO_WAIT" = None
let is_interactif = Unix.isatty Unix.stdin

module Err = MenhirLib.ErrorReports
module MInter = Parser.MenhirInterpreter
module Lex = MenhirLib.LexerUtil


let running_error e = 
  let msg,pos,while_running = match e with
    | Division_by_zero pos ->  "division par 0\n", Some pos,true
    | TooManyArgsException (name,pos) -> sprintf "trop d'argument donné à la fonction %s\n" name,Some pos, false
    | ArgsMissingException (name ,pos) -> sprintf "pas assez d'argument donné à la fonction %s\n" name, Some pos, false
    | EnvEmpty ->  "environnement vide lorsque dépiler", None,true
    | OutOfBoundsCursor pos ->  "curseur en dehors de l'écran\n", Some pos, true
    | OutOfBoundsPencilWidth pos ->  "largeur pinceau trop grande\n", Some pos , true
    | AlreadyDeclaredVar (name,pos) -> sprintf "variable %s déjà déclaré\n" name, Some pos, false
    | AlreadyDeclaredFun  (name,pos) -> sprintf "fonction %s déjà déclaré\n" name, Some pos, false
    | UnknownFun (name,pos) -> sprintf  "fonction %s inconnu\n" name, Some pos, false
    | UnknownVar (name, pos) -> sprintf "variable %s inconnu\n" name,Some pos, false
    | OutOfContextReturn pos ->  "return en dehors d'une fonction\n", Some pos, false
    | NotYetInitVar (name,pos) -> sprintf "variable %s pas encore initialisé\n" name, Some pos, false
    | Invalid_argumentGenN pos ->  "GenN mal utiliser\n", Some pos, true
    | FloatWaited pos->  "float attandu\n", Some pos, true
    | BoolWaited pos->  "bool attendu\n", Some pos, true
    | ColorWaited pos->  "couleur attendu\n", Some pos, true
    | _ -> eprintf "erreur non pris en charge"; exit 1 
  in
  let location = sprintf "File \"%s\"\n" Sys.argv.(1) in
  let type_err = sprintf "Error %s" (if while_running then "Run : " else "PreRun : ")  in 
  let indication = match pos with 
                    | Some e -> sprintf "à la ligne %d, char %d\n" e.Lexing.pos_lnum e.Lexing.pos_cnum 
                    | None -> "\n" in
  eprintf "%s%s%s%s" location type_err indication msg; exit 1


(*n'est executé qu'une fois, lorsqu'on réduit à l'axiome*)
let run v = try ignore(pretraitement v);ignore(decode v)
with e -> running_error e

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
    | Shifting _ 
    | AboutToReduce _ -> let checkpoint = MInter.resume checkpoint in parse lexbuf buffer supplier source checkpoint 
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
        