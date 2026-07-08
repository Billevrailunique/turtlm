open Interp
open Graphics
open Ast
open Printf

let is_tested = Sys.getenv_opt "NO_WAIT" = None

module Err = MenhirLib.ErrorReports
module MInter = Parser.MenhirInterpreter
module Lex = MenhirLib.LexerUtil

let file_semicolon = "semicolon_error_state"
let file_end = "end_error_state"
let file_start = "start_error_state"



let create_file token file = ignore (Sys.command ("awk 'BEGIN{RS=\"State \"} NR>1{ match ($0, /^[0-9]+/); num = substr($0, RSTART, RLENGTH); if ($0 ~ /On " ^ token ^"/) print num }' parser.automaton > " ^ file))
let () = create_file "SEMICOLON" file_semicolon
let () = create_file "END" file_end
let () = create_file "START" file_start


let error_state_number file = 
  let rec aux acc l = match input_line l with 
                  | exception End_of_file -> acc
                  | n -> aux (int_of_string n ::acc) l 
in aux [] (open_in file)


let semicolon_error_state_number = error_state_number file_semicolon
let end_error_state_number = error_state_number file_end
let start_error_state_number = error_state_number file_start


let () = Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> close_graph();print_endline "\nbyebye !"; flush stdout; exit 0))

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
    | NegativeRepeat pos -> "in repeat X fois, X must be positiv\n", Some pos, true
    | _ -> eprintf "erreur non pris en charge"; exit 1 
  in
  let location = sprintf "File \"%s\"\n" Sys.argv.(1) in
  let type_err = sprintf "Error %s" (if while_running then "Run : " else "PreRun : ")  in 
  let indication = match pos with 
                    | Some e -> sprintf "à la ligne %d, char %d\n" e.Lexing.pos_lnum e.Lexing.pos_cnum 
                    | None -> "\n" in
  eprintf "%s%s%s%s" location type_err indication msg; exit 1


(*n'est executé qu'une fois, lorsqu'on réduit à l'axiome*)
let run v = let initial_state =  {draw = false;
                                  val_angle = 90.;
                                  env = []; 
                                  env_fun = [];
                                  deep = 0;
                                  seed_init = false} in 
             try ignore(pretraitement v);ignore(decode v initial_state)
with e -> running_error e

let get_env checkpoint =
  match checkpoint with
  | MInter.InputNeeded env 
  | MInter.Shifting (env,_,_)
  | MInter.AboutToReduce (env,_)
  | HandlingError env -> env
  | _ -> assert false

let in_state checkpoint =
  MInter.current_state_number (get_env checkpoint)

let syntax_error checkpoint buffer source = 
  let num = (in_state checkpoint) in
  let location = Lex.range (Err.last buffer) in
  let indication = sprintf  "Erreur syntaxique (echec in state %d) %s\n" num (Err.show (Err.extract source) buffer) in 
  try 
  printf "%s%s%s" location indication (ParserMessages.message num); flush stdout
with 
| Not_found -> (print_endline "probleme avec la génération du message d'erreur de syntaxe"; flush stdout)

let rec parse lexbuf buffer supplier source checkpoint =  
  match checkpoint with
    | MInter.InputNeeded _ -> (try 
                                let checkpoint = MInter.offer checkpoint (supplier ()) in 
                                parse lexbuf buffer supplier source checkpoint
                              with
                              | Lexer.Error msg -> printf "Erreur lexicale %s\n" msg; exit 1)
    | Shifting _ 
    | AboutToReduce _ -> let checkpoint = MInter.resume checkpoint in parse lexbuf buffer supplier source checkpoint 
    | HandlingError _ -> syntax_error checkpoint buffer source; close_graph ()
    | Accepted v ->  run v 
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

let initial_state =  {draw = false;
                      val_angle = 90.;
                      env = []; 
                      env_fun = [];
                      deep = 0;
                      seed_init = false}

let fresh_checkpoint () =
    Parser.Incremental.programme Lexing.dummy_pos
  
let rec loop_on_line b checkpoint state buffer =
    let rec aux b = match b with 
    | 0 -> ()
    | n -> (print_string ("  "); aux (n-1))
    in aux b;
    print_string ("> "); flush stdout;
    match input_line stdin with
    | exception End_of_file -> ()
    | line ->
      let source = if buffer = "" then line else buffer ^ "\n" ^ line in
      let lexbuf = Lexing.from_string (source) in
      let supplier = MInter.lexer_lexbuf_to_supplier Lexer.token lexbuf in
      let buffer, supplier = Err.wrap_supplier supplier in
      feed b checkpoint source supplier buffer state    

and feed b checkpoint source supplier buffer state =
    match checkpoint with
    | MInter.InputNeeded _ ->
              (try
                let checkpoint = MInter.offer checkpoint (supplier ()) in
                feed b checkpoint source supplier buffer state
              with
              | Lexer.Error msg ->
                Printf.eprintf "Erreur lexicale : %s\n%!" msg;
                loop_on_line 0 (fresh_checkpoint ()) initial_state ""
              | End_of_file ->
                loop_on_line 0 checkpoint state "") 
    | Shifting _
    | AboutToReduce _ ->
      let checkpoint = MInter.resume checkpoint in
      feed b checkpoint source supplier buffer state
    | Accepted v -> 
      let f = decode v state in 
      begin 
        match f with 
              | Continue s -> loop_on_line 0 (fresh_checkpoint ()) s ""
              | Returned _ -> assert false
      end
    | MInter.HandlingError env -> 
      let n = MInter.current_state_number env in 
        if List.exists ((=)n) semicolon_error_state_number 
        then recovery Parser.SEMICOLON source supplier buffer env state b
        else if List.exists ((=)n) end_error_state_number 
          then loop_on_line (1) (fresh_checkpoint ()) state source
          else if List.exists ((=)n) start_error_state_number 
            then (print_endline "Debut"; recovery Parser.START (source ^ "\nDebut") supplier buffer env state b)
            else 
        (syntax_error checkpoint buffer source; loop_on_line b (fresh_checkpoint ()) state "")
    | Rejected ->
      assert false

and recovery token source supplier buffer env state b= 
  let checkpoint = MInter.input_needed env in 
  let pos = snd (Err.last buffer) in 
  let triple_tok = (token,pos,pos) in 
  if MInter.acceptable checkpoint token pos 
  then let checkpoint = MInter.offer checkpoint triple_tok in feed b checkpoint source supplier buffer state

let mode_interactif () = loop_on_line 0 (fresh_checkpoint ()) initial_state ""

let () = init_graphics ();
        if (Array.length Sys.argv = 2)
        then mode_fichier () 
        else (if (Array.length Sys.argv = 1 ) then mode_interactif () else print_endline "arg invalide");
        close_graph ()
        