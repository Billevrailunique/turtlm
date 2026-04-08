open Ast

let lexbuf = Lexing.from_channel stdin 

let ast = Parser.programme Lexer.main lexbuf