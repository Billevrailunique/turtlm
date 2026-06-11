
(* This file was auto-generated based on "parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 118 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 113 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 110 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 103 ->
        "instruction non reconnu\n"
    | 101 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 100 ->
        "'CouleurPinceau' attend une expression bien formé\n"
    | 99 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 94 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 50
    | 49
    | 93 ->
        "les paramètres et les arguments d'une fonction sont séparé par des ',' et sont contenus dans des '( )'\n"
    | 92 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 105 
    | 98 
    | 91 ->
        "l'instruction bien formé est : Def [nom] ( [suite de paramètre séparé par des ','] ) Debut [suite d'instruction séparé par des ';', avec inclus, potentiellement un 'Retourn'] Fin\n"
    | 90 ->
        "le nom d'une fonction commende par une minuscule\n"
    | 87 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 86 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 83 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 81 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 102 
    | 77 ->
        "une suite d'instructions est séparé par des ';' et termine par ';'\n"
    | 76 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 75 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 74 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 73
    | 70 
    | 72 ->
        "l'instruction bien formé est : Repeter [expression] Fois Debut [suite d'intruction séparé par des ';'] Fin;\n"
    | 69 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 68 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 67 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 66 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 65 ->
        "un appel de fonction se fait comme : [nom] ( [suite d'expression séparé par des ','] )\n"
    | 63 ->
        "'==' est l'opérateur booléen d'égalité; une expression est attendue à gauche et à droite\n"
    | 62 ->
        "on attend un nom de variable qui commence avec une minuscule pour la déclarer"
    | 61 ->
        "une instruction ou 'Fin' est attendu \n"
    | 115 
    | 60 
    | 58 
    | 59 ->
        "l'instruction bien formé est : \nTant que [expression] Faire Debut [suite d'instruction séparé par des ';'] Fin;\n"
    | 57 ->
        "on attend soit une expression bien formé ou un ';' pour signifier la fin de l'instruction\n"
    | 54 ->
        "on attend une expression bien formé ou un ')'\n"
    | 42 
    | 28
    | 36
    | 26 
    | 38 
    | 30 
    | 24
    | 20 
    | 40
    | 64
    | 22 
    | 32
    | 44 
    | 34 ->
        "une expression bien formé est attendue à gauche et à droite d'un opérateur binaire\n"
    | 33 ->
        "une expression de la forme [expression] == [expression] a été comprise; mais le token qui suit ne semble pas faire sens\n"
    | 109 
    | 108 
    | 111 
    | 79 
    | 78 
    | 107 
    | 80 ->
        "l'instruction bien formé est : \nSi [expression] Alors Debut [suite d'instruction séparé par des ';'] Fin;\n
        ou alors :\nSi [expression] Alors Debut [suite d'instruction séparé par des ';'] Fin Sinon Debut [suite d'instruction séparé par des ';'] Fin;"
    | 71 
    | 31 -> 
        "dans une instruction conditionnel, un mot de transition est attandue entre l'expression et le bloc Debut--Fin comme 'Alors' pour un 'Si', ou 'Faire' pour un 'Tant que', ou 'Fois' pour un 'Repeter'\n"
    | 29 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 82 -> 
        "on attend soit '(' pour un appel ou une décla de fonction, soit '=' pour donner une valeur à une variable\n"
    | 27 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 25 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 23 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 19 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 17 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 13 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 12 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 11 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 10 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 8 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 6 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 4 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 1 ->
        "'LargeurPinceau' attend une expression bien formé\n"
    | 0 ->
        "on commence par une instruction, pas par une expression\n"
    | _ ->
        raise Not_found
