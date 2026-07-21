
(* This file was auto-generated based on "parser.messages". *)
(*on y touche pas du coup*)
(* Please note that the function [message] can raise [Not_found]. *)

(*int -> string
en fonction d'un numéros (celui de l'état du parse où une erreur s'est produit), on renvoie la string du message d'erreur correspondant*)
let message =
  fun s ->
    match s with
    | 90 ->
        "fin de fichier attendue; \"Fin\" est en trop, il manque peut-etre un bloc \"Debut\" correspondant\n"
    | 86 ->
        "on attend \"Fin\" pour fermer le bloc \"Debut ... Fin\", ou une nouvelle instruction\n"
    | 84 ->
        "on attend une instruction; une valeur bool\195\169enne seule n'est pas une instruction\n"
    | 83 ->
        "on attend \";\" apres l'instruction\n"
    | 82 ->
        "on attend \";\" pour terminer l'affectation, ou un op\195\169rateur\n"
    | 81 ->
        "on attend une expression apres \"=\"\n"
    | 79 ->
        "on attend \")\" pour fermer les arguments de l'appel\n"
    | 78 ->
        "on attend une expression (les arguments), ou \")\" pour un appel sans argument\n"
    | 77 ->
        "on attend \";\" pour terminer l'affectation, ou un op\195\169rateur\n"
    | 76 ->
        "on attend une expression apres \"=\" pour la valeur a affecter\n"
    | 75 ->
        "on attend \"=\" apres l'index, ou un op\195\169rateur\n"
    | 74 ->
        "on attend une expression apres \".\" pour designer l'index\n"
    | 73 ->
        "on attend \"=\" pour une affectation, \"(\" pour un appel, ou \".\" pour un index\n"
    | 71 ->
        "on attend \"Debut\" pour ouvrir le corps de la fonction\n"
    | 67 ->
        "on attend un nom de parametre apres \",\"\n"
    | 66 ->
        "on attend \",\" pour un autre parametre, ou \")\" pour terminer la liste des parametres\n"
    | 65 ->
        "on attend un nom de parametre ou \")\" pour une fonction sans parametre\n"
    | 64 ->
        "on attend \"(\" apres le nom de la fonction\n"
    | 63 ->
        "on attend un nom de fonction apres \"Def\"\n"
    | 60 ->
        "on attend \"Debut\" pour ouvrir le bloc du \"Sinon\"\n"
    | 59 ->
        "on attend \";\" pour terminer le \"Si\", ou \"Sinon\"\n"
    | 58 ->
        "on attend \"Debut\" pour ouvrir le bloc du \"Alors\"\n"
    | 57 ->
        "on attend \"Alors\" apres la condition du \"Si\", ou un op\195\169rateur\n"
    | 56 ->
        "on attend une expression apres \"Si\"\n"
    | 54 ->
        "on attend \"Debut\" pour ouvrir le bloc a repeter\n"
    | 53 ->
        "on attend \"Fois\" apres le nombre de r\195\169p\195\169titions, ou un op\195\169rateur\n"
    | 52 ->
        "on attend une expression apres \"Repeter\"\n"
    | 51 ->
        "on attend \";\" pour terminer l'instruction, ou un op\195\169rateur\n"
    | 50 ->
        "on attend une expression apres cette instruction\n"
    | 49 ->
        "on attend \";\" pour terminer la d\195\169claration, ou un op\195\169rateur\n"
    | 48 ->
        "on attend une expression apres \"=\"\n"
    | 47 ->
        "on attend \";\" pour terminer la d\195\169claration, ou \"=\" pour l'initialiser\n"
    | 46 ->
        "on attend un nom de variable apres \"Var\"\n"
    | 45 ->
        "on attend une instruction apres \"Debut\"; une valeur bool\195\169enne seule n'est pas une instruction\n"
    | 44 ->
        "on attend \"Debut\" pour ouvrir le bloc du \"Tant que ... Faire\"\n"
    | 43 ->
        "on attend \"Faire\" apres la condition du \"Tant que\", ou un op\195\169rateur\n"
    | 39 ->
        "on attend \")\" pour fermer la parenth\195\168se, ou un op\195\169rateur pour prolonger l'expression\n"
    | 37 ->
        "on attend \"]\" pour fermer la liste\n"
    | 35 ->
        "on attend \")\" pour fermer les arguments de \"GenN\"\n"
    | 34 ->
        "on attend \")\" pour fermer l'argument de \"GenC\", ou un op\195\169rateur pour prolonger l'expression\n"
    | 30 ->
        "on attend une expression; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 28 ->
        "on attend une expression; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 26 ->
        "on attend une expression; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 24 ->
        "on attend une expression; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 23 ->
        "expression termin\195\169e: on attend soit un op\195\169rateur pour la prolonger, soit le symbole qui cl\195\180t le contexte courant (\";\", \")\", \",\", \"=\", \"Alors\", \"Faire\", \"Fois\", selon o\195\185 l'on se trouve)\n"
    | 22 ->
        "on attend une expression apres une relation d'ordre; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 21 ->
        "on attend une expression ou une liste d'expression apres \"(\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 19 ->
        "on attend \")\" pour fermer les arguments\n"
    | 17 ->
        "on attend une expression apres \"(\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 14 ->
        "on attend une expression apres \".\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 13 ->
        "apres ce nom ou ce nombre, on peut avoir \"(\" pour un appel ou \".\" pour un index; sinon il manque probablement \";\" pour terminer l'instruction, ou un op\195\169rateur\n"
    | 11 ->
        "on attend une expression apres \"(\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 10 ->
        "on attend les arguments de \"GenC\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 9 ->
        "on attend une expression apres \"(\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 8 ->
        "on attend les arguments de \"GenN\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 7 ->
        "on attend une expression apres \"[\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 6 ->
        "on attend une expression apres \"(\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 5 ->
        "on attend une expression apres \"-\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 4 ->
        "on attend une expression apres \"Non\"; un mot-cl\195\169 d'instruction ne peut pas commencer une expression\n"
    | 1 ->
        "on attend une expression; \"Tant que\" est r\195\169p\195\169t\195\169\n"
    | 0 ->
        "on attend une instruction; pas une expression\n"
    | _ ->
        raise Not_found
