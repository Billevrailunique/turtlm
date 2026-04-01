(* Ce fichier contient une brève démo de la bibliothèque Graphics d'OCaml.
 * Il n'est pas utile de le conserver dans la version finale du projet.
 *
 * Voir la doc de la bibliothèque Graphics pour plus de détails
 * https://ocaml.org/manual/4.07/libref/Graphics.html
 *)
open Graphics

let () =
  (* Ouverture d'une fenêtre graphique *)
  open_graph " 800x800"; (* Attention à l'espace au début *)
  set_window_title "Exemple d'utilisation de Graphics";

  (* Taille et couleur du trait. *)
  set_line_width 5;
  set_color blue;

  (* Déplace le curseur aux coordonnées (400, 400) sans dessiner *)
  moveto 400 400;

  (* Trace trois lignes en coordonnées absolues avec [Graphics.lineto] *)
  lineto 600 300;
  lineto 500 500;
  lineto 200 400;

  (* Trace une ligne de longueur 100 et d'angle 45 degrés,
   * en coordonnées relatives avec [Graphics.rlineto]
   *)
  set_color red;
  let angle = 45. *. Float.pi /. 180. in (* angle en radians *)
  let distance = 100. in
  let dx = int_of_float (distance *. cos angle) in
  let dy = int_of_float (distance *. sin angle) in
  rlineto dx dy;

  (* Attend l'appui d'une touche et ferme la fenêtre *)
  ignore(read_key ());
  close_graph ()
