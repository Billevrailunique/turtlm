#  TurtLM

> Interpréteur d'un langage de dessin, développé en OCaml.

## Présentation

TurtLM est un interpréteur pour un langage permettant de créer des dessins en manipulant une tortue virtuelle à l'aide d'instructions telles que les déplacements, rotations, structures conditionnelles, boucles et fonctions.

Le langage est Turing-complet.

Le lexer utilise OCamlLex. \
Le parser utilise Menhir. \
L'affichage est géré par la bibliothèque Graphics

---

# Fonctionnalités

-  Mode interactif
-  Exécution depuis un fichier
-  Message d'erreur de syntaxe précis
-  Restauration après erreur (mode interactif uniquement)



---

# Compilation

Le projet utilise **Dune**.

```bash
dune build
```

---

# Exécution

Exécuter un programme :

```bash
_build/default/main.exe sample/exemple1
```

Mode interactif :

```bash
_build/default/main.exe
```

Puis saisir les commandes du langage.

---

# Exemples

Le dossier `sample/` contient de nombreux programmes pour tester l'interpréteur, notamment une fractal (`sample/fractal`) et une simulation de Gosper Glider Gun du jeu de la vie de Conway (`sample/game_of_life`).

---

# Dépendances

- OCaml
- Dune
- Menhir
- ocamllex
- Graphics


---

# Tests

Le dépôt contient un script :

```bash
./test.sh
```

exécutant un ensemble de programme de `sample/`avant de tester l'égalité avec la sortie erreur attandue dans `sample_ans/`.


---

# Licence

Projet académique.
