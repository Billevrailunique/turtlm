# Projet Grammaires et Analyse Syntaxique 2026

## Commande

à partir de la racine du projet :

### Compilation:
``dune build`` 
 
### Exécution normale: 
``_build/default/main.exe < sample/exemple1``

### Exécution intéractive:

``_build/default/main.exe``\
``BaisserPinceau;`` \
*Entré puis Crtl-D* \
``Si Vrais Alors Debut``\
``Avancer 10;``\
``Fin Sinon Debut``\
``Tourner 90;``\
``Fin;``\
*Entré puis Crtl-D* \
\
Dans le dossier sample se trouvent tous les exemples à votre disposition de la forme "exemple[i]" pour i allant de 1 à 25; ainsi qu'un fichier "fractale". \
Les exemples 1,7,10,15,16 sont directement issus du sujet.\
\
Il est possible de lancer l'ensemble des exemples à travers la commande
``./test.sh``\
\
**Attention :** en rasion de l'ouverture/fermeture rapide de multiples fenêtre Graphics, un bug peut survenir et bloquer le terminal. Dans ce cas, Crtl-C et relancé la commande jusqu'à ce que ça marche.  \

Dans le dossier sample_ans se trouvent les sorties erreurs (vide sinon) respectives des exécutions des sample/exemple[i].


## Contenu

Sur cette branch se trouvent, en plus de la partie obligatoire, les ajouts bonus suivants :
- fonctions et procédures
- variables supportant les types couleur, valeur numérique, booléen et string
- génération aléatoire d'entier et de couleur
- mode intéractif
- l'opérateur Mod et la fonction Afficher
- l'opérateur moins unaire