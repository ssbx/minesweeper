Voir: https://games-stats.com/steam/?title=minesweeper !

Interessant de voir que les seuls jeux payants qui n'aient pas fait de revenus
sont des multi joueurs. C'est vrai que c'est bizare pour une jeux de mine!

- menu choix grille (besoin de lib ou sys pour afficher charactères et/ou menus)
- rendre la fin de jeux réussie belle (particules, feux d'artifice ...)

# mines next gen

- une grille à progression horizontale qui défile de droite vers la gauche a
vitesse constante.
- le premier shot génère la grille. Puiseque la progression démare aussi tot,
il est preferable de démarer le premier shot à gauche de la grille.
- La grille va difficulté grandissante, et est garantie de pouvoir être finie.
- Si un emplacement est marqué sans mine, ou n'est pas marqué avec une mine, une
alerte survient alors que la mine est proche de sortir de l'écran, donnant une
chance au joueur de la corriger.
- peut être l'avancement se ferait par acoup, pour pas devoir viser les cases
avec la souris
- arcade sons/effets/explosions


customisation pour offrir
=========================

L'acheteur pourrait personnaliser
- le début du j'eux (joyeux anniversaire... plus ou moins loufoque, chant
exalté, que quand on croit que c'est finit, ça repars encore...
- le contenu du jeux, avec deep fake visages/personnages du jeux
- la fin du jeux deep faque

TODO jeux solitaire cartes avec progresion comme dans le jeux "im not a
designer" minesweeper ou un cube débloque d'autres.


pof1
====

- Défilement du niveau de droite à gauche.
- Un vaisseau suit la progression sur la gauche de l'écran
- Déminer, soit:
  - quand il trouve un flag (sous lui) il démine et incrémente un conteur
  - un vaisseau mobile va déminer les flag posé par le joueur
- Quand arrive sous lui (contre écran gauche) une case non découverte:
  - compteur est vide:
    - case sans mine: rien
    - case avec mine: explosion dégats au vaisseau
  - compteur plein SOIT:
    - il déclanche une explosion qui "nettoie" l'écran et reset le compteur
    - rien, c'est le joueur qui décide de manuellement lancer la bombe

Modules de déminage:
- plusieurs types de bombes
- un premier module va déterminer le type de bombe et colorer le flag (si
le flag est mal placé, couleur random)
- des lors le module approprié va ce charger du déminage
  - pour en extraire l'energie (participe au combo)
  - pour le faire imploser
- le joueur peut influencer l'ordre de déminage et le souhait de récolter ou
pas l'energie
- possibilitée de combos de trois couleurs identiques déminées d'affilé pour
gagner soit: energie bombe, score et amélioration auto du vaisseau (radiant
silvergun), points pour améliration vaiseau entre niveaux

------
(pas sur, mais trouver un truc pour créer des possibilité de combos qui
plafonnent)
-- 1 --
Armes joueur pour système de combo, on peut utiser la jauge energie pour
utiliser des armes spécifiques:
- découverte auto sans explosion du niveau +:
  - extracteur d'energie en promenant une sphère sur les cases
-- 2 --
vider le tableau de 3 des 4 couleurs puis déclancher la bombe


Tout début du jeux, on voi une sphere (planette), parcourue du chemin
que devra emprunter le joueur. Zoom vers le jeux. Lorsqu'il perd, zoom
inverse, et il vois sa progression.

Pièges parfois, explosion inévitable (compteur) à un moment, suivi d'un
vide, puis gros pacas de bombes qui met en difficulté (puisque compteur
vide)

Peut être boss déffilement du tableau dessous, besoin de récupérer de
l'énergie en déminant pour ensuite lancer la bombe manuelle sur le boss

Ecran d'accueil du jeux, un panorama de foret abimée par la guerre, et
des explosions intermitantes. Il faut déminer cette planète.

fin perdue, panorama de planète dévastée.
fin gagnée, panorama de forêe primaire pleine d'animaux

Amélioration vaisseau:
- achat de module spécifiques pour nouvelles mines
- nombre de modules de déminage mobile
- portée de la bombe
- reserve d'energie pour bombe
- reserve de points de vie du vaisseau mère

Communication équipage lors de diverses situations, comme dans Radiant
Silvergun des fenètres avec les personnages manga en visio.

particules
==========

- geometry wars: petro evol 2/2008, bizare créations

addictif(?) mais moche
======================

- drop7/2008, area/code

voir jeux
=========
- dongeon keeper/1997, bullfrog prod
- pikmin/2001, nintendo
- zoo keeper/2003, robot communications/success
- lumines/2004, Q entertainment
- meteos/2005, Q entertainment
- eets/2006, klei entertainment
- the innedible machine
- Ant style: orbient/2006, skip ltd
- intelligent Qube/97, sony
- tower bloxx/2005, digital chocolate
- space invaders extreme/2008, taito
- Q bert/82, gottlieb
- bookworm/2003
- flow/2006, that game company
- unpay/2006, Q entertainment
- slitherlink/2006, hudson soft
- gritter crunch/2008, capybara games
- galcon/2008, phil hassey
- edge/edgy/2008, mobygame

- boom blox bash party/2009, ea
- zen bound/2009, secret exit
- lumines live/2006, Q entertainment
- peggle/2007, popcap games
- puzzle league/2007, Intelligent systems
- puzzle quest/2007,, infinite interactive
- begeweled twist/2008, popcap games
- chimne/2010, zoé mode
- sokoban/82
- archon/83, free fall associates
- saolomon's key/86, tecmo
- dungeon master/87, ftl games (avec histoire commune)
- les incroyables machines du prof tim/92, dynamik
- devil dice/98, shift
- picros ds

ressources shaders
==================

[https://github.com/vanrez-nez/awesome-glsl?tab=readme-ov-file](https://github.com/vanrez-nez/awesome-glsl?tab=readme-ov-file)
[https://sibras.github.io/OpenGL4-Tutorials/docs/Tutorials/01-Tutorial1/](https://sibras.github.io/OpenGL4-Tutorials/docs/Tutorials/01-Tutorial1/)

perfs, TODO chipmunk/opengl
===========================

Lib haut niveau pour faire le max de trucs en C pour le rendering/iteration
des systèmes.

