Readme
Bienvenue sur le jeu "Camel_Legacy"!

INSTALLATION :﻿

Pour y jouer, veillez à avoir OCaml 5.3, le package Notty et dune d'installé sur votre machine. Pour lancer le jeu, ouvrez votre terminal dans le dossier projet_prog et exécutez les commandes suivantes :﻿

"dune build"﻿
"dune exec rogue"﻿

Pour relancer une partie juste après avoir joué, vous n'avez besoin que de lancer la deuxième commande.﻿

PRÉSENTATION DU JEU :﻿

"" Est un rogue like où vous contrôlez un chameau qui se déplace dans le désert (généré à chaque partie par world.ml). Votre but est de survivre dans un environnement hostile, où les ennemis apparaissent par vague.﻿

Une vague d'ennemis apparaît tous les 50 tours, et chaque tour où le chameau reste en vie correspond à un point dans son score final. Comme les ennemis des vagues précédentes ne disparaissent pas naturellement et que le nombre d'entités qui spawn est linéaire en le nombre de vagues, il vous faudra en éliminer régulièrement pour ne pas être submergé (géré par vague.ml, sauf le système de score qui dépend de player.ml).﻿

Les différentes entités présentes sur la map sont les suivantes :﻿
- Le chameau (vous). Après avoir ramassé un boost (durée de 20 tours) vous devenez un super chameau. Le super chameau est invulnérable et surpuissant. Il est capable de tuer n'importe quelle entité. (fichier player.ml)﻿
- Les serpents : ils se déplacent aléatoirement. Ils peuvent tuer le player et les araignées en allant sur leur case. (fichier snake.ml)﻿
- Les éléphants : ils se déplacent aléatoirement jusqu'à ce qu'ils aient le chameau en vue. Lorsque c'est le cas, ils se mettrons à charger dans sa direction pendant 10 tours avant de revenir à la normale. Si un éléphant se heurte à un cactus pendant cette phase, il sera sonné pendant 20 tours, laps de temps durant lequel il deviendra vulnérable face au chameau. Un éléphant peut aller sur n'importe quelle case (autre que les cacti, les robots et autres éléphants) et tuera l'entité qui s'y trouve à coup sûr. Les robots ne sont pas normalement vulnérables aux éléphants, mais si ces derniers chargent, ils peuvent les écraser.(fichier elephant.ml)﻿
- Les araignées : se déplacent aléatoirement et peuvent tuer le player en allant sur sa case. À chaque tour, elles ont 1% de chance de faire spawn un nid d'araignée sur une case adjacente vide. (fichier spider.ml)﻿
- Les nids d'araignée : n'apparaissent pas sans l'action d'une araignée. Ne ce déplacent pas. Tous les 20 tours, essaie de faire spawn une araignée sur une case adjacente vide. Après 3 essais, le nid s'autodétruit. Toutes les entités peuvent casser le nid en marchant dessus, mis à part les araignées qui les éviteront. (fichier spider.ml)﻿
- Le robot : se déplace en direction du player (en utilisant l'algorithme A*) (*quand Anas l'aura fait*). Il peut tuer toutes les autres entités (sauf l'éléphant). (fichier robot.ml)﻿


Pour "tuer" une entité de manière efficace, elles possèdent toutes un identifiant (attribué dans l'ordre d'apparition des créatures) qui permet de sortir le thread de la queue (fichier utils.ml)﻿

DÉTAILS SUR LES AUTRES FICHIERS﻿

main.ml lance le jeu.﻿
utils.ml contient des fonctions utiles dans la plupart des autres fichiers.﻿
engine.ml contrôle les fils (donne la main au premier de la file, et performe "End_Of_Turn" de manière à ce qu'une entité morte ne se replace pas dans la file).﻿
ui.ml gère l'affichage des différentes entités sur la map.﻿

EXTENSIONS ﻿

En comparaison avec le jeu de base, "" possède un système de score et de vagues, en plus de l'ajout du robot et des dynamiques de kill entre les différentes entités.
