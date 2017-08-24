# Brainstorm

C'est dans ce document qu'on pourrait tous mettre nos idées de trucs sur lesquels travailler en fin de semaine. Évidemment, ce serait bien de rester dans la thématique du tennis. J'ai essayé de mettre quelques titres évocateurs afin de classer un peu les idées, mais vous pouvez les modifier/en ajouter.

## Visualisation

- On s'en était déjà parlé initiallement, mais ca serait bien d'avoir un script/fonction/méthode qui permettrait de ploter le graph d'un tournoi. On pourrait s'en servir autant pour voir des tournois dans le passé que pour ploter les résultats de nos futurs modèles prédictifs de matchs. Ça nous permettrait de visualiser rapidement l'outcome d'un tournoi donné.

## Modeling

Clairement, il nous faut identifier des variables potentielles pour nourrir nos modèles. D'après mon guy Sipko (2015), les variables suivantes pourraient être intéressantes: (c'est une liste longue, un "\" sépare deux variables...)

- ATP rank/points
- First serve success
- Winning on first/second serve %
- Overall winning on first serve %
- Winning on return %
[... Plusieurs autres du style (basée sur des transformations assez simples)]
- Fatigue
- Comeback from injury
- Polyvalence? (Completness)
- Advantage when serving
- Head-to-head balance (résultats précédents de ces deux joueurs)

On peut probablement en trouver d'autres plus complexes comme les dernières. Celles-ci on doit les "créer" de façon ingénieuse (comment quantifier la fatigue : en comptabilisant le nombre de minutes jouées dans les 3 derniers jours par exemple). Certaines variables mentionnées n'ont pas été utilisées finalement (comme ATP rank, qui ne représente pas vraiment la "forme" d'un certain joueur à un temps précis).

### Unsupervised

- Ça pourrait être nice de faire du clustering sur les joueurs en utilisant différentes stats de ceux-ci. On pourrait voir par la suite qu'est qui serait possible de faire avec ça, mais peut-être que si on est capable de dégager des constats intéressants quant au clustering de différents types de joueurs, cela pourrait éventuellement servir de input à une méthode supervisée. Par exemple, si deux joueurs n'ont pas joué beaucoup de matchs l'un contre l'autre, on pourrait utiliser les matchs d'un joueur contre le même style de joueur. 

 - J'aime l'idée. Notre guy Sipko (2015) a une variable Head-to-head (pour prendre en compte les matchs opposants les deux joueurs). On pourrait aller plus loin et définir des variables qui permettent d'aller chercher plus d'info sur des "games similaires" mettons.

- Une des grandes difficulté ici c'est de prendre en compte les adversaires d'un certain joueur, car ceux-ci ont une influence sur les stats de ce dernier. Si un joueur a perdu au premier/deuxième tour à ses trois premiers tournoi sur gazon, mais que ce fût contre Federer à chaque fois, on doit en prendre compte...

### Supervised

- À date, on a déjà parler des modèles logistique et réseau de neurones pour prédire l'issue d'un match. L'étape suivante est de prédire le résultat de tel ou tel tournoi. Ceci pourrait potentiellement se faire encore avec un réseau de neurones, mais il y a aussi l'option d'utiliser nos modèles de bases et faire beaucoup de simulations (pour voir les scénarios les plus probables). C'est pas pour ajourd'hui de toute façon... On peut penser à d'autres choses qu'on voudrait prédire aussi : le ranking dans 1 an, la probabilité de blessure, le tennis féminin (je crois que ça c'est un must à long terme : intégrer les femmes au projet), ...

### Side projects -- PoolExpert pour des ligues amicales de tennis

Mon frère joue dans une ligue (vraiment amicale, c'est juste un groupe facebook là...) et il m'a expliqué leur système de pointage et comment il trouvait que c'était stupide. En fait, tu peux jouer le nombre de matchs que tu veux contre n'importe qui dans la ligue -- une victoire vaut 10 points et un set gangé 5 points (donc 20 points le vainqueur). Il leur avait proposé de faire un système pour lequel gagné contre le meilleur donne plus de points (un peu comme si tu jouais dans un meilleur tournoi). C'est une bonne idée, mais il reste encore des lacunes évidentes. Le problème majeur pour un système cohérent, c'est que n'importe qui joue des matchs contre n'importe qui, certains jouent plus de matchs que d'autres, certains toujours contre les mêmes, etc.

Pour faire un ranking qui a du sens, on pourrait tout d'abord imiter l'idée de l'année dans l'ATP. Certains joueurs font plus de tournois que d'autres: c'est légal de jouer plus pour faire plus de points, mais le même tournoi ne compte jamais 2 fois... Pour imiter ça, j'opterais pour une combinaison (flexible) de temps et de nombre de parties contre un même joueur : les seuls points qui compte sont ceux, par exemple, obtenus dans des matchs joués dans le dernier 6 mois et seulement tes 3 meilleurs contre un même joueur. Comme ça ton nombre de matchs total ne peut pas dépasser 3 * (Nombre de joueurs dans la ligue). Il y a certainement d'autres trucs nice à intégrer en plus.

Bref, je dis ça pcq un petit shiny vraiment simple qui fait ça ce serait cool. On met ça gratuit avec une petite pub et hop! J'ai développé l'idée pour le genre de ligue ou le nombre de matchs est vraiment aléatoire... Les gars/filles s'appellent et vont jouer comme bon leur semble, mais on peut facilement donner l'option de créer un nombre de tournoi fini et marcher comme dans l'ATP... Je lance ça de même.

@stecaron: Cool Sam j'aime l'idée de travailler là-dessus. Devrait pas être un big deal à faire en plus un coup qu'on se crée un compte pour déployer des Shiny. On pourrait même faire dequoi pour que le monde se crée leur compte (je pense que Shiny gère des créations d'accès et tout) pour avoir des vues personnalisées (stats générales, prochain match, analyses des dernières parties, etc) dépendemment de la personne qui se log. 

