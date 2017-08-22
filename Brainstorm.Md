# Brainstorm

C'est dans ce document qu'on pourrait tous mettre nos idées de trucs sur lesquels travailler en fin de semaine. Évidemment, ça serait bien de rester dans la thématique du tennis. J'ai essayé de mettre quelques titres évocateurs afin de classer un peu les idées, mais vous pouvez les modifier/en ajouter.

## Visualisation

- On s'en était déjà parlé initiallement, mais ca serait bien d'avoir un script/fonction/méthode qui permettrait de ploter le graph d'un tournoi. On pourrait s'en servir autant pour voir des tournois dans le passé que pour ploter les résultats de nos futurs modèles prédictifs de matchs. Ça nous permettrait de visualiser rapidement l'outcome d'un tournoi donné.

## Modeling

Clairement, il nous faut identifier des variables potentielles pour nourrir nos modèles. D'après mon guy Sipko (2015), les variables suivantes pourraient être intéressante: (c'est une liste longue, un "\" sépare deux variables...)

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

On peut probablement en trouver d'autres plus complexes comme les dernières. Certaines que j'ai nommées n'ont pas été utilisées finalement (comme ATP rank, qui ne représente pas vraiment la "forme" d'un certain joueur à un temps précis).

### Unsupervised

- Ça pourrait être nice de faire du clustering sur les joueurs en utilisant différentes stats de ceux-ci. On pourrait voir par la suite qu'est qui serait possible de faire avec ça, mais peut-être que si on est capable de dégager des constats intéressants quant au clustering de différents types de joueurs, cela pourrait éventuellement servir de input à une méthode supervisée. Par exemple, si deux joueurs n'ont pas joué beaucoup de matchs l'un contre l'autre, on pourrait utiliser les matchs d'un joueur contre le même style de joueur. 

### Supervised





