### Statapp
## Code R du sujet de Statapp sur la modélisation factorielle du risque de devise 

### commencer rapport pour la semaine du 20 Mars 

### Bilan réunion du 07/03/2019

- ACP en prix
- ACP en rendements 
- Comment l'ACP est utilisée dans la gestion d'un portefeuille 
- Couverture de portefeuille

-	Utilisation des résultats de l’ACP :
o	Identifier des devises dont l’évolution va « à contre-courant » (en particulier, celles peu corrélées à la première composante principale) et sur lesquelles il peut donc être intéressant d’investir
o	Utiliser les composantes principales pour construire un portefeuille de couverture, pondéré selon la variance expliquée (en pourcentage). Pour chaque composante, trouver un bon proxy (une devise très corrélée à cette composante ou une variable explicative adéquate), qu’on utilisera pour couvrir la tendance représentée par cette composante principale. C’est là que les régressions serviront.
-	L’étude des devises en prix permet de prendre en compte la tendance de long terme (tendance baissière en général pour les économies émergentes), ce qui est intéressant pour Symbiotics au vu de leurs investissements (horizon de temps assez long)
-	Normalement, on n’étudie pas les actifs en prix car les moyennes ne sont pas stables (problème de stationnarité). Est-ce qu'on s'affranchit de ce problème en ACP ? Problème dont il faudrait discuter avec C.Francq (notre référent)
-	Chercher du côté des tests d’intégration/co-intégration (problème de séries temporelles à discuter également avec Francq)
-	On peut essayer de mettre en application nos résultats sur un portefeuille (réel donné par Belpaire ou qu’on compose nous-même ?)

En résumé, les prochaines choses à faire sont :


-	Régression sur les sets de variable explicatives (quelles variables expliquent pca1 par exemple)
- Justifier que les rendements sont une série stationnaire
- Tester la stationnarité des devises en prix 
- Gerer le problème de stationnarité 
-	Application sur un portefeuille
- Commencer le rapport

