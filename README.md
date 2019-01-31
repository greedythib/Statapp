# Statapp
## Code R du sujet de Statapp sur la modélisation factorielle du risque de devise 

### Prochaine réunion : vendredi 8 Février 

### 1) ELie et Robin 

test du chi2 : test de normalité

calcul de VaR  sous l’hypothèse de normalité : 
- avec moyenne
- sans moyenne

calcul de Var historique

pour le test du chi-2 : faire representation graphique comparee de la loi normale et de la distribution empirique

test de jarque-bera pour l’adequation (a regarder aussi)

## 

Note : moyenne des distributions est negative : en moyenne les devises se sont dépréciées 

en moyenne les dépréciations se sont compensées par le taux de portage

investissement dans deux devises différentes sont équivalente : tqux de portqge compense la perte, avec en p’us : compensation du risque lié a la prise de la devise 

papier prime de risque :
le coupon va compenser les mouvements de change

var sans moyenne ? : hyp que en moyenne, le taux de portage compense au moins la perte de change 

### 2) Thibaud

expected short fall : moyenne des pertes réalisées au dela de la var = densité de la distribution qui est au delà de 5% (si on considere une var a 5%) (voir lien sur le calcul de l’expected shortfall pour une loi normale) ou prendre l’ensemble des obs au dela de la var et faire la moyenne

turoriel pour le calcul : http://blog.smaga.ch/expected-shortfall-closed-form-for-normal-distribution/
 

### 3) Alexandre
 
 matrice de correlation et ACP. Regarder quelles sont les devises les plus correlees aux cp 

problème probable : devises tres peu correlees entre elles, donc pouvoir explicatif faible de la premiere cp

 essayer de remarquer deux clusters regionaux :
- russie bloc soviétique 
- euro

indice DXY (indice de devise : dollar weighted par devises en circulation) (lien Belpaire) => A inclure dans l’analyse pour tester la dependance au dollar des currencies 

indice BBDXY à tenter également

travailler sur le concept : regarder la var en incluant la moyenne ou pas. 
pb : on ne connait pas le cout de portage. ici on est sur des devises frontieres, donc pas d’historique de taux d’intérêt donc on se base seulement sur les mouvements de change 

#### envoyer notebook pour mercredi
