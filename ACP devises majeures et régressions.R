library(readxl)
fxdata=read_excel("C:/Users/baeli/Documents/explaining variables ENSAE.xlsx",sheet=1)
names(fxdata)=c("date","SEK","CHF","EUR","AUD","CAD","JPY","GBP","NZD")
library("FactoMineR")
library("factoextra")

#ACP
res.pca.fxdata <- PCA(fxdata[-1], scale.unit = TRUE ,graph = FALSE)

#On regarde les valeurs propres pour choisir les composantes principales

eig.val.fxdata <- get_eigenvalue(res.pca.fxdata)
fviz_eig(res.pca.fxdata, addlabels = TRUE, ylim = c(0, 80))

#Cercle des corrélations
#Rappel : la projection de la flèche sur F1 correspond au coefficient de corrélation entre v et F1 
fviz_pca_var(res.pca.fxdata, col.var = "blue")

#On distingue un groupe de devises très corrélées positivement à la première CP (Euro, Couronne suédoise, 
#Dollar canadien, Dollars australien et néo-zélandais). La Livre Sterling, très corrélée à la 2e CP
#suit une tendance différente. 

#On stocke les valeurs prises par la premère composante principale
pc1=get_pca_ind(res.pca.fxdata)$coord[,c(1)]

plot(fxdata[["date"]],fxdata[["CAD"]],type='l',ylab="Dollar canadien",xlab="Date")
plot(fxdata[["date"]],pc1,type='l',ylab="Composante principale 1",xlab="Date")

###Régressions linéaires avec les explaining variables

singlereg=function(){
  #la fonction fournit une liste avec le R² associé à chaque variable explicative quand on régresse la CP1 dessus
  expdata=read_excel("C:/Users/baeli/Documents/explaining variables ENSAE.xlsx",sheet=2)
  n=ncol(expdata[-1]) 
  l=list()
  for (k in seq(1,n)){
    regresseur=expdata[-1][[k]]
    model=lm(pc1~regresseur)
    l[[k]]=summary(model)$r.squared
  }
  names(l)=names(expdata[-1])
  l
}


