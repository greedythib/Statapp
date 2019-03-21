##### Traitement des bases de données FX #####

library(readxl)

### Importation des bases ###

dataset<-read_excel("C:/Users/RIO/Documents/ENSAE/2A/Stats App/Data/ENSAE-Data.xlsx")
curr_dvp<-read_excel("C:/Users/RIO/Documents/ENSAE/2A/Stats App/Data/Developed Currencies.xlsx")
curr_em<-read_excel("C:/Users/RIO/Documents/ENSAE/2A/Stats App/Data/Emerging Currencies.xlsx")
explaining_var<-read_excel("C:/Users/RIO/Documents/ENSAE/2A/Stats App/Data/explaining-variables-ENSAE.xlsx", sheet=2)

## Traitement de la base dataset ##

dataset <- dataset[c(-1,-2),]
names(dataset) <- c("Date", "IDRUSD", "Date1", "TJSUSD", "Date2", 
                    "KGSUSD", "Date3", "TNDUSD", "Date4", "PENUSD", "Date5", 
                    "KZTUSD", "Date6", "INDRUSD", "INDRUSD_2", "INDRUSD_3", "Date7", 
                    "MMDKUSD", "MMDKUSD_2", "MMDKUSD_3", "Date8", "RUBUSD", "RUBUSD_2", 
                    "RUBUSD_3", "Date9", "GELUSD", "Date10", "HNLUSD", 
                    "Date11", "CNYUSD", "Date12", "MNTUSD", "Date13", 
                    "THBUSD", "Date14", "ZARUSD", "Date15", "JODUSD", 
                    "Date17", "TNDUSD_2", "Date18", "MXNUSD", "Date19", 
                    "XOFUSD", "Date20", "COPUSD")
dataset <- dataset[,c(-3,-5,-7,-9,-11,-13,-15,-16,-17,-19,-20,-21,-23,-24,-25,-27,-29,-31,-33,-35,-37,-39,-40,-41,-43,-45)]
dataset$Date <-as.numeric(as.character(dataset$Date))
dataset$Date <- as.Date(dataset$Date, origin = "1899-12-30")
dataset <- dataset[seq(1,942),]

# noms des devises du dataset
currency <- names(dataset[c(-1)])

# on passe toutes les variables au format numeric
for (k in currency){
  dataset[[k]] <- as.numeric(dataset[[k]])
}

n=nrow(dataset)
data <- dataset[,-1] #dataset sans la colonne "date" pour les calculs

## Traitement de la base curr_dvp ##

names(curr_dvp)[1] <- "Date"

### Matrice des log-rendements ###

# Hebdomadaires

return_wkly = dataset

for (i in seq(2,n)){
  return_wkly[i,c(2:ncol(dataset))] = log(dataset[i,c(2:ncol(dataset))] / dataset[i-1,c(2:ncol(dataset))])
}

return_wkly = return_wkly[-1,]

# Annuels

return_yrly = dataset

for (i in seq(53,n)){
  return_yrly[i,c(2:ncol(dataset))] = log(dataset[i,c(2:ncol(dataset))] / dataset[i-52,c(2:ncol(dataset))])
}

return_yrly = return_yrly[-c(1:52),]

return_wkly = return_matrix(dataset,1)
return_yrly = return_matrix(dataset,52)
dvp_return_wkly = return_matrix(curr_dvp,1)
dvp_return_yrly = return_matrix(curr_dvp,52)
em_return_wkly = return_matrix(curr_em,1)
em_return_yrly = return_matrix(curr_em,52)

### Matrice des corrélations ###

cor_matrix = (1/(n-1))*(t(scale(data))%*%scale(data)) #Estimateur sans biais de la matrice de variance covariance
rw_cor_matrix = (1/(nrow(return_wkly)-1))*(t(scale(return_wkly[,-1]))%*%scale(return_wkly[,-1]))
ry_cor_matrix = (1/(nrow(return_yrly)-1))*(t(scale(return_yrly[,-1]))%*%scale(return_yrly[,-1]))

##### Analyses en Composantes Principales #####

### Analyse en prix ###

library(Factoshiny)

data.pca <- PCA(data, graph = FALSE)

# Résumé des valeurs propres et de leur décroissance

eig.val <- data.pca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances expliquées par les CPs (%)",
        xlab = "Composantes Principales",
        ylab = "Variance Expliquée (%)",
        col ="steelblue")
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

# Réprésentation du cercle des corrélations

plot(data.pca, choix = "var", autoLab = "yes")
#85% de la variance expliquée par les deux premières composantes

# Réprésentation des omposantes principales

data.var <- data.pca$var
data.ind <- data.pca$ind

curr_CP1 <- data.ind$coord[,1]
curr_CP2 <- data.ind$coord[,2]

CP <- data.frame(dataset$Date, curr_CP1, curr_CP2)
names(CP) <- c("date","CP1","CP2")

plot(CP$date,CP$CP1,type="l", ylab="Composante Principale 1", xlab="Date")
plot(CP$date,CP$CP2,type="l", ylab="Composante Principale 2", xlab="Date")

# Représentation d'une CP et d'une currency corrélée sur un même graph

# CP1 et Roupie Indienne

par(mar=c(5,4,4,5))
plot(CP$date,CP$CP1,type="l",col="red" , xlab="Date", ylab = "Composante Principale 1")
par(new=TRUE)
plot(CP$date,data$INRUSD,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Roupie Indienne",side=4,line=3)
legend("topright",col=c("red","blue"),lty=1,legend=c("CP1","INRUSD"))
title(main="Première CP vs. Roupie Indienne")

# CP2 et Sol Péruvien

par(mar=c(5,4,4,5))
plot(CP$date,CP$CP2,type="l",col="red" , xlab="Date", ylab = "Composante Principale 2")
par(new=TRUE)
plot(CP$date,data$PENUSD,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Sol Péruvien",side=4,line=3)
legend("topright",col=c("red","blue"),lty=1,legend=c("CP2","PENUSD"))
title(main="Deuxième CP vs Sol Péruvien")

### Analyse en log-rendements hebdomadaires ###

rw.pca <- PCA(return_wkly[,-1], graph = FALSE)

plot(rw.pca, choix = "var", autoLab = "yes")

rw.ind <- rw.pca$ind

rw_CP1 <- rw.ind$coord[,1]
rw_CP2 <- rw.ind$coord[,2]

rw_CP <- data.frame(return_wkly$Date, rw_CP1, rw_CP2)
names(rw_CP) <- c("date","CP1","CP2")

plot(rw_CP$date,rw_CP$CP1,type="l", ylab="Composante Principale 1", xlab="Date")
plot(rw_CP$date,rw_CP$CP2,type="l", ylab="Composante Principale 2", xlab="Date")

### Analyse en log-rendements annuels ###

ry.pca <- PCA(return_yrly[,-1], graph = FALSE)

plot(ry.pca, choix = "var", autoLab = "yes")

ry.ind <- ry.pca$ind

ry_CP1 <- ry.ind$coord[,1]
ry_CP2 <- ry.ind$coord[,2]

ry_CP <- data.frame(return_yrly$Date, ry_CP1, ry_CP2)
names(ry_CP) <- c("date","CP1","CP2")

plot(ry_CP$date,ry_CP$CP1,type="l", ylab="Composante Principale 1", xlab="Date")
plot(ry_CP$date,ry_CP$CP2,type="l", ylab="Composante Principale 2", xlab="Date")

######################################################################################################################

### Fonctions ACP

fct_PCA <- function(data){ ### Prend en argument la BDD avec la colonne Date
  
  pca <- PCA(data[-1], graph = FALSE) #ACP
  
  # Cercle des corrélations
  plot(pca, choix = "var", autoLab = "yes")
  
  #Extraction des CP
  
  pca.ind <- pca$ind
  
  pca_CP1 <- pca.ind$coord[,1]
  pca_CP2 <- pca.ind$coord[,2]
  
  pca_CP <- data.frame(data$Date, pca_CP1, pca_CP2)
  names(pca_CP) <- c("date","CP1","CP2")
  
  # Affichage des deux premières composantes principales
  plot(pca_CP$date,pca_CP$CP1,type="l", ylab="Composante Principale 1", xlab="Date")
  plot(pca_CP$date,pca_CP$CP2,type="l", ylab="Composante Principale 2", xlab="Date")
}

### Fonctions log-rendements ###

return_matrix <- function(data, weeks){
  
  return = data
  
  for (i in seq(weeks+1,n)){
    return[i,c(2:ncol(data))] = log(data[i,c(2:ncol(data))] / data[i-weeks,c(2:ncol(data))])
  }
  
  return = return[-1,]
}

######################################################################################################################