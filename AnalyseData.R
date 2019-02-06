library(readxl)
dataset<-read_excel("C:/Users/RIO/Documents/ENSAE/2A/Stats App/Data/ENSAE-Data.xlsx")

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

# Rendements moyens (base hebdomadaire puis annuelle)

avg_return_wkly <- function(){
  sum_return = 0
  for (i in seq(2,n)){
    v_cur = data[i,]
    v_past = data[i-1,]
    sum_return = sum_return + log(v_cur/v_past)
  }
  return(sum_return/(n-1))
}

avg_return_yrly <- function(){
  sum_return = 0
  for (i in seq(53,n)){
    v_cur = data[i,]
    v_past = data[i-52,]
    sum_return = sum_return + log(v_cur/v_past)
  }
  return(sum_return/(n-1))
}

avg_return_wkly = avg_return_wkly()
avg_return_yrly = avg_return_yrly()

# Volatilité

volatility_wkly <- function(){
  sum = 0
  for (i in 2:n){
    sum = sum + (log(data[i,]/data[i-1,]) - avg_return_wkly)**2
  }
  return (sqrt(sum/(n-1)))
}

volatility_yrly <- function(){
  sum = 0
  for (i in 53:n){
    sum = sum + (log(data[i,]/data[i-52,]) - avg_return_yrly)**2
  }
  return (sqrt(sum/(n-1)))
}

volatility_wkly = volatility_wkly()
volatility_yrly = volatility_yrly()

# Skewness

skewness_wkly <- function(){
  sum = 0
  for (i in 2:n){
    sum = sum + ((log(data[i,]/data[i-1,]) - avg_return_wkly)/volatility_wkly)**3
  }
  return (sum/(n-1))
}

skewness_yrly <- function(){
  sum = 0
  for (i in 53:n){
    sum = sum + ((log(data[i,]/data[i-52,]) - avg_return_yrly)/volatility_yrly)**3
  }
  return (sum/(n-1))
}

# Kurtosis

kurtosis_wkly <- function(){
  sum = 0
  for (i in 2:n){
    sum = sum + ((log(data[i,]/data[i-1,]) - avg_return_wkly)/volatility_wkly)**4
  }
  return (sum/(n-1))
}

kurtosis_yrly <- function(){
  sum = 0
  for (i in 53:n){
    sum = sum + ((log(data[i,]/data[i-52,]) - avg_return_yrly)/volatility_yrly)**4
  }
  return (sum/(n-1))
}

# Obtention de la matrice de corrélation

data_centered = scale(data) #Données centrées réduites
var_matrix = (1/(n-1))*(t(data_centered)%*%data_centered) #Estimateur sans biais de la matrice de corrélation

# ACP 

library(Factoshiny)

data.pca <- PCA(data, graph = FALSE)

# Visualisation du pouvoir explicatif des composantes principales
eig.val <- data.pca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances expliquées par les CPs (%)",
        xlab = "Composantes Principales",
        ylab = "Variance Expliquée (%)",
        col ="steelblue")
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")
# Les deux premières composantes principales expliquent environ 85 % de la variance

# Cercle des corrélations
plot(data.pca, choix = "var", autoLab = "yes") 

# On peut distinguer tout un groupe de devises très corrélées à la première composante principale (et toute bien représentées
# par elle étant données les longueurs des flèches : KZT (Kazakhstan Tenge), RUB (Russian Ruble), KGS (Kyrgyzstan Som) , 
# ZAR (South African Rand), IDR (Indonesian Rupiah), INR (Indian Rupee), TND (Tunisian Dinar), MMDK (Myanmar Kyat), 
# MNT (Mongolian Togrog), MXN (Mexican Peso), HNL (Honduras Lempira), TJS (Tadjikistan Somoni)

# Un deuxième groupe semble être plus corrélé à la deuxième composantes principale (mais réserve sur les devises GEL et CNY (également assez
# fortement corrélées à la deuxième CP, respectivement positivement et négativement) et JOD (mal représentée par la CP)) : 
# CNY (China Renminbi), THB (Thai Baht), PEN (Peruvian Sol), XOF (Franc CFA (Attention, pas Spot mais Bceao)), 
# JOD (Jordanian Dinar), GEL (Georgia Lari), COP (Colombian Peso)

# Visualisation des composantes principales

data.var <- data.pca$var
data.var$coord

data.ind <- data.pca$ind
data.ind$coord

curr_CP1 <- data.ind$coord[,1]
curr_CP2 <- data.ind$coord[,2]

CP <- data.frame(dataset$Date, curr_CP1, curr_CP2)
names(CP) <- c("date","CP1","CP2")

plot(CP$date,CP$CP1,type="l", ylab="Composante Principale 1", xlab="Date") # Première composante principale
plot(CP$date,CP$CP2,type="l", ylab="Composante Principale 2", xlab="Date") # Deuxième composante principale

# Affichage de l'évolution de quelques devises

plot(dataset$Date,dataset$TNDUSD,type="l", ylab="Tunisian Dinar", xlab="Date")
plot(dataset$Date,dataset$PENUSD,type="l", ylab="Peruvian Sol", xlab="Date")
# Observer la similarité respectivement à la première et la seconde composante principale

# Corrélation des devises à chacune des deux premières composantes principales

cor_CP <- data.var$cor[,c(1,2)]
# Un tableau affichant pour chaque devise la composante à la laquelle elel est le plus corrélée et le coefficient de corrélation pourrait
# être intéressant

