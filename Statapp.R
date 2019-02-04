install.packages("readxl")
library(readxl)
dataset<-read_excel("C:/Users/baeli/Documents/R/ENSAE-Data (1).xlsx")

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

# VolatilitY

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

# KHI 2

for(k in currency){
  v <- seq(1,n -1)
  d <- data[[k]]
  for (i in seq(1,n-1)){
    v_cur = d[i + 1]
    v_past = d[i]
    v[i] <- log(v_cur/v_past)
  }
  m <-mean(v)
  sd <- sd(v)
  
  # Comparaison fonction de répartition
  plot(ecdf(v), main = k)
  curve(pnorm(x, m, sd), col = "blue", add = TRUE)  
  
  # Comparaison de densités
  plot(density(v), main = k, ylim = c(0,45))
  curve(dnorm(x, m, sd), col="blue", add = TRUE)
}

#JARQUE BERA
jb_test=function(curr){
  vect_rend <- seq(1,n-1)
  d <- data[[curr]]
  for (i in seq(1,n-1)){
    current = d[i + 1]
    past = d[i]
    vect_rend[i] <- log(current/past)
  }
  m <-mean(vect_rend)
  N=length(vect_rend)
  num_b1=0
  num_b2=0
  den_b1=0
  for (k in seq(1,N)){
    num_b1=num_b1+(vect_rend[i]-m)**3
    num_b2=num_b2+(vect_rend[i]-m)**4
    den_b1=den_b1+(vect_rend[i]-m)**2
  }
  num_b1=num_b1/N
  num_b2=num_b2/N
  den_b1=(den_b1/N)**(3/2)
  den_b2=den_b1**(4/3)
  b1=num_b1/den_b1
  b2=num_b2/den_b2
  jb=N*((b1**2)/6+((b2-3)**2)/24)
  resultat=c(N,num_b1,den_b1,b1,b2,jb)
  return(resultat)
}

#VaR historique
var_hist=function(curr,conf){
  vect_rend <- seq(1,n-1)
  d <- data[[curr]]
  for (i in seq(1,n-1)){
    current = d[i + 1]
    past = d[i]
    vect_rend[i] <- log(current/past)
  }
  vect_rend=sort(vect_rend) #on trie les rendements journaliers par ordre croissant
  indice=n-conf*(n-1) #la var est la valeur "juste au dessus" de la queue de distribution à conf%
  return(vect_rend[indice])
}