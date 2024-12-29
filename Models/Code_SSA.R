 

# Library

library(fftw)
library(Rssa)
library(haven)
library(xts)
library(dplyr)


#Importation de donn??es


donnees <- read.table("C:/Users/kaffa/OneDrive/Bureau/Projet_pr??vision/data_source.csv", header = TRUE, sep = "\t")
donnees$Italy <- as.numeric(gsub(",", ".", donnees$Italy))

data_s <-select(donnees,c(Time,Italy))

data_ts <- ts(data_s[[2]],frequency=12,end=c(2022,12))


######################################################################
### Alternative pour dicomposer une serie temporelle par SSA #########
######################################################################


### Regroupement automatique en fixant le nombre de cluster : utilisation des w-correlations ###


ii <- length(data_ts)
L_Elsner <- floor(ii/4)

s.model_ssa <- ssa(data_ts, L=L_Elsner , kind ="1d-ssa", svd.method ="svd", neig=L_Elsner)

#Eigenvectors

plot(s.model_ssa, type = "vectors")

#Conclusion : 3 clusters


g.model_ssa <- grouping.auto(s.model_ssa , nclust=3 ,
                                grouping.method = "wcor" , method = "complete")


r.model_ssa <- reconstruct(s.model_ssa , groups=g.model_ssa)


trend<- r.model_ssa[[1]]
g.model_ssa[[1]]   # Composantes associies au trend
cycle<- r.model_ssa[[2]]
g.model_ssa[[2]]   # Composantes associies au cycle
noise<- r.model_ssa[[3]]
g.model_ssa[[3]]   # Composantes associies ` l'alia

trend_cycle <- trend + cycle

############################################################
### Reprisentation graphique des diffirentes composantes ###
############################################################

plot.zoo(cbind(data_ts , trend) , plot.type = "single", ylab = "CPI", xlab = "Time",
         col = c( "blue", "red"))
legend(x=1970,y=60,c("ratio","tendance"),col=c("blue", "red"), lty=1:2 , cex=0.85  , bty = "n")
title(main = "CPI  et sa tendance")

plot(x = cycle, ylab = "Cycle ", xlab = "Time", plot.type = "single", col = c( "blue")) 
title(main = "Composante cyclique du CPI")

plot(x = noise, ylab = "Bruit (%)", xlab = "Time", plot.type = "single", col = c( "blue")) 
title(main = "Composante bruit du CPI")

plot.zoo(cbind(data_ts , trend_cycle) , plot.type = "single", ylab = "CPI", xlab = "Time",
         col = c( "blue", "red") )
legend(x=1970,y=60,c("ratio","composante tendance-cycle"),col=c("blue", "red"), lty=1:2 , cex=0.85  , bty = "n")
title(main = "CPI composante tendance-cycle")



#########################
#Extraction des erreurs## 
#########################

erreur_prev <- data_ts - trend_cycle

plot(x = erreur_prev, ylab = "", xlab = "", plot.type = "single", col = c( "blue")) 

export <- cbind(data_ts,trend_cycle)

write.csv(export, file = "error.csv")




##################
### Pr??visions ###
##################

#### Privisions basies sur un regroupement automatique : cluster 1 & 2 ####
# Privision sur 12 p??riodes de TC (cluster + cluster 2) #

prev.model_ssa <- rforecast(s.model_ssa , groups=list(TC=c(g.model_ssa[[1]],g.model_ssa[[2]])), 
                               only.new = FALSE , len=12 )
prev.model_ssa


# Reprisentation graphique

plot(cbind(data_ts , prev.model_ssa), plot.type='single', col = c( "black", "red"))
prev.model_ssa




#### Intervalle de confiance pour la privision pricidente ###

ic_prev <- forecast(s.model_ssa, groups=list(TC=c(g.model_ssa[[1]],g.model_ssa[[2]])), 
                    method = "recurrent",len = 12, R=500,level=0.90 ,interval='prediction' ,  
                    only.new = FALSE, only.intervals = FALSE) 

ic_prev
# Reprisentation graphique

plot(cbind(data_ts , ic_prev$fitted, ic_prev$mean , ic_prev$lower ,ic_prev$upper), plot.type='single', 
     col = c( "black", "red","red","grey","grey"),lty=c(1,1,2,3,3))

legend(x=1970,y=60,c("CPI" , "Trend-cycle", "Pr??vision", "IC ` 90%"),col = c( "black", "red","red","grey","grey"),lty=c(1,1,2,3,3), bty = "n")


