##### Packages R pour les graphiques du support ####



mypackages<-c("tidyverse","corrplot","xts","zoo","sp","CASdatasets",
              "MASS","lattice","ggplot2","caret","FactoMineR","factoextra",
              "gplots","openxlsx","actuar","lubridate","ChainLadder","readxl",
              "questionr","labelled","gganimate","insuranceData","lifecontingencies",
              "LifeTables","lme4","olsrr","pROC","rmarkdown","gam","e1071","ade4","ggpubr")

check_fct<-function(pkg){
  if(!require(pkg,character.only = TRUE)){
    install.packages(pkg,dependencies = TRUE)
    library(pkg,character.only = TRUE)
  }
}

check_mypackages<-lapply(mypackages,check_fct)

#### Base des données####
data(freMPL4)
data(freMPL2)


freMPL4 <- freMPL4[,-12]
str(freMPL4)

data1 <- rbind(freMPL4, freMPL2)

#### Structure et visualisation de la base des données ####

str(data1)

head(data1, 10)

View(data1)


#### Statistiques descriptives pour les  Variables quantitatives ####



Var_Quant <- as.data.frame(data1[,+c(1,2,10,12,19,20)])


#Representation des correlations : plus l'ellipse ressemble a un cercle et moins
#les variables sont correlees. Plus l'ellipse ressemble a une droite et plus les 
#variables sont correlees.

## Corrplot pour les variables quantitatives

corrplot(cor(Var_Quant),method = "ellipse")



## Représentation graphique multivariée

plot(Var_Quant)

# Il y a une forte correlation entre DriveAge et LicAge. On retire la variable DriveAge
# pour la suite des analyses.



#### Analyse multivariée pour les variables quantitatives ACP ####


Data_ACP <- data1[,+c(1,2,10,12,19,20)]

summary(Data_ACP)

res.pca <- PCA(Data_ACP, scale.unit = TRUE, ncp=2)
summary(res.pca)

round(res.pca$eig,2)
fviz_eig(res.pca)


## Pour extraire les valeurs singulières:
res.pca$svd$V
## Pour extraire les informations sur les observations
res.pca$ind
## Pour extraire les informations sur les variables
res.pca$var

plot.PCA(res.pca, axes = c(1,2), choix = 'ind')


### Analyse multivariée pour les variables quanlitatives AFC ###


tableau <- housetasks[c(1,2,3,4,5,6),c(1,2)]
colnames(tableau) = c("ClaimInd=0","ClaimInd=1")
rownames(tableau) <- c("Male","Female","Alone","Other","HasKmLimit=0",
                       "HasKmLimit=1")


# table(data1$Gender, data1$ClaimInd)
# table(data1$MariStat, data1$ClaimInd)
# table(data1$HasKmLimit, data1$ClaimInd)

tableau[1,1] <- 52059
tableau[2,1] <- 30035
tableau[3,1] <- 22693
tableau[4,1] <- 59401
tableau[5,1] <- 71525
tableau[6,1] <- 10569

tableau[1,2] <- 1584
tableau[2,2] <- 912
tableau[3,2] <- 726
tableau[4,2] <- 1770
tableau[5,2] <- 2231
tableau[6,2] <- 265


View(tableau)

## Test statistique d'indépendance de khi-deux ##

chisq <- chisq.test (tableau)
chisq



# Graphique


balloon_melted<-melt(tableau)

balloon_melted <-as.data.frame(balloon_melted)

balloon_melted$Nom <- c("Male","Female","Alone","Other","HasKmLimit=0",
                        "HasKmLimit=1", "Male","Female","Alone","Other","HasKmLimit=0",
                        "HasKmLimit=1")
balloon_melted

p <- ggplot(balloon_melted, aes(x =variable, y = Nom)) 
p+geom_point( aes(size=value),shape=21, colour="black", fill="skyblue")+
  theme(panel.background=element_blank(), panel.border = element_rect(colour = "blue", fill=NA, size=1))+
  scale_size_area(max_size=20)+
  #Add labels to axes
  labs(x="Graphique du tableaux de contingence", y="")


tableau1 <- as.data.frame.matrix(tableau)

res.acb <- CA (tableau1, ncp =2,  graph = FALSE)
summary(res.acb)
res.acb$eig



########################  Modélisation GLM pour les fréquences (ClaimInd)    #################


levels(data1$SocioCateg) <- factor(c("CSP1","CSP1","CSP1","CSP1","CSP2",
                                     "CSP2","CSP2","CSP2","CSP2","CSP2",
                                     "CSP2", "CSP2", "CSP2","CSP3","CSP3",
                                     "CSP3","CSP3","CSP3","CSP3","CSP3",
                                     "CSP3","CSP3","CSP3","CSP4","CSP4","CSP4",
                                     "CSP4","CSP4","CSP4","CSP4","CSP4",
                                     "CSP5","CSP5","CSP5","CSP5","CSP5","CSP5",
                                     "CSP5","CSP6","CSP6","CSP6","CSP6","CSP6",
                                     "CSP6","CSP6","CSP7","CSP7","CSP7",
                                     "CSP7","CSP7","CSP9","CSP9"))
levels(data1$VehPrice) <- factor(c("A","A","A","D","E","F","G","H","I","J",
                                   "K","L","M","N","O","P","Q","Q","Q","Q",
                                   "Q","Q","Q","Q","Q","Q","Q"))
levels(data1$VehMaxSpeed) <- factor(c("1-140 km/h","1-140 km/h",
                                      "140-150 km/h","150-160 km/h",
                                      "160-170 km/h","170-180 km/h",
                                      "180-190 km/h","190-200 km/h",
                                      "200-220 km/h","220+ km/h "))

data1 <- data1[(data1$VehEnergy != "eletric") &
                 (data1$VehEnergy != "GPL"),]
data1$VehEnergy <- droplevels(data1$VehEnergy)
data1 <- data1[(data1$VehEnergy != "eletric") &
                 (data1$VehEnergy != "GPL"),]
data1$VehEnergy <- droplevels(data1$VehEnergy)
data1$VehEngine <- droplevels(data1$VehEngine)
data1$LicAge <- floor(data1$LicAge/12)
data1$LicAge3 <- (data1$LicAge-22)^2

n <- NROW(data1) ; p <- round(0.8*n)
index.app <- sample(1:n, p)
data1.app <- data1[index.app, ]
data1.test <- data1[-index.app, ]


data1.app.reg <- model.matrix(ClaimInd ~ LicAge+LicAge3 + VehAge + 
                                Gender + MariStat + SocioCateg + VehUsage + 
                                DrivAge + HasKmLimit  + 
                                BonusMalus + VehBody + VehPrice + VehEngine +
                                VehEnergy + VehMaxSpeed + VehClass + 
                                RiskVar + Garage, data=data1.app)

data1.test.reg <- model.matrix(ClaimInd ~ LicAge+LicAge3 + VehAge + 
                                 Gender + MariStat + SocioCateg + VehUsage +
                                 DrivAge + HasKmLimit  + 
                                 BonusMalus + VehBody + VehPrice + 
                                 VehEngine + VehEnergy + VehMaxSpeed + 
                                 VehClass + RiskVar + Garage,
                               data=data1.test)

colnames(data1.app.reg)[ colnames(data1.app.reg)
                         =="VehAge10+"] <- "VehAge10"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehAge6-7"] <- "VehAge6"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehAge8-9"] <- "VehAge8"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(data1.app.reg)[ colnames(data1.app.reg) 
                         =="GaragePrivate garage"] <- "GaragePrivate_garage"


colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehAge10+"] <- "VehAge10"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehAge6-7"] <- "VehAge6"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehAge8-9"] <- "VehAge8"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehUsageProfessional run"] <- "VehUsageProfessional_run"

colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(data1.test.reg)[ colnames(data1.test.reg) 
                          =="GaragePrivate garage"] <- "GaragePrivate_garage"


## Ajustement du modele GLM Binomiale avec fonction de lien canonique ##

GLM.claimInd <- glm(ClaimInd ~ LicAge3 + VehAge1 + VehAge10 + VehAge2 + VehAge3 +
                      VehAge4 + VehAge5 + VehAge6 + VehAge8 + GenderMale + 
                      MariStatOther + SocioCategCSP2 + SocioCategCSP3 + 
                      SocioCategCSP4 + SocioCategCSP5 + SocioCategCSP6 + 
                      SocioCategCSP7 + SocioCategCSP9 + 
                      VehUsagePrivate_trip_to_office + VehUsageProfessional + 
                      VehUsageProfessional_run + DrivAge + HasKmLimit + 
                      BonusMalus +
                      VehBodycabriolet + VehBodycoupe + VehBodymicrovan + 
                      VehBodyother_microvan + VehBodysedan + 
                      VehBodysport_utility_vehicle + VehBodystation_wagon +
                      VehBodyvan + VehPriceD + VehPriceE + VehPriceF + VehPriceG +
                      VehPriceH + VehPriceI + VehPriceJ + VehPriceK + VehPriceL + 
                      VehPriceM + VehPriceN + VehPriceO + VehPriceP + VehPriceQ +
                      VehEnginedirect_injection_overpowered + VehEngineinjection +
                      VehEngineinjection_overpowered + VehEnergyregular + 
                      VehMaxSpeed140_150_km_h + VehMaxSpeed150_160_km_h + 
                      VehMaxSpeed160_170_km_h + VehMaxSpeed170_180_km_h + 
                      VehMaxSpeed180_190_km_h + VehMaxSpeed190_200_km_h + 
                      VehMaxSpeed200_220_km_h + VehMaxSpeed220_km_h  + VehClassA +
                      VehClassB + VehClassH + VehClassM1 + VehClassM2 + RiskVar +
                      GarageNone + GaragePrivate_garage,
                    family=binomial(link = 'logit'),
                    data=cbind.data.frame(ClaimInd=data1.app$ClaimInd, 
                                          data1.app.reg))

#Selection des variables par la méthode (Forward, Backward, Stepwise) et AIC le plus petit

GLM.claimInd <- stepAIC(GLM.claimInd, trace=TRUE, direction=c("both"))
AIC(GLM.claimInd)

#GLM.claimInd <- stepAIC(GLM.claimInd, trace=TRUE, direction=c("backward"))
#AIC(GLM.claimInd)

#GLM.claimInd <- stepAIC(GLM.claimInd, trace=TRUE, direction=c("forward"))
#AIC(GLM.claimInd)

#le GLM ClaimInd selectionne 

## Modèle final ##

GLM.ClaimInd <- glm(ClaimInd ~ VehAge10 + VehAge6 + VehAge8 + SocioCategCSP2 + SocioCategCSP7 + 
                      VehUsagePrivate_trip_to_office + VehUsageProfessional + VehUsageProfessional_run + 
                      DrivAge + HasKmLimit + BonusMalus + VehBodystation_wagon + 
                      VehPriceL + VehEngineinjection + VehMaxSpeed140_150_km_h + 
                      VehMaxSpeed150_160_km_h + VehMaxSpeed160_170_km_h + VehMaxSpeed170_180_km_h + 
                      VehMaxSpeed220_km_h + VehClassB + VehClassM1 + VehClassM2 + 
                      RiskVar + GarageNone, 
                    family = binomial(link = 'logit'), 
                    data = cbind.data.frame(ClaimInd = data1.app$ClaimInd,
                                            data1.app.reg))



summary(GLM.claimInd)

Anova(GLM.claimInd, type = c(3))

## Probabilité d'avoir au moins un sinistre ClaimInd=1 ##


p1 <- length(data1[data1$ClaimInd==1,]$ClaimInd)
p2 <- length(data1$ClaimInd)

proba_ind_1 <- p1 / p2

## Prediction du modele ##

predict.GLM.claimInd.test <- predict(GLM.claimInd, newdata 
                                     = as.data.frame(data1.test.reg), type = "response")

predict.GLM.claimInd.test.bis=as.factor(ifelse(predict.GLM.claimInd.test
                                               > proba_ind_1,1,0))



## Matrice de confusion sur l'echantillon test ##

confusionMatrix(data = predict.GLM.claimInd.test.bis, reference 
                =as.factor(data1.test$ClaimInd))

pred <- prediction(predict.GLM.claimInd.test, predict.GLM.claimInd.test.bis)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)

## L'air sous la courbe de ROC ##

pROC::auc(pROC:: roc(response=data1.test$ClaimInd , predictor=predict.GLM.claimInd.test))




## Erreur sur l'echantillon test ##

mean((predict.GLM.claimInd.test - data1.test[,"ClaimInd"])**2)



# L'erreur est petit et Accuracy est acceptable,  donc le modèle est accepté # 


########################   Modélisation GLM pour les couts (ClaimAmount)    #################

data1.app.sinistre <- subset(data1.app, ClaimAmount > 0)
data1.test.sinistre <- subset(data1.test, ClaimAmount > 0)

data1.app.sinistre.reg <- model.matrix(ClaimAmount ~ LicAge + VehAge + 
                                         Gender + MariStat + SocioCateg + 
                                         VehUsage + DrivAge + HasKmLimit + 
                                         BonusMalus + VehBody + 
                                         VehPrice + VehEngine + VehEnergy + 
                                         VehMaxSpeed + VehClass + RiskVar + 
                                         Garage, data=data1.app.sinistre)

data1.test.sinistre.reg <- model.matrix(ClaimAmount ~ LicAge + VehAge + 
                                          Gender + MariStat + SocioCateg +
                                          VehUsage + DrivAge + HasKmLimit +
                                          BonusMalus + VehBody + 
                                          VehPrice + VehEngine + VehEnergy +
                                          VehMaxSpeed + VehClass + RiskVar +
                                          Garage, data=data1.test.sinistre)

colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehAge10+"] <- "VehAge10"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehAge6-7"] <- "VehAge6"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehAge8-9"] <- "VehAge8"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(data1.app.sinistre.reg)[ colnames(data1.app.sinistre.reg) 
                                  =="GaragePrivate garage"] <- "GaragePrivate_garage"

colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehAge10+"] <- "VehAge10"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehAge6-7"] <- "VehAge6"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehAge8-9"] <- "VehAge8"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(data1.test.sinistre.reg)[ colnames(data1.test.sinistre.reg) 
                                   =="GaragePrivate garage"] <- "GaragePrivate_garage"

## Ajustement du modele GLM Gamma avec fonction de lien logarithmique ##

GLM.claimAmount <- glm(ClaimAmount ~ DrivAge + BonusMalus + 
                         HasKmLimit + RiskVar+VehAge1 + VehAge10 + VehAge2 +
                         VehAge3 + VehAge4 + VehAge5 + VehAge6 + VehAge8 + 
                         VehPriceD +
                         VehPriceE + VehPriceF + VehPriceG + VehPriceH +
                         VehPriceI + VehPriceJ + VehPriceK + VehPriceL + 
                         VehPriceM + VehPriceN + VehPriceO + VehPriceP +
                         VehPriceQ + + VehEnginedirect_injection_overpowered + 
                         VehEngineinjection + VehEngineinjection_overpowered +
                         VehEnergyregular + VehMaxSpeed140_150_km_h + 
                         VehMaxSpeed150_160_km_h + VehMaxSpeed160_170_km_h +
                         VehMaxSpeed170_180_km_h + VehMaxSpeed180_190_km_h + 
                         VehMaxSpeed190_200_km_h + VehMaxSpeed200_220_km_h +
                         VehMaxSpeed220_km_h + VehClassA + VehClassB +
                         VehClassH + VehClassM1 + VehClassM2+ GarageNone + 
                         GaragePrivate_garage + VehUsagePrivate_trip_to_office +
                         VehUsageProfessional_run + VehBodycabriolet + 
                         VehBodymicrovan + VehBodycoupe + SocioCategCSP2 +
                         SocioCategCSP4 + SocioCategCSP5 + SocioCategCSP7 +
                         SocioCategCSP9  , 
                       family=Gamma(link = 'log') ,
                       data= cbind.data.frame(ClaimAmount = data1.app.sinistre$ClaimAmount,
                                              data1.app.sinistre.reg) )


#Selection des variables par la méthode (Forward, Backward, Stepwise) et le plus petit  AIC

GLM.claimAmount <- stepAIC(GLM.claimAmount, trace=TRUE, direction=c("both"))
AIC(GLM.claimAmount)

#GLM.claimAmount <- stepAIC(GLM.claimAmount, trace=TRUE, direction=c("backward"))
#AIC(GLM.claimAmount)

#GLM.claimAmount <- stepAIC(GLM.claimAmount, trace=TRUE, direction=c("forward"))
#AIC(GLM.claimAmount)

## Modèle final ## 

GLM.claimAmount <- glm(ClaimAmount ~ DrivAge + HasKmLimit + RiskVar + VehAge1 + VehAge10 + 
                         VehAge2 + VehAge5 + VehPriceE + VehPriceI + VehPriceJ + VehPriceK + 
                         VehPriceM + VehPriceN + VehPriceO + VehPriceQ + VehEnginedirect_injection_overpowered + 
                         VehEngineinjection + VehEngineinjection_overpowered + VehEnergyregular + 
                         VehMaxSpeed150_160_km_h + VehMaxSpeed160_170_km_h + VehMaxSpeed180_190_km_h + 
                         VehClassB + VehClassH + VehClassM2 + VehUsagePrivate_trip_to_office + 
                         VehUsageProfessional_run + VehBodymicrovan + SocioCategCSP4 + 
                         SocioCategCSP5 + SocioCategCSP7, 
                       family = Gamma(link = "log"), 
                       data = cbind.data.frame(ClaimAmount = data1.app.sinistre$ClaimAmount,
                                               data1.app.sinistre.reg),
                       weights = VehBodymicrovan + VehBodycoupe + 
                         SocioCategCSP4 +
                         SocioCategCSP5 + SocioCategCSP7 +
                         SocioCategCSP9)



summary(GLM.claimAmount)


Anova(GLM.claimAmount, type = c(3))

## Prediction du modele ##


predict.GLM.claimAmount.test <- predict(GLM.claimAmount, newdata 
                                        = as.data.frame(data1.test.sinistre.reg), type = "response")


## Erreur de l'échantillon test ##

mean((predict.GLM.claimAmount.test - data1.test[,"ClaimAmount"])**2)

## Graphe de prediction ##

data1.test.sinistre$prediction <- predict.GLM.claimAmount.test



ggplot(data1.test.sinistre, aes(x = prediction, y=ClaimAmount))+  
  geom_point(color = "darkblue", size = 3, alpha = 0.3) + geom_abline(color="red")


data1.test.sinistre <- subset(data1.test,ClaimAmount > 250 & ClaimAmount < 6000)

plot.new() 
par(mar=c(4,4,3,5),main = "") 
plot(data1.test.sinistre$ClaimAmount,col = "blue",,axes=F,xlab="",ylab="")
axis(2, col="blue",col.axis="blue") 
mtext("ClaimAmount",side=2,line=2.5,col="blue") 
par(new = T) 
plot(predict.GLM.claimAmount.test,col = "red",,axes=F,xlab="",ylab="")
axis( 4 ,col="red",col.axis="red") 
mtext("Prédiction",side=4,line=2.5,col="red") 
axis( 1 ,col="black",col.axis="black") 
mtext("",side=1,line=2.5,col="black")


hist(predict.GLM.claimAmount.test, main = "Prédiction du coût de sinistres", xlab = "Predict.claimAmount", ylab = "Fréquences")
densite <- density(predict.GLM.claimAmount.test)
plot(density(predict.GLM.claimAmount.test), xlab = "Predict.claimAmount", main = "Densité pour la prédiction\n du coût de sinistres", ylab = "Densité")


# Conclusion: Le modèle est acceptable, car il s'adapte bien aux données.




## Calcul de la  Prime Pure ## 


predict.GLM.claimAmount.test <- predict(GLM.claimAmount, newdata 
                                        = as.data.frame(data1.test.reg), type = "response")
PrimeP_Individus_GLM_test <- predict.GLM.claimAmount.test*predict.GLM.claimInd.test

summary(PrimeP_Individus_GLM_test)

(PrimeP_Unique_GLM_test <- mean(PrimeP_Individus_GLM_test))

boxplot(PrimeP_Individus_GLM_test, 
        main="Boxplot de la prédiction de la prime pure")

hist(PrimeP_Individus_GLM_test,breaks=100,col="red",density=5,xlab="Prime Pure",
     ylab="Fréquences",main="Prime pure calculée par les régressions\n(Logistique et Gamma)",ylim=c(0,5000),xlim=c(0,500),tck=0.01)

densite.predict.PrimePur.test <- density(PrimeP_Individus_GLM_test)
plot(densite.predict.PrimePur.test,xlim=c(0,400), main = "Densité de prédiction pour la prime pure", ylab="Densité")

## Transformation en facteurs pour faire des boxplots

data1.test$prime<-PrimeP_Individus_GLM_test
data1.test$VehAge<-as.factor(data1.test$VehAge)
data1.test$DrivAge<-as.factor(data1.test$DrivAge)
data1.test$LicAge<-as.factor(data1.test$LicAge)
data1.test$SocioCateg<-as.factor(data1.test$SocioCateg)



## Boxplots avec la prime pure 

sortie1 <- ggplot(data1.test, aes(x=SocioCateg, y=prime)) + 
  geom_boxplot()+
  labs(title = "Distribution de la prime en fonction du statut social",
       x = "Statut social",
       y = "Prime")
sortie2 <- ggplot(data1.test, aes(x=VehAge, y=prime)) + 
  geom_boxplot()+
  labs(title = "Distribution de la prime en fonction de l'âge du véhicule",
       x = "Âge du véhicule",
       y = "Prime")
sortie3 <- ggplot(data1.test, aes(x=DrivAge, y=prime)) + 
  geom_boxplot()+
  labs(title = "Distribution de la prime en fonction de l'âge de l'assuré",
       x = "Âge de l'assuré",
       y = "Prime")
sortie4 <- ggplot(data1.test, aes(x=LicAge, y=prime)) + 
  geom_boxplot()+
  labs(title = "Distribution de la prime en fonction du nombre d'années de permis",
       x = "Nombre d'années de permis",
       y = "Prime")

sortie1
sortie2
sortie3
sortie4

