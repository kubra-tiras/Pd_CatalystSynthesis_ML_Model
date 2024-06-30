# Downloading necessary tools for n-fold Classification, Classification Tree, Boruta Importance Plots:

library(tidyverse)
library(rpart)
library(rpart.plot)
library(readxl)
library(caret)
library(plyr)
library(dplyr)
library(Boruta)
library(caTools)
library(randomForest)
library(party)
library(ggplot2)
library(readr)
library(Metrics)
library(writexl)
library(gam)
library(gganimate)
library(ggbeeswarm)


# Read the data from Excel:

dataPd = read_excel('C:/Users/kubra/Desktop/Supported Pd Catalysts Model Data.xlsx')

dataPd$given_dispersion <- na.omit(dataPd$given_dispersion)
columns <- c(27)
dataPd[, columns] <- lapply(columns, function(x) as.numeric(dataPd[[x]]))
glimpse(dataPd)


# Beeswarm plots in ggplot2, run the plots separately: 

# Metal Loading vs. Dispersion:

dataPd[,5] <-as.factor(ifelse(dataPd[,5]<0.5,"1",ifelse(dataPd[,5]<1,"2",ifelse(dataPd[,5]<5,"3",
                                  ifelse(dataPd[,5]<10,"4","5")))))


ggplot(dataPd, aes(x = MetalLoading, y = given_dispersion, color = MetalLoading)) +
  geom_beeswarm(cex = 0.5)+
  ggtitle("Metal Loading vs. Dispersion")+
  labs(x = "Pd Loading wt.%",y = "Dispersion %")+
  theme(axis.title=element_text(size=14,face="bold")) +
  theme(axis.text.x = element_text(angle=90, hjust=1,size = 14,face="bold"))+
  theme(axis.text.y = element_text(size = 14, face="bold"))


# Precursor vs. Dispersion:

  
  dataPd <-dataPd[dataPd$precursor=='Pd(NO3)2' | dataPd$precursor=='PdCl2'| 
                    dataPd$precursor=='Pd(acac)2'|dataPd$precursor=='Pd(OAc)2',]
  ggplot(dataPd, aes(x = precursor, y = given_dispersion, color = precursor)) +
  geom_beeswarm(cex = 0.5)+
  ggtitle("Precursor vs. Dispersion")+
  labs(x = "Precursor",y = "Dispersion %")+
    theme(axis.title=element_text(size=14,face="bold")) +
    theme(axis.text.x = element_text(angle=90, hjust=1,size = 14,face="bold"))+
    theme(axis.text.y = element_text(size = 14, face="bold"))
  

# Support vs. Dispersion:
  
  dataPd <-dataPd[dataPd$support=='mixed-Al2O3' | dataPd$support=='gamma-Al2O3'| 
                    dataPd$support=='C'| dataPd$support=='SiO2'|
                    dataPd$support=='CeO2'|  dataPd$support=='TiO2',]
  
  ggplot(dataPd, aes(x = support, y = given_dispersion, color=support)) +
  geom_beeswarm(cex = 0.5)+ ggtitle("Support vs. Dispersion")+
  labs(y = "Dispersion %")+
    theme(axis.title=element_text(size=18,face="bold")) +
    theme(axis.text.x = element_text(angle=90, hjust=1,size = 14,face="bold"))+
    theme(axis.text.y = element_text(size = 14, face="bold"))
  
  
# Solvent vs. Dispersion:
  
  dataPd <-dataPd[dataPd$solvent=='water' | dataPd$solvent=='aqueous hydrochloric acid'| 
                    dataPd$solvent=='toluene'| dataPd$solvent=='HCl'|
                    dataPd$support=='aqueous ethanol'|  dataPd$solvent=='acetone',]
  
  ggplot(dataPd, aes(x = solvent, y = given_dispersion, color=solvent)) +
  geom_beeswarm(cex = 0.5)+ ggtitle("Solvent vs. Dispersion")+
  labs(x = "Solvent",y = "Dispersion %")+
    theme(axis.title=element_text(size=14,face="bold")) +
    theme(axis.text.x = element_text(angle=90, hjust=1,size = 14,face="bold"))+
    theme(axis.text.y = element_text(size = 14, face="bold"))

# pH adjustment vs. Dispersion:
  
  ggplot(dataPd, aes(x = pHadjustment, y = given_dispersion, color=pHadjustment)) +
  geom_beeswarm(cex = 0.5)+ ggtitle("pH adjustment vs. Dispersion")+
  labs(x = "pH adjustment",y = "Dispersion %")+
    theme(axis.title=element_text(size=14,face="bold")) +
    theme(axis.text.x = element_text(angle=90, hjust=1,size = 14,face="bold"))+
    theme(axis.text.y = element_text(size = 14, face="bold"))

# Method vs. Dispersion:
  
  dataPd <-dataPd[dataPd$method=='Deposition-precipitation' | dataPd$method=='Wet Impregnation'| 
                    dataPd$method=='Polyol' | dataPd$method=='Sol-gel'| 
                    dataPd$method=='Incipient wetness impregnation'| dataPd$method=='Ion-exchange'|
                    dataPd$method=='Liquid-phase reduction'|  dataPd$method=='Photo-deposition',]
  ggplot(dataPd, aes(x = method, y = given_dispersion, color=method)) +
    geom_beeswarm(cex = 0.5)+ ggtitle("Synthesis Method vs. Dispersion") +
    labs(y = "Dispersion %") +
    theme(axis.title=element_text(size=14,face="bold")) +
    theme(axis.text.x = element_text(angle=90, hjust=1,size = 14,face="bold"))+
    theme(axis.text.y = element_text(size = 14, face="bold"))
  
  
  
