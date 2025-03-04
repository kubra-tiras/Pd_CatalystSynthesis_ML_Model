# Downloading necessary tools for n-fold Classification:

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
library(caret)

mode <- function(x) {   # Create mode function 
  unique_x <- unique(x)
  mode <- unique_x[which.max(tabulate(match(x, unique_x)))]
  mode
}

# Read the data from Excel:

dataPd = read_excel('C:/Users/kubra/Desktop/Supported Pd Catalysts Model Data.xlsx')

#Select columns for model (exclude Support Source, operation on support, normalized support area,difference pH,
#         Determination techniques, CO Chemisorption Constant, H2 Chemisorption Constant, standard deviations, dispersion,
#         estimation for dispersion, size estimation, size, year, country, reaction, title, link, author)

dataPd <- dataPd[,c(1,5,6,8,11,12,13,14,15,16,17,19,20,22,23,30)]

# Convert other columns into numeric:

columns <- c(2,3,5,6,9,11,12,13,14,15,16)
dataPd[, columns] <- lapply(columns, function(x) as.numeric(dataPd[[x]]))
glimpse(dataPd)

# Print data and convert into factors for Method, Precursor, 
#   Support, pH adjustment, Reduction Solvent/Gas: 

glimpse(dataPd)
names <- c(1,4,7,8,10)
dataPd[,names] <- lapply(dataPd[,names] , factor)


# TO DEAL WITH NA - IMPUTATION:

dataPd$precursor[is.na(dataPd$precursor)] <- mode(dataPd$precursor)
dataPd$solvent[is.na(dataPd$solvent)] <- mode(dataPd$solvent)
dataPd$SupportSA[is.na(dataPd$SupportSA)] <- mean(dataPd$SupportSA,na.rm = TRUE)
dataPd$SupportCalcTemp[is.na(dataPd$SupportCalcTemp)] <- 298
dataPd$SupportCalcTime[is.na(dataPd$SupportCalcTime)] <- 2
dataPd$solventpH[is.na(dataPd$solventpH)] <- mean(dataPd$solventpH,na.rm = TRUE)
dataPd$PZC[is.na(dataPd$PZC)] <- mean(dataPd$PZC,na.rm = TRUE)
dataPd$CalcTime[is.na(dataPd$CalcTime)] <- 2
dataPd$CalcTemp[is.na(dataPd$CalcTemp)] <- 298
dataPd$ReducTemp[is.na(dataPd$ReducTemp)] <- 298
dataPd$ReducTime[is.na(dataPd$ReducTime)] <- 2

hist(dataPd$dispersion, col='red')

dataPd[, 16] <- lapply(16, function(x) log10(dataPd[[x]]))
glimpse(dataPd)

hist(dataPd$dispersion, col='blue')

# To produce same sequence we are deciding a fixed number:
set.seed(123)

# Split data into two groups: 
# Test and Train Data Set with Split Ratio: 0.80:

index <- createDataPartition(dataPd$dispersion, p=.80, list=FALSE, times=1)
train <- dataPd[index,]
test <- dataPd[-index,]
dim(train)
dim(test)


# Cross Validation (Prepare matrix with number of folds):

k=5 # 5-fold cross validation
sample <- sample(nrow(train))
CrossVal <- train[sample,]    
foldcolumn <- cut(seq(1,nrow(CrossVal)), breaks=k,labels=FALSE)
NewData <- cbind(foldcolumn,CrossVal)
glimpse(NewData)


# Random Forest Model for Predictions:

t=0
error=numeric()
r_square=numeric()
rootmse=numeric()
prediction=numeric()
val.y=numeric()
i=numeric()
j=numeric()
n=numeric()

results=data.frame(error,rootmse,r_square,i,j,n)

# j for Number of Trees, i for Number of Folds 
# and n for Number of Node Size:

for (j in seq(from=100, to=500, by=10))
{
  for (n in seq(from=4, to=50, by=1))
  {
    
    for (i in 1:k)
    {
      
      train=NewData[which(!NewData[,1]==i),]
      train=train[,-1]
      value=NewData[which(NewData[,1]==i),]
      value=value[,-1]
      
      avg=mean(value[,ncol(value)])
      
      set.seed(123)
      
      model= randomForest(dispersion~ .,data=train, ntree=j,nodesize=5,mtry=n, importance =T, na.action=na.omit)
      
      prediction <- c(prediction,predict(model,value[,-ncol(value)]))
      
      val.y = c(val.y,value[,ncol(value)])
      
      par(new=TRUE)
      
      plot((value[,ncol(value)]), predict(model,value[,-ncol(value)]),
           main="Validation Data Results of Random Forest with 5-Folds Classification"
           ,xlab="log(Observed Data)",ylab="log(Predicted Data)",
           pch=19,cex.axis=1.5,xlim=c(0,10),ylim=c(0,10),cex.lab=1,col=i)
      abline(0,1,lwd=1,col="red")
      
      error = mae(as.matrix(prediction),as.matrix(val.y)) #calculate mean absolute error for prediction
      r_square <- 1-(sum((prediction-val.y)^2)/sum((val.y-avg)^2)) #calculate r-square for prediction
      rootmse=rmse(prediction,val.y)  #calculate r-root mean square error for prediction
      
      t=t+1
      results[t,1]=error
      results[t,2]=rootmse
      results[t,3]=r_square
      results[t,4]=i
      results[t,5]=j
      results[t,6]=n
      
      print(c("Program is running","j=",j,"n=",n,"i=",i))
      
      
    }
  }
}

# Save Results File:
resultfile <- write_xlsx(results)

NTREE_OPT <- 500   # PUT THE OPTIMUM TREE NUMBER BY USING RMSE, ERROR, R-SQUARE FROM ABOVE
MTRY_OPT <- 5  # PUT THE OPTIMUM NUMBER OF VARIABLES FOR EACH SPLIT BY USING RMSE, ERROR, R-SQUARE FROM ABOVE

train_model <- data.frame(train)

# Train Data Results:

error_train=numeric()
r_square_train=numeric()
rootmse_train=numeric()
prediction_train=numeric()
val_train=numeric()
avg_train=numeric()

avg_train=mean(train_model[,ncol(train_model)])

set.seed(123)

model_train = randomForest( dispersion ~ .,data=train_model, ntree=NTREE_OPT,nodesize=5,mtry=MTRY_OPT, importance =T, na.action=na.omit)

prediction_train <- c(prediction_train,predict(model_train,train_model[,-ncol(train_model)]))

val_train <- c(val_train,train_model[,ncol(train_model)])

plot(x=(train_model[,ncol(train_model)]),y=prediction_train,
     main="Train Data Results with 5-Folds Classification",
     xlab="log(Observed Pd Dispersion (in %))" ,
     ylab="log(Predicted Pd Dispersion (in %))",
     pch=19,col="black",
     cex.axis=1.5,
     xlim=c(0,2),
     ylim=c(0,2),
     cex.lab=1)
abline(0,1,lwd=1.85,col="red")


h=data.frame()
l=data.frame()

h <- train_model[,ncol(train_model)]
l <- c(l,prediction_train)

resultfile_train_observed <- write_xlsx(data.frame(h))
resultfile_train_predicted <- write_xlsx(data.frame(l))


# Conversion to percentages:

prediction_train_perc = 10^(prediction_train)
val_train_perc = 10^(val_train)
avg_train_prec=10^(mean(train_model[,ncol(train_model)]))


# MAE, R2 and Root MSE values for train data:

error_T = mae(as.matrix(prediction_train_perc),as.matrix(val_train_perc)) #calculate mean absolute error for prediction
r_square_T <- 1-(sum((prediction_train_perc-val_train_perc)^2)/sum((val_train_perc-avg_train_prec)^2)) #calculate r-square for prediction
rootmse_T = rmse(prediction_train_perc,(val_train_perc))  #calculate r-root mean square error for prediction
print(c("Train Data Results:","MAE:",error_T,"RootMSE:",rootmse_T,"R2:",r_square_T))

plot(x=10^((train_model[,ncol(train_model)])),y=prediction_train_perc,
     main="Train Data Results with 5-Folds Classification",
     xlab="Observed Pd Dispersion (in %)" ,
     ylab="Predicted Pd Dispersion (in %)",
     pch=19,col="blue",
     cex.axis=1.5,
     xlim=c(0,100),
     ylim=c(0,100),
     cex.lab=1)
abline(0,1,lwd=1.85,col="red")


# Test Model with Test Data:  

test <- data.frame(test)

set.seed(123)

tes=numeric()
error_test=numeric()
rsquare_test=numeric()
rmse_test=numeric()
prediction_test=numeric()
tes_prec=numeric()


prediction_test <- c(prediction_test,predict(model_train,test[,-ncol(test)]))
tes <- c(tes,test[,ncol(test)])
aveg=mean(test[,ncol(test)])

# Conversion to percentages:

prediction_test_prec = 10^(c(prediction_test,predict(model_train,test[,-ncol(test)])))
tes_prec = 10^(c(tes,test[,ncol(test)]))
aveg_prec = mean(tes_prec)

plot(x=test[,ncol(test)],y=prediction_test,
     main="Test Data Results with 5-Folds Classification",
     xlab="log(Observed Pd Dispersion (in %))" ,
     ylab="log(Predicted Pd Dispersion (in %))",
     pch=19,col="black",
     cex.axis=1.5,
     xlim=c(0,2),
     ylim=c(0,2),
     cex.lab=1)
abline(0,1,lwd=0.85,col="red")


k=data.frame()
s=data.frame()

k <- test[,ncol(test)]
s <- c(s,prediction_test)

resultfile_test_observed <- write_xlsx(data.frame(k))
resultfile_test_predicted <- write_xlsx(data.frame(s))


error_TE_prec=mae(as.matrix(tes_prec),as.matrix(prediction_test_prec))
rsquare_TE_prec=1-(sum((prediction_test_prec-tes_prec)^2)/sum((tes_prec-aveg_prec)^2))  
rmse_TE_prec=rmse(tes_prec,prediction_test_prec)
print(c("Test Data Results:","Error:",error_TE_prec,
        "Root MSE:",rmse_TE_prec,"R squared:",rsquare_TE_prec))

plot(x=tes_prec,y=prediction_test_prec,
     main="Test Data Results with 5-Folds Classification",
     xlab="Observed Pd Dispersion %" ,
     ylab="Predicted Pd Dispersion %",
     pch=19,col="blue",
     cex.axis=1.5,
     xlim=c(0,100),
     ylim=c(0,100),
     cex.lab=1)
abline(0,1,lwd=0.85,col="red")
