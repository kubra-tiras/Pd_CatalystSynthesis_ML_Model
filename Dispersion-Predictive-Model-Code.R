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
CrossVal <- train[sample,]      # Train Data is duplicated with excluding out-of-bag data??
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
      plot(value[,ncol(value)], predict(model,value[,-ncol(value)]),
           main="Validation Data Results of Random Forest with 5-Folds Classification"
           ,xlab="Observed Data",ylab="Predicted Data",
           pch=19,cex.axis=1.5,xlim=c(0,100),ylim=c(0,100),cex.lab=1,col=i)
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


  NTREE_OPT <- 400   # PUT THE OPTIMUM TREE NUMBER BY USING RMSE, ERROR, R-SQUARE FROM ABOVE
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
  model_train = randomForest(dispersion ~ .,data=train_model, ntree=NTREE_OPT,nodesize=5,mtry=MTRY_OPT, importance =T, na.action=na.omit)
  vi <- varImp(model_train)
  plot(vi)
  
  prediction_train <- c(prediction_train,predict(model_train,train_model[,-ncol(train_model)]))
  val_train <- c(val_train,train_model[,ncol(train_model)])
  
  
  plot(x=train_model[,ncol(train_model)],y=prediction_train,
       main="Train Data Results with 5-Folds Classification",
       xlab="Observed Pd Dispersion %" ,
       ylab="Predicted Pd Dispersion %",
       pch=19,col="black",
       cex.axis=1.5,
       xlim=c(0,100),
       ylim=c(0,100),
       cex.lab=1)
  abline(0,1,lwd=1.85,col="red")
  
  importance(model_train)
  
  # MAE, R2 and Root MSE values for train data before correction function:
  
  error_T = mae(as.matrix(prediction_train),as.matrix(val_train)) #calculate mean absolute error for prediction
  r_square_T <- 1-(sum((prediction_train-val_train)^2)/sum((val_train-avg_train)^2)) #calculate r-square for prediction
  rootmse_T = rmse(prediction_train,val_train)  #calculate r-root mean square error for prediction
  
  
  print(c("Train Data Results Before CF:","MAE:",error_T,"RootMSE:",rootmse_T,"R2:",r_square_T))

  
  # Correction Function Optimization:
  
  a=0
  error_cf=numeric()
  r_square_cf=numeric()
  rootmse_cf=numeric()
  m=numeric()
  k=numeric()
  results_cf=data.frame(error_cf,rootmse_cf,r_square_cf,m,k)
  for (m in seq(from=1.0, to=1.9, by=0.01))
  {
    
    for (k in seq(from=2, to=8, by=0.01))
    {
      train_cf= prediction_train*m-k
      error_cf = mae(as.matrix(train_cf),as.matrix(val_train))
      r_square_cf <- 1-(sum((train_cf-val_train)^2)/sum((val_train-avg_train)^2))
      rootmse_cf = rmse(train_cf,val_train)
      
      a=a+1
      results_cf[a,1]=error_cf
      results_cf[a,2]=rootmse_cf
      results_cf[a,3]=r_square_cf
      results_cf[a,4]=m
      results_cf[a,5]=k
    }
  }
  resultfile_cf <- write_xlsx(results_cf)
  
  prediction_corrected = prediction_train*1.22-6.57
  
  
  plot(x=train_model[,ncol(train_model)],y=prediction_corrected,
       main="Train Data Results with 5-Folds Classification",
       xlab="Observed Pd Dispersion %" ,
       ylab="Predicted Pd Dispersion %",
       pch=19,col="Blue",
       cex.axis=1.5,
       xlim=c(0,100),
       ylim=c(0,100),
       cex.lab=1)
  abline(0,1,lwd=1.85,col="red")
  
  
  # MAE, R2 and Root MSE values for train data after correction function:
  
  error_TCF = mae(as.matrix(prediction_corrected),as.matrix(val_train)) #calculate mean absolute error for prediction
  r_square_TCF <- 1-(sum((prediction_corrected-val_train)^2)/sum((val_train-avg_train)^2)) #calculate r-square for prediction
  rootmse_TCF = rmse(prediction_corrected,val_train)  #calculate r-root mean square error for prediction
  
  
  print(c("Train Data Results After CF:","MAE:",error_TCF,
          "Root MSE:",rootmse_TCF,"R squared:",r_square_TCF)) 
  
# Test Model with Test Data:  
  
  test <- data.frame(test)
  
  set.seed(123)

  tes=numeric()
  error_test=numeric()
  rsquare_test=numeric()
  rmse_test=numeric()
  prediction_test=numeric()

  prediction_test <- c(prediction_test,predict(model_train,test[,-ncol(test)]))
  tes <- c(tes,test[,ncol(test)])
  aveg=mean(test[,ncol(test)])
  
  error_cf_test=numeric()
  rquare_cf_test=numeric()
  rmse_cf_test=numeric()

  prediction_test_corrected=prediction_test*1.22-6.57
  test_y <- prediction_test_corrected
  test_x <- test[,ncol(test)]
  results_test_record <- data.frame(test_y,test_x)
  result_test_record <- write_xlsx(results_test_record)
  
  plot(x=test[,ncol(test)],y=prediction_test_corrected,
       main="Test Data Results with 5-Folds Classification",
       xlab="Observed Pd Dispersion %" ,
       ylab="Predicted Pd Dispersion %",
       pch=19,col="Blue",
       cex.axis=1.5,
       xlim=c(0,100),
       ylim=c(0,100),
       cex.lab=1)
  abline(0,1,lwd=0.85,col="red")
  
  plot(x=test[,ncol(test)],y=prediction_test,
       main="Test Data Results with 5-Folds Classification",
       xlab="Observed Pd Dispersion %" ,
       ylab="Predicted Pd Dispersion %",
       pch=19,col="black",
       cex.axis=1.5,
       xlim=c(0,100),
       ylim=c(0,100),
       cex.lab=1)
  abline(0,1,lwd=0.85,col="red")
  
  # MAE, R2 and Root MSE values for train data before correction function:
  
  error_TE=mae(as.matrix(tes),as.matrix(prediction_test))
  
  rsquare_TE=1-(sum((prediction_test-tes)^2)/sum((tes-aveg)^2))  
  
  rmse_TE=rmse(tes,prediction_test)
  
  print(c("Test Data Results Before CF:","Error:",error_TE,
          "Root MSE:",rmse_TE,"R squared:",rsquare_TE)) 
  
  # MAE, R2 and Root MSE values for train data after correction function:
  
  error_TECF=mae(as.matrix(tes),as.matrix(prediction_test_corrected))
  
  rsquare_TECF=1-(sum((prediction_test_corrected-tes)^2)/sum((tes-aveg)^2))  
  
  rmse_TECF=rmse(tes,prediction_test_corrected)
  
  print(c("Test Data Results After CF:","Error:",error_TECF,
          "Root MSE:",rmse_TECF,"R squared:",rsquare_TECF)) 

  
  # Experimental Data for Testing the Data:  
  
  
  expt = read_excel('C:/Users/kubra/Desktop/experimentdata.xlsx')
  
  # Print data and convert into factors for Method, Precursor, 
  #   Support, pH adjustment, Reduction Solvent/Gas: 
  
  expt <- expt[,c(1,5,6,8,11,12,13,14,15,16,17,19,20,22,23,30)]
  glimpse(expt)
  
  # Convert other columns into numeric:
  
  columns_exp <- c(2,3,5,6,9,11,12,13,14,15,16)
  expt[, columns_exp] <- lapply(columns_exp, function(x) as.numeric(expt[[x]]))
  glimpse(expt)
  
  # Print data and convert into factors for Method, Precursor, 
  #   Support, pH adjustment, Reduction Solvent/Gas: 
  
  names_exp <- c(1,4,7,8,10)
  expt[,names_exp] <- lapply(expt[,names_exp] , factor)
  glimpse(expt)
  
  # TO DEAL WITH NA - IMPUTATION:
  
  expt$precursor[is.na(expt$precursor)] <- mode(expt$precursor)
  expt$solvent[is.na(expt$solvent)] <- mode(expt$solvent)
  expt$SupportSA[is.na(expt$SupportSA)] <- mean(expt$SupportSA,na.rm = TRUE)
  expt$SupportCalcTemp[is.na(expt$SupportCalcTemp)] <- 298
  expt$SupportCalcTime[is.na(expt$SupportCalcTime)] <- 2
  expt$solventpH[is.na(expt$solventpH)] <- median(expt$solventpH,na.rm = TRUE)
  expt$PZC[is.na(expt$PZC)] <- mean(expt$PZC,na.rm = TRUE)
  expt$CalcTime[is.na(expt$CalcTime)] <- 2
  expt$CalcTemp[is.na(expt$CalcTemp)] <- 298
  expt$ReducTemp[is.na(expt$ReducTemp)] <- 298
  expt$ReducTime[is.na(expt$ReducTime)] <- 2
  
  # To produce same sequence we are deciding a fixed number:
  
  set.seed(123)
  
  expdata=numeric()
  error_exp=numeric()
  rsquare_exp=numeric()
  rmse_exp=numeric()
  prediction_exp=numeric()
  aveg_exp=numeric()

  prediction_exp <- c(prediction_exp,predict(model_train,expt[,-ncol(expt)]))
  expdata <- as.matrix(c(expdata,expt[,ncol(expt)]))
  a <- as.matrix(expt[,ncol(expt)])
  aveg_exp=mean(a)
  
  prediction_exp_cf = prediction_exp*1.22-6.57
  prediction_exp_cf
  
  #Boruta Importance Plot:
  
  data1 <- na.omit(dataPd)     # Get rid of columns with NA and ND
  boruta.train <- Boruta(dispersion~., data = data1, doTrace = 2)
  print(boruta.train)
  plot(boruta.train, xlab = "", xaxt = "n",cex.axis = 1.5)
  lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
    boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
  names(lz) <- colnames(boruta.train$ImpHistory)
  Labels <- sort(sapply(lz,median))
  axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory))

  
  
  


