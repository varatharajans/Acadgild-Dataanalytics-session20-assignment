# Acadgild-Dataanalytics-session20-assignment
DATA ANALYTICS WITH R, EXCEL AND TABLEAU SESSION 20 ASSIGNMENT 

                                                       Session 20 Assignment
                                                      Weight Lifting Exercise


This human activity recognition research has traditionally focused on discriminating between different activities, i.e. to predict "which" activity was performed at a specific point in time (like with the Daily Living Activities dataset above). The approach we propose for the Weight Lifting Exercises dataset is to investigate "how (well)" an activity was performed by the wearer. The "how (well)" investigation has only received little attention so far, even though it potentially provides useful information for a large variety of applications, such as sports training.
1. Use the below given data set 
Data Set 
2. Perform the below given activities: 
a. Create classification model using different random forest models 
b. Verify model goodness of fit 
c. Apply all the model validation techniques 
d. Make conclusions 
e. Plot importance of variables



setwd("C:/Users/Seshan/Desktop")
library(readr)
Weight_lift <- read.csv("Weight lift.csv")
View(Weight_lift)
str(Weight_lift)
data<-Weight_lift
# load libraries
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(lattice)
library(rattle)

library(C50)
#install.package('devtools') # Only needed if you dont have this installed.
library(devtools)
install_github('adam-m-mcelhinney/helpRFunctions')
library(helpRFunctions)
names(data)
dim(data)
pairs(data[1:10])
# enable multi-core processing
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel()
set.seed(12345)
dataTrain<-data[1:4004,]
dataTest<-data[4005:4024,]
head(dataTrain)
head(dataTest)
indexNA <- as.vector(sapply(dataTrain[,1:158],function(x) {length(which(is.na(x)))!=0}))
dataTrain <- dataTrain[,!indexNA]
train_control<- trainControl(method="cv", number=10)

model<- train(classe ~., data=dataTrain,trControl=train_control, method="rf")
model
# make predictions
predictions<- predict(model,dataTrain)
# append predictions
pred<- cbind(dataTrain,predictions)
# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix
#how do we create a cross validation scheme
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3)
seed <-7
metric <- 'Accuracy'
set.seed(seed)
mtry <- sqrt(ncol(dataTrain))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(pitch_belt~., 
                    data = dataTrain,
                    method = 'rf',
                    metric = 0,
                    tuneGrid = tunegrid,
                    trControl = control)
print(rf_default)
#-------------------------------

# make predictions
predictions<- predict(rf_default,dataTest)
# append predictions
pred<- cbind(dataTest,predictions)
# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix
varImp(rf_default)
#----------------
# random search for parameters
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        search = 'random')
# make predictions
predictions<- predict(rf_default,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
#confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix
varImp(random)
#--------------------

# Grid search
control <- trainControl(method = 'repeatedcv',
                      number = 10,
                       repeats = 3,
                       search = 'grid')
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:80))
mtry <- sqrt(ncol(x))
rf_gridsearch <- train(~., 
                      data = dataTrain[1:200,],
                      method = 'rf',
                       metric = 0,
                      tuneGrid = tunegrid,
                       trControl = control)
print(rf_gridsearch)
plot(rf_gridsearch)
# make predictions
predictions<- predict(rf_gridsearch,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$pitch_belt)
confusionMatrix
varImp(rf_gridsearch)

---------------------------
  # Boosting
  #  ---------------------------------------
# Boosting model requires three things

#1- a loss function to be optimized
#2- a weak learner to make predictions
#3- an additive model to add the weak learners to minimize the loss function

# gradient boosting
control <- trainControl(method = 'repeatedcv',
                        number = 5,
                        repeats = 3,
                        search = 'grid')

seed <- 7
library(C50)
set.seed(seed)
metric <- 'Accuracy'
gbm_mod <- train(pitch_belt~., 
                 data = dataTrain,
                 method = 'gbm',
                 metric = 0,
                 trControl = control)
print(gbm_mod)
plot(gbm_mod)

summary(gbm_mod)
# make predictions
predictions<- predict(gbm_mod,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix


