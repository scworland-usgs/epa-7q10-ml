
# Description:
# This script allows a user to recreate the model results using the optimal hyparameters.
# The optimal hyperparameters are calculated in the "training_models.R" script. This script 
# requires several R packages to be installed prior to the analysis.

## Install and load packages
install.packages(c("dplyr","reshape2","randomForest","kknn","caret",
                   "gbm","kernlab","Cubist","glmnet","devtools"))

devtools::install_github("wfarmer-usgs/PUBAD")

library(dplyr)
library(reshape2)
library(randomForest)
library(kknn)
library(gbm)
library(kernlab)
library(Cubist)
library(glmnet)
library(devtools)
library(PUBAD)
library(caret)

# set working directory for script
setwd("~/Documents/Ungaged basins/epa_7q10_ml")

## Load csv file: note, data should be in a "data" folder in working directory
data_full <- read.csv("data/lowflow_sc_ga_al_gagesII_2015.csv",header=T,na.strings = "-999") %>%
  setNames(tolower(names(.))) # make column names lower case

## Transform and scale response
area <- data_full$drain_sqkm
ln7q10_da <- log((data_full$y7q10+0.001)/area)

## use functions from PUBAD to cull covariates
expVars <- data.frame(gages = data_full$staid) %>%
  getBasinChar()

## create model data using clean basin chars
model_data <- expVars$cleanBCs %>% # start with basin chars
  mutate(CLASS = as.integer(as.factor(CLASS))) %>% # change class to integer
  mutate_each(funs(as.numeric)) %>% # make everything numeric
  scale() %>% # convert to z-score: (x-mu)/sigma
  as.data.frame() %>% # make data frame
  setNames(tolower(names(.))) %>% # make sure colname are lower case
  mutate(y=ln7q10_da) %>% # add the transformed response variable from above
  select(y, class,lat_gage:aspect_eastness) # reorder column positions

## pre-allocate prediction matrix
preds <- matrix(data=NA, ncol=8, nrow=nrow(model_data))
colnames(preds) <- c("rf","knn","gbm","svmp","svmr","cubist","glmnet","obs")

## This for-loop makes leave-one-out cross validated predictions using the optimal
## hyperparameters found in the 'model_training.R' file. There are much sexier ways
## to do this, but this is the most transparent I believe. It might take a couple of 
## minutes to run, as it has to train the model on new data each time.
for (i in 1:nrow(model_data)){
  # select nrows-1 from model data
  train <- model_data[-i,]
  test <- model_data[i,]
  
  #fit model models with formula
  set.seed(1)
  rf_fit <- randomForest(y~., data=train, ntree=200, mtry=90)

  set.seed(1)
  knn_fit <- kknn(y~., train, test, k = 5, distance = 0.25, kernel = "triangular")

  set.seed(1)
  gbm_fit <- gbm(y~., data=train, distribution="gaussian", n.trees=100,
                 interaction.depth=5, n.minobsinnode = 10, shrinkage=0.1,
                 verbose=F,keep.data=F)

  set.seed(1)
  svmp_fit <- ksvm(y~., data=train, C=1, kernel="polydot",
                  kpar=list(degree=2, scale=0.0025))
  
  set.seed(1)
  svmr_fit <- ksvm(y~., data=train, C=2, kernel="rbfdot",
                   kpar=list(sigma=0.007682599))
  
  
  # fit models with design matrix and response vector
  train2 <- as.matrix(train[,-1])
  y2 <- as.numeric(train[,1])
  test2 <- as.matrix(test[,-1])
  
  cubist_fit <- cubist(train2, y2, committees=10, neighbors=9, 
                       control=cubistControl(seed=1))
  
  glmnet_fit <- glmnet(train2,y2,alpha=0.8)
  
  # test model on left-out observation
  preds[i,1] <- (exp(predict(rf_fit, test)) * area[i]) - 0.001
  preds[i,2] <- (exp(knn_fit$fitted.values) * area[i]) - 0.001
  preds[i,3] <- (exp(predict(gbm_fit, test, n.trees=100)) * area[i]) - 0.001
  preds[i,4] <- (exp(predict(svmp_fit, test)) * area[i]) - 0.001
  preds[i,5] <- (exp(predict(svmr_fit, test)) * area[i]) - 0.001
  preds[i,6] <- (exp(predict(cubist_fit, test2)) * area[i]) - 0.001
  preds[i,7] <- (exp(predict(glmnet_fit, test2, s=0.193)) * area[i]) - 0.001

  
  preds[i,8] <- data_full$y7q10[i]
}

## make data frame for stacked model
ml_preds <- data.frame(preds) %>% 
  select(obs,rf:glmnet) %>%
  mutate(med=apply(.[,2:8],1,median)) %>% # calculate median
  mutate(mean=apply(.[,2:8],1,mean)) # calcualte mean

## stacked model (Figure 3 in paper)
## Stacked regression combines the LOO-CV predictions (and mean and median) from all 
## the machine learning models using a level-1 M5-cubist model

### Use random search to find optimal hyperparameters
ctrl <- trainControl(method = "LOOCV")
stack_cubist <- train(obs~., data=ml_preds,
                      trControl=ctrl,
                      tuneLength=20,
                      method='cubist')

## tuned hyperparameters
committees = stack_cubist$bestTune[[1]]
neighbors = stack_cubist$bestTune[[2]]

## calculate loo-cv cross validate predictions using for-loop like above. The optimimal
### hyperparameters of committees=10, and neighbors=2 were found above.
stack_preds <- numeric() # preallocate numeric prediction vector
for (i in 1:nrow(ml_preds)){
  
  # select nrows-1 from model data
  train <- ml_preds[-i,]
  test <- ml_preds[i,]
  
  stack_cubist <- cubist(train[,-1], train[,1], committees=10, neighbors=0,
                       control=cubistControl(seed=1))

  # predict left out observations
  stack_preds[i] <- predict(stack_cubist, test[,-1])
}

# add stacked predictions to to ml_preds. meta_cubist==stacked model
ml_preds <- ml_preds[,1:8] %>%
  mutate(meta_cubist=stack_preds)

# save to rda file to use later
save(ml.preds,file="ml_preds.rda")



