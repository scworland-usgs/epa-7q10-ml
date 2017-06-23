
library(pacman)
p_load(dplyr,PUBAD,reshape2,randomForest,kknn,gbm,kernlab,Cubist,glmnet)

source("fit_metrics.R")

setwd("~/Documents/Ungaged basins/R_ungaged/EPA7Q10/epa-lowflow-FY16/final_scripts")

## Load csv file
data.full <- read.csv("data/epa_lowflow_final_input.csv",header=T,na.strings = "-999") %>%
  setNames(tolower(names(.))) # make column names lower case

## Transform and scale response
area <- data.full$drain_sqkm
ln7q10.da <- log((data.full$y7q10+0.001)/area)

## use functions from PUBAD
expVars <- data.frame(gages = data.full$staid) %>%
  getBasinChar()

## create model data using clean basin chars
model.data <- expVars$cleanBCs %>% 
  mutate(CLASS = as.integer(as.factor(CLASS))) %>%
  mutate_each(funs(as.numeric)) %>% 
  scale() %>%
  as.data.frame() %>% 
  setNames(tolower(names(.))) %>% 
  mutate(y=ln7q10.da) %>%
  select(y, class,lat_gage:aspect_eastness)

preds <- matrix(data=NA, ncol=8, nrow=nrow(model.data))
colnames(preds) <- c("rf","knn","gbm","svmp","svmr","cubist","glmnet","obs")

for (i in 1:nrow(model.data)){
  # select nrows-1 from model data
  train <- model.data[-i,]
  test <- model.data[i,]
  
  #fit model models with formula
  set.seed(1)
  rf.fit <- randomForest(y~., data=train, ntree=200, mtry=90)

  set.seed(1)
  knn.fit <- kknn(y~., train, test, k = 5, distance = 0.25, kernel = "triangular")

  set.seed(1)
  gbm.fit <- gbm(y~., data=train, distribution="gaussian", n.trees=100,
                 interaction.depth=5, n.minobsinnode = 10, shrinkage=0.1,
                 verbose=F,keep.data=F)

  set.seed(1)
  svmp.fit <- ksvm(y~., data=train, C=1, kernel="polydot",
                  kpar=list(degree=2, scale=0.0025))
  
  set.seed(1)
  svmr.fit <- ksvm(y~., data=train, C=2, kernel="rbfdot",
                   kpar=list(sigma=0.007682599))
  
  
  # fit models with design matrix and response vector
  train2 <- as.matrix(train[,-1])
  y2 <- as.numeric(train[,1])
  test2 <- as.matrix(test[,-1])
  
  cubist.fit <- cubist(train2, y2, committees=10, neighbors=9, 
                       control=cubistControl(seed=1))
  
  glmnet.fit <- glmnet(train2,y2,alpha=0.8)
  
  # test model on left-out observation
  preds[i,1] <- (exp(predict(rf.fit, test)) * area[i]) - 0.001
  preds[i,2] <- (exp(knn.fit$fitted.values) * area[i]) - 0.001
  preds[i,3] <- (exp(predict(gbm.fit, test)) * area[i]) - 0.001
  preds[i,4] <- (exp(predict(svmp.fit, test)) * area[i]) - 0.001
  preds[i,5] <- (exp(predict(svmr.fit, test)) * area[i]) - 0.001
  preds[i,6] <- (exp(predict(cubist.fit, test2)) * area[i]) - 0.001
  preds[i,7] <- (exp(predict(glmnet.fit, test2, s=0.193)) * area[i]) - 0.001

  
  preds[i,8] <- data.full$y7q10[i]
}

# make data frame for stacked model
ml.preds <- data.frame(preds) %>% 
  select(obs,rf:glmnet) %>%
  mutate(med=apply(.[,2:8],1,median)) %>%
  mutate(mean=apply(.[,2:8],1,mean))

# stacked model
# tran model
library(caret)
ctrl <- trainControl(method = "LOOCV")
stack.cubist <- train(obs~., data=ml.preds,
                      trControl=ctrl,
                      tuneLength=20,
                      method='cubist')

stack.preds <- numeric()
for (i in 1:nrow(ml.preds)){
  
  # select nrows-1 from model data
  train <- ml.preds[-i,]
  test <- ml.preds[i,]
  
  stack.cubist <- cubist(train[,-1], train[,1], committees=10, neighbors=0,
                       control=cubistControl(seed=1))

  # predict left out observations
  stack.preds[i] <- predict(stack.cubist, test[,-1])
}

# add to ml.preds
ml.preds <- ml.preds[,1:8] %>%
  mutate(meta_cubist=stack.preds)

save(ml.preds,file="ml.preds.rda")



