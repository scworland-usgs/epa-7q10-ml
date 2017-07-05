


train_ml_models <- function(model_data,data_full) {
  
  input <- menu(c("yes", "no"), title="Are you sure you want to run this function? It will take > 10 hours.")
  
  if(input==2){
    stop("User (wisely) chose to exit function due to absurd runtime...")
  }
  
  # if OS is unix, register 4 cores for parallel processing
  if(.Platform$OS.type == "unix") { # if unix
    if(!require(doMC)){             # check if doMC is installed...
      install.packages("doMC")      # ...if not install it
    }
    library(doMC)                   # load the library
    registerDoMC(cores = 4)         # register cores
  }
  
  library(caret)
  
  ## set up leave one out cross validation 
  ctrl <- trainControl(method = "LOOCV", search = "grid")
  
  ## start the clock
  ptm <- proc.time() 
  
  # random forest --------------------------------
  
  rf_grid <-  expand.grid(.mtry = seq(50,100,10))
  
  rf_fit <- train(y~., data=model_data,
                  trControl=ctrl,
                  tuneGrid = rf_grid,
                  importance=T,
                  ntree=200,
                  method='rf')
  
  rf_param_error <- arrange(rf_fit$results, RMSE) 
  
  ## extract best predictions
  rf_preds <- inner_join(rf_fit$pred,rf_fit$bestTune) %>%
    mutate(pred = (exp(pred) * data_full$drain_sqkm) + 0.001,
           obs = (exp(obs) * data_full$drain_sqkm) + 0.001) %>%
    mutate(obs = replace(obs, obs<0.0025, 0))
  
  # gradient boosting machine ----------------------------
  
  gbm_grid <- expand.grid(.nrounds = c(650,750),
                          .max_depth = c(4,6),
                          .eta = c(0.05),
                          .gamma=c(5.5,7.5),
                          .colsample_bytree=c(0.65),
                          .min_child_weight=c(10),
                          .subsample=c(0.55,0.65))
  

  gbm_fit <- train(y~., data=model_data,
                   trControl=ctrl,
                   tuneGrid = gbm_grid,
                   method='xgbTree',
                   distribution="gaussian",
                   verbose=FALSE)
  
  gbm_param_error <- arrange(gbm_fit$results, RMSE) 
  
  ## extract best predictions
  gbm_preds <- inner_join(gbm_fit$pred,gbm_fit$bestTune) %>%
    mutate(pred = (exp(pred) * data_full$drain_sqkm) + 0.001,
           obs = (exp(obs) * data_full$drain_sqkm) + 0.001) %>%
    mutate(obs = replace(obs, obs<0.002, 0))
  
  # cubist --------------------------------------------
  
  cubist_grid <-  expand.grid(.committees = c(5,10,15),
                              .neighbors = seq(3,9,1))
  
  cubist_fit <- train(y~., data=model_data,
                      trControl=ctrl,
                      tuneGrid=cubist_grid,
                      method='cubist')
  
  cubist_param_error <- arrange(cubist_fit$results, RMSE) 
  
  ## plot training
  plot(cubist_fit)
  
  ## extract best predictions
  cubist_preds <- inner_join(cubist_fit$pred,cubist_fit$bestTune) %>%
    mutate(pred = exp(pred) * data_full$drain_sqkm,
           obs = exp(obs) * data_full$drain_sqkm) %>%
    mutate(obs = replace(obs, obs<0.002, 0))
  
  # support vector machine with Gaussian kernel --------------------
  
  svmg_grid <-  expand.grid(.sigma = c(0.0008, 0.008, 0.08, 0.8),
                            .C = seq(0.5,5,0.5))
  
  svmg_fit <- train(y~., data=model_data,
                   trControl=ctrl,
                   tuneGrid = svmg_grid,
                   method='svmRadial')
  
  svmg_param_error <- arrange(svmg_fit$results, RMSE) 
  
  ## plot training
  plot(svmg_fit)
  
  ## extract best predictions
  svmg_preds <- inner_join(svmg_fit$pred,svmg_fit$bestTune) %>%
    mutate(pred = exp(pred) * data_full$drain_sqkm,
           obs = exp(obs) * data_full$drain_sqkm) %>%
    mutate(obs = replace(obs, obs<0.002, 0))
  
  # support vector machine with polynomial kernel --------------------
  
  svmp_grid <-  expand.grid(.scale = c(0.0008, 0.008, 0.08, 0.8),
                            .C = seq(0.5,5,0.5),
                            .degree = c(2,3,4))
  
  svmp_fit <- train(y~., data=model_data,
                   trControl=ctrl,
                   tuneGrid = svmp_grid,
                   method='svmPoly')
  
  svmp_param_error <- arrange(svmp_fit$results, RMSE) 
  
  ## plot training
  plot(svmp_fit)
  
  ## extract best predictions
  svmp_preds <- inner_join(svmp_fit$pred,svmp_fit$bestTune) %>%
    mutate(pred = exp(pred) * data_full$drain_sqkm,
           obs = exp(obs) * data_full$drain_sqkm) %>%
    mutate(obs = replace(obs, obs<0.002, 0))
  
  # k-nearest neighbors -------------------------------------------
  
  kknn_grid <- expand.grid(kmax=c(1,10,20,30), # max number of neighbors
                          distance = c(0.25,0.5,1,2),
                          kernel = c("triangular"))
  
  kknn_fit <- train(y~., data=model_data,
                   trControl = ctrl,
                   tuneGrid = kknn_grid,
                   method = "kknn")
  
  kknn_param_error <- arrange(kknn_fit$results, RMSE) 
  
  ## plot training
  plot(kknn_fit)
  
  ## extract best predictions
  kknn_preds <- inner_join(kknn.fit$pred,kknn_fit$bestTune) %>%
    mutate(pred = exp(pred) * data_full$drain_sqkm,
           obs = exp(obs) * data_full$drain_sqkm) %>%
    mutate(obs = replace(obs, obs<0.002, 0))
  
  fitmets(kknn_preds)
  
  # elastic net -------------------------------------------------
  
  enet_grid <-  expand.grid(.alpha = seq(0, 1, 0.2),
                            .lambda = seq(0.1,0.2,0.3))
  
  enet_fit <- train(y~., data=model_data,
                      trControl=ctrl,
                      preProcess = NULL,
                      tuneGrid=enet_grid,
                      method='glmnet')
  
  enet_param_error <- arrange(enet_fit$results, RMSE) 
  
  ## plot training
  plot(enet_fit)
  
  enet_preds <- inner_join(enet_fit$pred,enet_fit$bestTune) %>%
    mutate(pred = exp(pred) * data_full$drain_sqkm,
           obs = exp(obs) * data_full$drain_sqkm) %>%
    mutate(obs = replace(obs, obs<0.002, 0))
  
  # models stop -----------------------------------------------
  
  ## stop the clock 
  time <- proc.time() - ptm 
  
  # save models
  
  ml_preds <- list(rf=rf.preds, 
                   cubist=cubist.preds, 
                   gbm=gbm.preds, 
                   svm=svm.preds)
  
}
