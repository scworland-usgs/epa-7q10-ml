

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
  source('scripts/bayes_optim_caret.R')
  
  # random forest --------------------------------
  
  ptm <- proc.time() 
  
  ## boundaries
  rf_bounds <-  list(mtry = c(2L,120L))
  
  ## Bayesian optimization 
  rf_params <- bayes_optim_caret(model_data,'rf',rf_bounds,iter=30)
  
  time <- proc.time() - ptm 
  
  # gradient boosting machine ----------------------------
  
  # http://xgboost.readthedocs.io/en/latest/parameter.html
  gbm_bounds <- list(nrounds = c(1L,1000L),
                     max_depth = c(1L,20L),
                     eta = c(0,1),
                     gamma=c(0,100),
                     colsample_bytree=c(0.005,1),
                     min_child_weight=c(0L,20L),
                     subsample=c(0,1))
  
  ## Bayesian optimization 
  gbm_params <- bayes_optim_caret(model_data,'xgbTree',gbm_bounds,iter=30)
  
  # cubist --------------------------------------------
  
  cubist_bounds <-  list(committees = c(1L,100L),
                         neighbors = seq(0L,9L))
  
  cubist_params <- bayes_optim_caret(model_data,'cubist',cubist_bounds,iter=30)
  
  # support vector machine with Gaussian kernel --------------------
  # run one more with larger C boundary
  svmg_bounds <-  list(C = c(1,10),
                       sigma = c(0,1))
  
  svmg_params <- bayes_optim_caret(model_data,'svmRadial',svmg_bounds,iter=30)
  
  # support vector machine with polynomial kernel --------------------
  
  svmp_bounds <-  list(C = c(1,10),
                       degree = c(1L,3L),
                       scale = c(0,1))
  
  svmp_params <- bayes_optim_caret(model_data,'svmPoly',svmp_bounds,iter=30)
  
  # k-nearest neighbors -------------------------------------------
  
  kknn_grid <- expand.grid(kmax=c(10), # max number of neighbors
                           distance = c(0.25,0.5,1,2),
                           kernel = c("triangular", "rectangular", 
                                      "epanechnikov", "optimal"))
  
  kknn_fit <- train(y~., data=model_data,
                    trControl = ctrl,
                    tuneGrid = knn_grid,
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
  
  enet_bounds <-  list(alpha = c(0,1),
                       lambda = c(0,1.5))
  
  enet_params <- bayes_optim_caret(model_data,'glmnet',enet_bounds,
                                   iter=30,acq = "ucb")
  
  # models stop -----------------------------------------------
  
  ## stop the clock 
  time <- proc.time() - ptm 
  
  # save models
  
  ml_preds <- list(rf=rf.preds, 
                   cubist=cubist.preds, 
                   gbm=gbm.preds, 
                   svm=svm.preds)
  
}
