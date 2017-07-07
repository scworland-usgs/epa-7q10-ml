

train_ml_models <- function(model_data,data_full) {
  
  # input <- menu(c("yes", "no"), title="Are you sure you want to run this function? It will take > 10 hours.")
  # 
  # if(input==2){
  #   stop("User (wisely) chose to exit function due to absurd runtime...")
  # }
  
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
  rf_params <- bayes_optim_caret(model_data,'rf',rf_bounds,iter=15,acq = "ucb")
  
  print(rf_params$Best_Par)
  
  # gradient boosting machine ----------------------------
  
  # http://xgboost.readthedocs.io/en/latest/parameter.html
  gbm_bounds <- list(nrounds = c(1L,1000L),
                     max_depth = c(1L,20L),
                     eta = c(0,1),
                     gamma=c(0,100),
                     colsample_bytree=c(0.1,1),
                     min_child_weight=c(0L,20L),
                     subsample=c(0,1))
  
  ## Bayesian optimization 
  gbm_params <- bayes_optim_caret(model_data,'xgbTree',gbm_bounds,iter=15,acq = "ucb")
  
  print(gbm_params$Best_Par)
  
  # cubist --------------------------------------------
  
  cubist_bounds <-  list(committees = c(1L,100L), # 3.5 hours
                         neighbors = c(0L,9L))
  
  cubist_params <- bayes_optim_caret(model_data,'cubist',cubist_bounds,iter=15,acq = "ucb")

  print(cubist_params$Best_Par)
  
  # support vector machine with Gaussian kernel --------------------
  # run one more with larger C boundary
  svmg_bounds <-  list(C = c(1,5),
                       sigma = c(0,0.1))
  
  svmg_params <- bayes_optim_caret(model_data,'svmRadial',svmg_bounds,iter=15,acq='ei')
  
  print(svmg_params$Best_Par)
  
  # support vector machine with polynomial kernel --------------------
  
  svmp_bounds <-  list(C = c(0,2),
                       degree = c(1L,3L),
                       scale = c(0,0.1))
  
  svmp_params <- bayes_optim_caret(model_data,'svmPoly',svmp_bounds,iter=15,acq = "ucb")
  
  print(svmp_params$Best_Par)
  
  # k-nearest neighbors -------------------------------------------
  
  # kernel = c("triangular", "rectangular", 
  #            "epanechnikov", "optimal")
  kknn_grid <- expand.grid(kmax=c(4,6,8,10,12), # max number of neighbors
                           distance = c(0.25,0.5,1),
                           kernel = c("triangular","optimal"))
  
  kknn_params <- train(y~., data=model_data,
                       trControl = trainControl(method = 'LOOCV', search = 'grid'),
                       tuneGrid = kknn_grid,
                       method = "kknn")
  
  print(kknn_params$bestTune)

  # elastic net -------------------------------------------------
  
  enet_bounds <-  list(alpha = c(0,1),
                       lambda = c(0,1.5))
  
  enet_params <- bayes_optim_caret(model_data,'glmnet',enet_bounds,iter=30,acq = "ucb")
  
  print(enet_params$Best_Par)
  
  # models stop -----------------------------------------------
  
  ## stop the clock 
  time <- proc.time() - ptm 
  
  print(time)
  
  # save models
  
  ml_params <- list(rf_params, 
                    gbm_params, 
                    cubist_params, 
                    svmg_params,
                    svmp_params,
                    kkkn_params,
                    enet_params)
  
  return(ml_params)
  
}
