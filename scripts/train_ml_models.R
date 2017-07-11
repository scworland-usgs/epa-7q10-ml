

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
  rf_params <- bayes_optim_caret(data=model_data,
                                 method='rf',
                                 bounds=rf_bounds,
                                 iter=15,
                                 kappa=0.5)
  
  print(rf_params$Best_Par)
  
  # gradient boosting machine ----------------------------
  
  gbm_bounds <- list(n.trees = c(1L,1000L),
                     interaction.depth = c(1L,30L),
                     shrinkage = c(0,1),
                     n.minobsinnode=c(1L,50L))
  
  ## Bayesian optimization 
  gbm_params <- bayes_optim_caret(data=model_data,
                                  method='gbm',
                                  bounds=gbm_bounds,
                                  iter=15,
                                  kappa=0.5)
  
  print(gbm_params$Best_Par)
  
  # cubist --------------------------------------------
  
  cubist_bounds <-  list(committees = c(1L,100L), # 3.5 hours
                         neighbors = c(0L,9L))
  
  cubist_params <- bayes_optim_caret(data=model_data,
                                     method='cubist',
                                     bounds=cubist_bounds,
                                     iter=15,
                                     kappa=0.5)

  print(cubist_params$Best_Par)
  
  # support vector machine with Gaussian kernel --------------------

  svmg_bounds <-  list(C = c(1,5),
                       sigma = c(0,0.1))
  
  svmg_params <- bayes_optim_caret(model_data,
                                   'svmRadial',
                                   svmg_bounds,
                                   iter=15,
                                   kappa=0.5)
  
  print(svmg_params$Best_Par)
  
  # support vector machine with polynomial kernel --------------------
  
  svmp_bounds <-  list(C = c(0,2),
                       degree = c(1L,3L),
                       scale = c(0,0.1))
  
  svmp_params <- bayes_optim_caret(data=model_data,
                                   method='svmPoly',
                                   bounds=svmp_bounds,
                                   iter=15,
                                   kappa=0.5)
  
  print(svmp_params$Best_Par)
  
  # k-nearest neighbors -------------------------------------------
  
  kknn_grid <- expand.grid(kmax=c(4,6,8,10,12), # max number of neighbors
                           distance = c(0.25,0.5,1),
                           kernel = c("triangular", "rectangular", 
                                      "epanechnikov", "optimal"))
  
  kknn_params <- train(y~., data=model_data,
                       trControl = trainControl(method = 'LOOCV', search = 'grid'),
                       tuneGrid = kknn_grid,
                       method = "kknn")
  
  print(kknn_params$bestTune)

  # elastic net -------------------------------------------------
  
  enet_bounds <-  list(alpha = c(0,1),
                       lambda = c(0,1.5))
  
  enet_params <- bayes_optim_caret(data=model_data,
                                   method='glmnet',
                                   bounds=enet_bounds,
                                   iter=15,
                                   kappa = 0.2)
  
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
                    kkn_params,
                    enet_params)
  
  return(ml_params)
  
}
