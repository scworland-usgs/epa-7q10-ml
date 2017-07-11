

bayes_optim_caret <- function(data,method,bounds,iter=30,kappa=0.5){
  
  library(caret) 
  library(rBayesianOptimization)
  
  # if OS is unix, register 4 cores for parallel processing
  if(.Platform$OS.type == "unix") { # if unix
    if(!require(doMC)){             # check if doMC is installed...
      install.packages("doMC")      # ...if not install it
    }
    library(doMC)                   # load the library
    registerDoMC(cores = 4)         # register cores
  }
  
  # find parameter names of method
  params <- as.character(modelLookup(method)$parameter)
  params <- params[order(params)]
  
  # Intermediate function which creates a function that can be
  # resampled by the BayesianOptimzation function
    sample_fun <- function(params) {
      funargs <- paste(params, collapse=', ')
      dfargs <- paste0(params, '=', params, collapse=', ')
      funstr <- paste0(
        sprintf("function(%s) {", funargs),
        sprintf("
      param_grid <- data.frame(%s)", dfargs),
        "
      mod <- train(y~., data=data,
      trControl=trainControl(method = 'LOOCV', search = 'grid'),
      metric = 'RMSE',
      ",
        sprintf("method='%s'", method),",
      tuneGrid = param_grid)
      list(Score = -getTrainPerf(mod)[, 'TrainRMSE'], Pred = 0)",
        "}")
      eval(parse(text=funstr))
    }
  
  # run bayesian optimization search for parameters
  bayes_opt_search <- BayesianOptimization(sample_fun(params),
                                           bounds = bounds,
                                           init_points = 30,
                                           n_iter = iter,
                                           acq = 'ucb', 
                                           kappa = kappa, 
                                           kernel = list(type="matern",nu=5/2),
                                           verbose = TRUE)
  
  
  return(bayes_opt_search)
  
}












