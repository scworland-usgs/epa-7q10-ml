
stack_models <- function(cv_preds) {
  
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
  
  ## add median and mean for stacked model
  cv_preds$med <-apply(cv_preds[,2:11],1,median) 
  cv_preds$mean <-apply(cv_preds[,2:11],1,mean) 
  
  ## stacked model (described by figure 3 in paper)
  ## Stacked regression combines the LOO-CV predictions (and mean and median) from all 
  ## the machine learning models using a level-1 M5-cubist model
  
  gbm_bounds <- list(nrounds = c(1L,1000L),
                     max_depth = c(1L,20L),
                     eta = c(0,1),
                     gamma=c(0,100),
                     colsample_bytree=c(0.1,1),
                     min_child_weight=c(0L,20L),
                     subsample=c(0,1))
  
  ## Bayesian optimization 
  stack_train <- bayes_optim_caret(cv_preds,'xgbTree',gbm_bounds,iter=3)
  
  ## tuned hyperparameters
  param_list_df <- data.frame(stack_train$Best_Par) %>% 
    mutate(name=row.names(.))
  
  names(param_list)[1] <- "value"
  
  print(param_list_df)
  
  param_list <- split(param_list_df $value, param_list_df$name)
  
  
  ## calculate loo-cv cross validate predictions using for-loop like above. The optimimal
  ### hyperparameters of committees=10, and neighbors=2 were found above.
  stack_preds <- numeric() # preallocate numeric prediction vector
  for (i in 1:nrow(cv_preds)){
    
    # select nrows-1 from model data
    train <- cv_preds[-i,]
    test <- cv_preds[i,]
    
    set.seed(1)
    gbm_stack <- xgboost(data=as.matrix(train), #*
                       label=train$y,
                       verbose=0,
                       nrounds=56, 
                       params=param_list)
    
    # predict left out observations
    stack_preds[i] <- predict(stack_cubist, test[,-1])
  }
  
  # add stacked predictions to to ml_preds. meta_cubist==stacked model
  all_preds <- select(cv_preds, -med,-mean)
  all_preds$meta_cubist <- stack_preds
  
  return(all_preds)
  
}