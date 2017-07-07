
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
  names(cv_preds)[1] <- "y"
  
  ## stacked model (described by figure 3 in paper)
  ## Stacked regression combines the LOO-CV predictions (and mean and median) from all 
  ## the machine learning models using a level-1 M5-cubist model
  
  cubist_bounds <-  list(committees = c(1L,100L), 
                         neighbors = c(0L,9L))
  
  stack_train <- bayes_optim_caret(cv_preds,'cubist',cubist_bounds,iter=5,acq = "ei")
  
  ## tuned hyperparameters
  params <- data.frame(stack_train$Best_Par) %>% 
    dplyr::mutate(name=row.names(.)) %>%
    dplyr::select(name,value=stack_train.Best_Par)
  
  print(params)
  
  param_list <- split(params$value, params$name)
  
  ## calculate loo-cv cross validate predictions using for-loop like above. The optimimal
  ### hyperparameters of committees=10, and neighbors=2 were found above.
  stack_preds <- numeric() # preallocate numeric prediction vector
  for (i in 1:nrow(cv_preds)){
    
    # select nrows-1 from model data
    train <- as.matrix(cv_preds[-i,-1])
    y_train <- as.numeric(cv_preds[-i,1])
    test <- as.matrix(cv_preds[i,-1])
    
    cubist_stack <- cubist(train, y_train, committees=param_list[[1]], 
                           neighbors=param_list[[2]], #*
                           control=cubistControl(seed=1))
    
    # predict left out observations
    stack_preds[i] <- predict(cubist_stack, test)
    
    if((i %% 2) == 0){
      print(paste0(round(i/nrow(model_data)*100,2),"%"))
    }
    
  }
  
  # add stacked predictions to to ml_preds. meta_cubist==stacked model
  all_preds <- select(cv_preds, -med,-mean)
  all_preds$meta_model <- stack_preds
  
  return(all_preds)
  
}

