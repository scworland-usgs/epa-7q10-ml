
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
  
  ## add median and mean for stacked model
  cv_preds$med <-apply(cv_preds[,2:11],1,median) 
  cv_preds$mean <-apply(cv_preds[,2:11],1,mean) 
  
  ## stacked model (described by figure 3 in paper)
  ## Stacked regression combines the LOO-CV predictions (and mean and median) from all 
  ## the machine learning models using a level-1 M5-cubist model
  
  ### Use grid search to find optimal hyperparameters
  ctrl <- trainControl(method = "LOOCV", search="grid")
  stack_train <- train(obs~., data=cv_preds,
                        trControl=ctrl,
                        tuneLength=10,
                        method='cubist')
  
  ## tuned hyperparameters
  committees = stack_train$bestTune[[1]]
  neighbors = stack_train$bestTune[[2]]
  
  print(paste0("optimal number of committees for stacked model = ", committees))
  print(paste0("optimal number of neighbors for stacked model = ", neighbors))
  
  ## calculate loo-cv cross validate predictions using for-loop like above. The optimimal
  ### hyperparameters of committees=10, and neighbors=2 were found above.
  stack_preds <- numeric() # preallocate numeric prediction vector
  for (i in 1:nrow(cv_preds)){
    
    # select nrows-1 from model data
    train <- cv_preds[-i,]
    test <- cv_preds[i,]
    
    stack_cubist <- cubist(train[,-1], train[,1], committees=10, neighbors=0,
                           control=cubistControl(seed=1))
    
    # predict left out observations
    stack_preds[i] <- predict(stack_cubist, test[,-1])
  }
  
  # add stacked predictions to to ml_preds. meta_cubist==stacked model
  all_preds <- select(cv_preds, -med,-mean)
  all_preds$meta_cubist <- stack_preds
  
  return(all_preds)
  
}