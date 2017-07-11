
partial_dependence <- function(model_data,var_imp_overall) {
  
  input <- menu(c("yes", "no"), title="Are you sure you want to run this function? It will take > 5-10 minutes.")
  
  if(input==2){
    stop("User chose to exit function because due to runtime length...")
  }
  
  library(dplyr)
  library(reshape2)
  library(randomForest)
  library(kknn)
  library(gbm)
  library(kernlab)
  library(Cubist)
  library(ICEbox)
  
  # generate model fits for select models to create partial dep plot
  ## fit model models with formula
  set.seed(1)
  rf_fit <- randomForest(y~., data=model_data, ntree=200, mtry=90)
  
  set.seed(1)
  knn_fit <- train.kknn(y~., model_data, ks = 5, distance = 0.25, kernel = "triangular")
  
  set.seed(1)
  gbm_fit <- gbm(y~., data=train, distribution="gaussian", n.trees=439,
                 interaction.depth=15, n.minobsinnode = 14, shrinkage=0.0671,
                 verbose=F,keep.data=F)
  
  set.seed(1)
  svmp_fit <- ksvm(y~., data=model_data, C=1, kernel="polydot",
                   kpar=list(degree=2, scale=0.0025))
  
  ## fit models with design matrix and response vector
  X <- model_data[,-1]
  y <- model_data[,1]
  
  cubist_fit <- cubist(X, y, committees=10, neighbors=9, 
                       control=cubistControl(seed=1))
  
  ## list of model fits
  model_fits <- list(cubist=cubist_fit,
                     gbm=gbm_fit,
                     rf=rf_fit,
                     svmp=svmp_fit)
  
  # select variables for partial dependence
  varnames = var_imp_overall$variable
  
  ## calculate partial depdendence. This nested for-loop first selects
  ## on of the variable from varnames (outter loop), and then calculates 
  ## the partial depdence for each model and that variable (inner loop)
  
  # preallocate list
  pdp_combined <- list()
  for (j in 1:length(varnames)) {
    # preallocate matrix
    var_pdp <- matrix(ncol=length(model_fits)+1,
                      nrow=length(unique(model_data[,varnames[j]])), 
                      data=NA)
    
    var_pdp[,1] <- sort(unique(model_data[,varnames[j]]))
    colnames(var_pdp) <- c("x",names(model_fits))
      
      for (i in 1:length(model_fits)){
        
        # gmb requires "n.trees" so is done separately of the other models
        if(class(model_fits[[i]])=="gbm"){
          hold = ice(object=model_fits[[i]], X = X, y = y, predictor = varnames[j], frac_to_build = 1, 
                     predictfcn = function(object, newdata){predict(object, newdata, n.trees=439)})
          
          var_pdp[,i+1] = hold$pdp # extract partial dependence
        }else{
          
          hold = ice(object=model_fits[[i]], X = X, y = y, predictor = varnames[j], frac_to_build = 1, 
                     predictfcn = function(object, newdata){predict(object, newdata)})
          
          var_pdp[,i+1] = hold$pdp # extract partial dependence
        }
      }
    
    # clean up and add to list
    var_pdp_long <- data.frame(var_pdp) %>%
      melt(., id.vars="x",variable.name="model",value.name="value") %>% 
      mutate(variable=varnames[j])
    
    pdp_combined[[j]] <- var_pdp_long
    names(pdp_combined)[[j]] <- varnames[j]
    
  }
  
  pdp_combined_df <- do.call("rbind", pdp_combined) 
  
  return(pdp_combined_df)
}




