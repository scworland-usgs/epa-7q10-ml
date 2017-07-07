
gather_cv_predictions <- function(model_data,data_full) {
  
  library(dplyr)
  library(reshape2)
  library(randomForest)
  library(kknn)
  library(xgboost)
  library(kernlab)
  library(Cubist)
  library(glmnet)
  source("scripts/ordinary_kriging.R")
  
  # subset data for ordinary kriging
  data_ok <- dplyr::select(data_full,y7q10,drain_sqkm,lat_gage_utm,lng_gage_utm) %>%
    mutate(y = log((y7q10 + 0.001) * 2.589975 / drain_sqkm))
  
  ## pre-allocate prediction matrix
  ml_preds <- matrix(data=NA, ncol=8, nrow=nrow(model_data))
  colnames(ml_preds) <- c("rf","knn","gbm","svmp","svmg","cubist","enet","obs")
  ok_preds <- numeric()
  
  ## This for-loop makes leave-one-out cross validated predictions using the optimal
  ## hyperparameters found in the 'model_training.R' file. There are much sexier ways
  ## to do this, but this is the most transparent I believe. It might take a couple of 
  ## minutes to run, as it has to train the models on new data each time.
  for (i in 1:nrow(model_data)){
    # select nrows-1 from model data
    train <- model_data[-i,]
    test <- model_data[i,]
    
    #fit model models with formula
    set.seed(1)
    rf_fit <- randomForest(y~., data=train, ntree=500, mtry=116) #*
    
    set.seed(1)
    knn_fit <- kknn(y~., train, test, k = 5, distance = 0.25, kernel = "triangular")
    
    set.seed(1)
    gbm_fit <- xgboost(data=as.matrix(train), #*
                       label=train$y,
                       verbose=0,
                       nrounds=56, 
                       params=list(max_depth=16,
                                   eta=0.1883,
                                   gamma=5.1394,
                                   colsample_bytree=0.8558,
                                   min_child_weight=14,
                                   subsample=0.7868))
    
    set.seed(1)
    svmp_fit <- ksvm(y~., data=train, C=1, kernel="polydot",
                     kpar=list(degree=2, scale=0.0025))
    
    set.seed(1)
    svmg_fit <- ksvm(y~., data=train, C=1.51, kernel="rbfdot", #*
                     kpar=list(sigma= 0.0044))
    
    
    # fit models with design matrix and response vector
    train2 <- as.matrix(train[,-1])
    y2 <- as.numeric(train[,1])
    test2 <- as.matrix(test[,-1])
    
    cubist_fit <- cubist(train2, y2, committees=50, neighbors=8, #*
                         control=cubistControl(seed=1))
    
    enet_fit <- glmnet(train2,y2,alpha=0.0) #*
    
    # fit ordinary kriging
    train_ok <- data_ok[-i, ]
    test_ok <- data_ok[i, ]
    
    maxrange <- summary(dist(data_ok[, c("lat_gage_utm", "lng_gage_utm")],
                             diag = TRUE, upper = TRUE))[6]

    ok_fit <- ordinary_kriging(train_ok$y, train_ok[,3:4], test_ok[,3:4], numbins=10,
                             CovMod="spherical", FixNug=F, FixKap=T, neighbors=NA,
                             maxrange=maxrange)
    
    ok_preds[i] <- (exp(ok_fit$Pred) * test_ok$drain_sqkm / 2.589975) - 0.001

    # test model on left-out observation
    ml_preds[i,1] <- (exp(predict(rf_fit, test)) * data_full$drain_sqkm[i]) - 0.001
    ml_preds[i,2] <- (exp(knn_fit$fitted.values) * data_full$drain_sqkm[i]) - 0.001
    ml_preds[i,3] <- (exp(predict(gbm_fit, as.matrix(test))) * data_full$drain_sqkm[i]) - 0.001
    ml_preds[i,4] <- (exp(predict(svmp_fit, test)) * data_full$drain_sqkm[i]) - 0.001
    ml_preds[i,5] <- (exp(predict(svmg_fit, test)) * data_full$drain_sqkm[i]) - 0.001
    ml_preds[i,6] <- (exp(predict(cubist_fit, test2)) * data_full$drain_sqkm[i]) - 0.001
    ml_preds[i,7] <- (exp(predict(enet_fit, test2, s=1.3741)) * data_full$drain_sqkm[i]) - 0.001
    
    # add observations
    ml_preds[i,8] <- data_full$y7q10[i]
    
    print(paste0("ML and kriging: ",round(i/nrow(model_data)*100,2),"%"))
  }
  
  
  # the lcr functions return the loo-cv predictions
  source("scripts/lcr_tobit.R")
  source("scripts/roi_lcr_tobit.R")
  
  library(PUBAD)
  expVars <- data.frame(gages = data_full$staid) %>%
    getBasinChar()
  
  ## create model data using clean basin chars
  lcr_model_data <- expVars$cleanBCs %>% # start with basin chars
    setNames(tolower(names(.))) %>% # make sure colname are lower case
    mutate(class = as.integer(as.factor(class))) %>% # change class to integer
    mutate(drain_sqkm = log(drain_sqkm)) %>%
    mutate_each(funs(as.numeric)) %>% # make everything numeric
    scale() %>% # convert to z-score: (x-mu)/sigma
    as.data.frame() %>% # make data frame
    mutate(y=data_full$y7q10) %>%
    mutate(y=replace(y, y<0.01, 0.001)) %>%
    mutate(y=log(y)) %>%
    select(y, class:aspect_eastness)
  
  # left censored regression
  lcr_preds <- lcr.tobit(lcr_model_data, 
                         subset_method="forward",
                         subset_number=8, 
                         thold=log(0.001))
  
  # region of influence left censored regression
  roi_lcr_preds <- roi.lcr.tobit(lcr_model_data, 
                                 neighbors = 205,
                                 subset_method="backward",
                                 subset_number=8)
  
  # null predictions
  null_preds <- data.frame(obs=data_full$y7q10/data_full$drain_sqkm) %>%
    dplyr::mutate(pred = combn(obs, n()-1, FUN=mean),
                  pred = pred * data_full$drain_sqkm,
                  obs = obs * data_full$drain_sqkm)
  
  cv_preds <- data.frame(ml_preds) 
  cv_preds$ok <- ok_preds
  cv_preds$full_tobit <- lcr_preds$pred
  cv_preds$roi_tobit <- roi_lcr_preds$pred
  cv_preds$null <- null_preds$pred
  
  cv_preds <- select(cv_preds,obs,null,ok,full_tobit,roi_tobit,rf:enet)
  
  return(cv_preds)
}





