
var_imp <- function(model_data) {
  
  library(caret)
  
  ## leave one out CV
  ctrl <- trainControl(method = "LOOCV")
  
  ## random forest
  rf.grid <- data.frame(mtry=90)
  rf.fit <- train(y~., data=model_data,
                  trControl=ctrl,
                  tuneGrid=rf.grid,
                  importance=T,
                  ntree=200,
                  method='rf')
  
  rf.imp <- varImp(rf.fit)$importance %>% 
    mutate(names=row.names(.)) %>%
    arrange(-Overall)
  
  print("completed random forest (model 1/6)")
  
  ## GBM
  gbm.grid <- data.frame(n.trees=100, interaction.depth=5, 
                         n.minobsinnode = 10, shrinkage=0.1)
  
  gbm.fit <- train(y~., data=model_data,
                   trControl=ctrl,
                   tuneGrid=gbm.grid,
                   method='gbm',
                   distribution="gaussian",
                   verbose=FALSE)
  
  gbm.imp <- varImp(gbm.fit)$importance %>% 
    mutate(names=row.names(.)) %>%
    arrange(-Overall)
  
  print("completed gradient boosting machine (model 2/6)")
  
  ## kknn fit
  knn.grid <-  expand.grid(kmax=7, distance = 0.25,
                           kernel = "optimal")
  
  knn.fit <- train(y~., data=model_data,
                   method = "kknn",
                   tuneGrid = knn.grid,
                   k=7,
                   trControl = ctrl)
  
  kknn.imp <- varImp(knn.fit)$importance %>% 
    mutate(names=row.names(.)) %>%
    arrange(-Overall)
  
  print("completed k-nearest neighbors (model 3/6)")
  
  ## elastic net
  glmnet.grid <-  expand.grid(alpha=0.8,lambda=0.193)
  glmnet.fit <- train(y~., data=model_data,
                      tuneGrid=glmnet.grid,
                      method='glmnet')
  
  glmnet.imp <- varImp(glmnet.fit)$importance %>% 
    mutate(names=row.names(.)) %>%
    arrange(-Overall)
  
  print("completed elastic net (model 4/6)")
  
  ## cubist
  cubist.grid <-  expand.grid(committees=10, 
                              neighbors=9)
  
  cubist.fit <- train(y~., data=model_data,
                      trControl=ctrl,
                      tuneGrid=cubist.grid,
                      method='cubist')
  
  cubist.imp <- varImp(cubist.fit)$importance %>% 
    mutate(names=row.names(.)) %>%
    arrange(-Overall)
  
  print("completed cubist (model 5/6)")
  
  ## svm
  svm.grid <-  expand.grid(degree = 2,
                           scale = 0.0025,
                           C=1)
  
  svm.fit <- train(y~., data=model_data,
                   trControl=ctrl,
                   tuneGrid = svm.grid,
                   method='svmPoly')
  
  svm.imp <- varImp(svm.fit)$importance %>% 
    mutate(names=row.names(.)) %>%
    arrange(-Overall)
  
  print("completed support vector machine (model 6/6)")
  
  varImp_comb <- list(rf=rf.imp,
                      gbm=gbm.imp,
                      kknn=kknn.imp,
                      glmnet=glmnet.imp,
                      cubist=cubist.imp,
                      svm=svm.imp)
  
  print("combining everthing, should wrap up soon...")
  
  varImp_values <- data.frame(lapply(varImp_comb, "[[", "Overall"))[1:3,] %>%
    mutate(id = 1:3) %>% 
    melt(., id.vars="id",variable.name="model",value.name="overall")
  
  varImp_overall <- data.frame(lapply(varImp_comb, "[[", "names"))[1:3,] %>%
    mutate(id = 1:3) %>% 
    melt(., id.vars="id",variable.name="model",value.name="variable") %>%
    mutate(overall=varImp_values$overall) %>%
    group_by(variable) %>%
    dplyr::summarize(imp = sum(overall)) %>%
    mutate(imp=imp/max(imp)) %>%
    arrange(-imp)
  
  return(varImp_overall)
  
}