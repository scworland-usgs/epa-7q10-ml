
# left censored regression function
lcr.tobit <- function(df,subset_method="forward",subset_number=5,thold=log(0.001)){
  
  library(AER); library(leaps)
  
  preds <- numeric()
  for (i in 1:nrow(df)){
    # select nrows-1 from model data
    train <- df[-i,]
    
    # choose best subset using training data
    regfit <- regsubsets(y~.,data=train,nvmax=subset_number,method=subset_method)
    coefi <- coef(regfit,id=subset_number)
    model_data_sub <- train[,c("y",names(coefi)[2:(subset_number+1)])]
    
    # build model using training data and subset
    cr_fit <- tobit(y~.,data=model_data_sub, left = thold)
    
    # test model on left-out observation
    test <- df[i,c("y",names(coefi)[2:(subset_number+1)])]
    preds[i] <- predict(cr_fit, test)
  }
  
  lcr_preds <- data.frame(pred = preds, obs = df$y)  %>%
    mutate(pred = exp(pred) + 0.001,
           obs = exp(obs) + 0.001) %>%
    mutate(obs = replace(obs, obs < 0.0025, 0))
  
  return(lcr_preds)
  
}
