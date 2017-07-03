
# region of influence left censored regression function
roi.lcr.tobit <- function(df,neighbors=50,subset_method="forward",subset_number=5,thold=log(0.001)){
  
  source("scripts/region_of_influence.R")
  
  preds <- numeric()
  for (ii in 1:nrow(df)){
    # select nrows-1 from model data
    train <- df[-ii,]
    
    # choose best subset using training data
    regfit <- regsubsets(y~.,data=train,nvmax=subset_number,method=subset_method)
    coefi <- coef(regfit,id=subset_number)
    
    # find the ROI using raw predictors of ALL observations
    nn <- roi(data_full[,names(coefi)[2:(subset_number+1)]],neighbors)
    
    # subset training observations using ROI, and predictors using best subsets
    model_data_sub <- train[(nn[ii,]-1),c("y",names(coefi)[2:(subset_number+1)])]
    
    # build model using training data and subset
    #cr.fit <- censReg(as.lcens(y, log(0.001)) ~ ., data = model.data.sub)
    cr_fit <- tobit(y~.,data=model_data_sub, left = thold)
    
    # test model on left-out observation
    test <- df[ii,c("y",names(coefi)[2:(subset_number+1)])]
    preds[ii] <- predict(cr_fit, test)
  }
  
  roi_lcr_preds <- data.frame(pred = preds, obs = df$y)  %>%
    mutate(pred = exp(pred) + 0.001,
           obs = exp(obs) + 0.001) %>%
    mutate(obs = replace(obs, obs<0.0025, 0))
  
  return(roi_lcr_preds)
  
}