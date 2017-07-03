
fit_metrics <- function(all_preds,data_full) {
  
  ## custom function to calcualate fit metrics
  fitmets <- function(df,model_name="model",data_full) {
    
    est <- df[,1] # estimated
    obs <- df[,2] # observed
    
    ### convert predictions to unit area before calculating
    ### unit area RMSE
    ft2 <- data_full$drain_sqkm*1.076e+7 # requires 'data_full'
    unit.est <- (df[,1]*60*60*24*365)/ft2
    unit.obs <- (df[,2]*60*60*24*365)/ft2
    
    ### 4 error metrics
    rmse <- sqrt(mean((obs-est)^2,na.rm=T)) # root mean squared error
    unit.rmse <- sqrt(mean((unit.obs-unit.est)^2,na.rm=T)) # unit area RMSE
    mpe <- median(abs((est[obs>0]-obs[obs>0])/obs[obs>0]))*100 # median % error
    nse <- 1-(sum((est-obs)^2)/sum((obs-mean(obs))^2)) # Nash-Sutcliffe coefficient
    
    ### combine metrics and return output
    metrics <- data.frame(rmse,mpe,nse,unit.rmse)
    rownames(metrics) <- model_name
    
    return(metrics)
  }
  
  if(ncol(all_preds)==2) {
    metrics=fitmets(all_preds[,c(2,1)],model_name=names(all_preds)[2],data_full)
    return(metrics)
    }else{
  
  metrics=fitmets(all_preds[,c(2,1)],model_name=names(all_preds)[2],data_full)
  for(i in 3:ncol(all_preds)){
    metrics[i-1,] <- fitmets(all_preds[,c(i,1)],model_name=names(all_preds[i]),data_full)
  }
  
  return(metrics)
  }
}