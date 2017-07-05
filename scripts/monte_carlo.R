
monte_carlo <- function(model_data) {
  
  input <- menu(c("yes", "no"), 
                title= "The full Monte-Carlo presented in the paper will take over 5 days to run, so we only included a fully functioning minimal example. We also included commented lines of scripts that can be uncommented if a user wants to run the whole thing. If this doesn't make sense, email scworland@usgs.gov. If I do not reply, you are on your own! Do you wish to continue?")
  
  if(input==2){
    stop("Exiting function, user does not wish to proceed.")
  }
  
  cat("
          _
         /(|
        (  :
       __\\  \\  _____
     (____)  `|
    (____)|   |
     (____).__|
      (___)__.|_____Here we go --- ")
  
  # if OS is unix, register 4 cores for parallel processing
  if(.Platform$OS.type == "unix") { # if unix
    if(!require(doMC)){             # check if doMC is installed...
      install.packages("doMC")      # ...if not install it
    }
    library(doMC)                   # load the library
    registerDoMC(cores = 4)         # register cores
  }
  
  library(caret)
  
  ## group model_data by the quantiles in the response variable.
  ## This ensures that when we sample a fraction of rows from 
  ## model_data_sub we will sample a fraction of small streams
  ## and large streams.
  model_data_grouped <- model_data %>%
    mutate(row_num = 1:nrow(.)) %>%
    mutate(quant_class = ifelse(y>quantile(model_data$y,0.90), 1, 0)) %>%
    select(y,quant_class,lat_gage:row_num) %>%
    group_by(quant_class)
  
  ## leave one out CV
  ctrl <- trainControl(method = "LOOCV", returnData=FALSE)
  
  ## vector of fraction of full dataset to sample
  #frac <- seq(0.1,1,0.1)
  frac <- c(0.1,0.2)
  knn_obs_number <- matrix(data=NA,nrow=length(frac),ncol=11)
  null_obs_number <- matrix(data=NA,nrow=length(frac),ncol=11)

  k <- 0 # counter
  n <- length(frac)*ncol(obs_number)-1
  
  for (i in 1:length(frac)) {
    for (j in 1:(ncol(knn_obs_number)-1)){
      
      model_data_frac <- sample_frac(model_data_grouped,frac[i]) 
      rowi <- model_data_frac$row_num
      
      model_data_frac <- model_data_frac %>%
        ungroup() %>%
        select(-quant_class,-row_num)
      
      # null model
      null_hold_preds <- data.frame(obs=model_data_frac$y) %>%
        dplyr::mutate(pred = combn(obs, n()-1, FUN=mean)) %>%
        mutate(pred = (exp(pred)*data_full$drain_sqkm[rowi])-0.001, 
               obs = (exp(obs)*data_full$drain_sqkm[rowi])-0.001)
      
      null_obs_number[i,1] <- length(rowi)
      null_obs_number[i,j+1] <- sqrt(mean((null_hold_preds$obs-null_hold_preds$pred)^2,na.rm=T))
      
      ## knn model
      knn_hold <- train(y~., model_data_frac,
                        method = "knn",
                        tuneLength = 10,
                        trControl = ctrl)
      
      knn_hold_preds <- suppressMessages(inner_join(knn_hold$pred,knn_hold$bestTune)) %>%
        mutate(pred = (exp(pred) * data_full$drain_sqkm[rowi])-0.001,
               obs = (exp(obs) * data_full$drain_sqkm[rowi])-0.001) %>%
        mutate(obs = replace(obs, obs<0.002, 0))
      
      knn_obs_number[i,1] <- length(rowi)
      knn_obs_number[i,j+1] <- sqrt(mean((knn_hold_preds$obs-knn_hold_preds$pred)^2,na.rm=T))
      
      k <- k+1 
      print(paste0(round(k / n * 100), '% completed'))
    }
    
  }
  
  obs_num_list <- list('null'=null_obs_number,
                       'knn'=knn_obs_number)
  
  return(obs_num_list)
}

# obs_num <- data.frame(num=obs_number[,1], rmse=obs_number[,-1]) %>%
#   melt(., id.vars="num") %>%
#   group_by(num) %>%
#   dplyr::summarize(p25 = quantile(value,probs=0.25),
#                    p50 = quantile(value,probs=0.50),
#                    p75 = quantile(value,probs=0.75))
