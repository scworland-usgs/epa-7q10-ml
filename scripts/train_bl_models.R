
train_bl_models <- function(data_full) {
  
  input <- menu(c("yes", "no"), title="Are you sure you want to run this function? It will take > 5 minutes.")
  
  if(input==2){
    stop("User chose to exit function because 5 minutes is just to long to wait...")
  }
  
  library(PUBAD)
  expVars <- data.frame(gages = data_full$staid) %>%
    getBasinChar()
  
  ## create model data using clean basin chars
  model_data <- expVars$cleanBCs %>% # start with basin chars
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
  
  # Full tobit left censored regression --------------------------
  # source left censored regression function
  source("scripts/lcr_tobit.R")
  
  # tune parameter grid for lcr_tobit
  lcr_grid <- expand.grid(subset_method=c("forward","backward"),
                          subset_number=c(2,4,8,10,12))
  
  lcr_tune_error <- numeric()
  for (i in 1:nrow(lcr_grid)){
    
    fit <- lcr.tobit(model_data, 
                     subset_method=as.character(lcr_grid[i,1]),
                     subset_number=lcr_grid[i,2], 
                     thold=log(0.001))
    
    lcr_tune_error[i] <- sqrt(mean((fit$obs-fit$pred)^2,na.rm=T))
    
    # print progress
    print(paste0("lcr train progress = ",round(i/nrow(lcr_grid),3)*100,"%"))
    
  }
  
  # add rmse to tuning parameters
  lcr_param_error <- lcr_grid %>%
    mutate(rmse = lcr_tune_error) %>% 
    arrange(rmse)
  
  # create plot of tuning parameters and rmse
  lcr_plot <- ggplot(lcr_param_error,aes(x=subset_number,y=rmse,color=subset_method)) + 
    geom_line() + 
    geom_point()
  
  # Region of influence censored regression -------------------------
  
  # source ROI censored regression function
  source("scripts/roi_lcr_tobit.R")
  
  # tune parameter grid for lcr_tobit
  roi_grid <- expand.grid(neighbors=seq(25,200,25),
                          subset_method=c("forward","backward"),
                          subset_number=c(2,4,8,10,12))
  
  roi_tune_error <- numeric()
  for (i in 1:nrow(roi_grid)){
    
    fit <- roi.lcr.tobit(model_data, 
                         neighbors=roi_grid[i,1],
                         subset_method=as.character(roi_grid[i,2]),
                         subset_number=roi_grid[i,3], 
                         thold=log(0.001))
    
    roi_tune_error[i] <- sqrt(mean((fit$obs-fit$pred)^2,na.rm=T))
    
    # print progress
    print(paste0("ROI lcr train progress = ",round(i/nrow(roi_grid),3)*100,"%"))
    
  }
  
  # add rmse to tuning parameters
  roi_param_error <- roi_grid %>%
    mutate(rmse = roi_tune_error,
           subset_number=as.character(subset_number)) %>%
    arrange(rmse)
  
  # create plot of tuning parameters and rmse
  roi_plot <- ggplot(roi_param_error,aes(x=neighbors,y=rmse,color=subset_number)) + 
    facet_wrap(~subset_method, ncol=1, scales="free_y") +
    scale_y_sqrt() +
    geom_line() + 
    geom_point()
  
  # Return trained objects in a list
  bl_train_list <- list("lcr_tune" = lcr_param_error, 
                        "lcr_tune_plot" = lcr_plot,
                        "roi_lcr_tune" = roi_param_error,
                        "roi_lcr_tune_plot" = roi_plot)
}







