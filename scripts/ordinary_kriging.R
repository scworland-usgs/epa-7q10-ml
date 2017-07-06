
# Kriging function 
# written by William Farmer, September 2015 
# modified by Scott Worland, June 2017
# Applies ordinary kriging with custom constraints.

ordinary_kriging <- function(netdata, netlatlon, tarlatlon, numbins, CovMod,
                  FixNug, FixKap, neighbors, maxrange) {
  
  library(geoR)
  
  netdata[!is.finite(netdata)] <- NA
  NDX <- !is.na(netdata)
  EmpVario <- variog(coords = netlatlon[NDX, ], data = netdata[NDX],
                     breaks = seq(0, maxrange,len = numbins), messages = F)
  iMod <- variofit(EmpVario, cov.model = CovMod, messages = F,
                   fix.nugget = FixNug, fix.kappa = FixKap)
  VarPar <- c(iMod$nugget, iMod$cov.pars, iMod$cov.model, iMod$kappa,
              iMod$value, iMod$beta.ols, iMod$practicalRange, iMod$max.dist,
              EmpVario$n.data, neighbors)
  if (iMod$nugget < 0) {
    iMod$nugget <- 0
  }
  iMod.pred <- krige.control(obj.model = iMod)
  iPred <- krige.conv(coords = netlatlon[NDX, ], data = netdata[NDX],
                      locations = tarlatlon, krige = iMod.pred, output = list(messages = F))
  if (!is.na(neighbors)) {
    iPred <- ksline(coords = netlatlon[NDX, ], data = netdata[NDX],
                    locations = tarlatlon, nwin = neighbors, cov.model = iMod.pred$cov.model,
                    cov.pars = iMod.pred$cov.pars, kappa = iMod.pred$kappa,
                    nugget = iMod.pred$nugget, message = F)
  }
  Pred <- iPred$predict
  kVar <- iPred$krige.var
  result <- list(VarPar = VarPar, Pred = Pred, kVar = kVar)
  return(result)
}

# Below is a script to tune the hyperparameters of the OK model

# ## load libraries needed for main.R script
# library(dplyr)
# 
# ## set working directory
# setwd("~/Documents/Ungaged basins/epa_7q10_ml")
# 
# ## Load csv file: note, data should be in a "data" folder in working directory
# data_full <- read.csv("data/lowflow_sc_ga_al_gagesII_2015.csv",header=T,na.strings = "-999") %>%
#   setNames(tolower(names(.))) # make column names lower case
# 
# data_ok <- dplyr::select(data_full,y7q10,drain_sqkm,lat_gage_utm,lng_gage_utm) %>%
#   dplyr::mutate(y = log((y7q10 + 0.001) * 2.589975 / drain_sqkm))
# 
# ok_grid <- expand.grid(CovMod = c("matern", "exponential", "gaussian", 
#                                   "spherical", "circular", "cubic", 
#                                   "wave", "cauchy", "gneiting", 
#                                   "pure.nugget"),
#                        numbins = c(5,10,20),
#                        stringsAsFactors = F)
# 
# ok_preds <- rmse <- numeric()
# for(j in 1:nrow(ok_grid)) {
#   for(i in 1:nrow(data_ok)){
#     train_ok <- data_ok[-i, ]
#     test_ok <- data_ok[i, ]
#     
#     ok_fit <- ordinary_kriging(train_ok$y, train_ok[,3:4], test_ok[,3:4], numbins=ok_grid$numbins[j],
#                                CovMod=ok_grid$CovMod[j], FixNug=F, FixKap=T, neighbors=NA)
#     
#     ok_preds[i] <- (exp(ok_fit$Pred) * test_ok$drain_sqkm / 2.589975) - 0.001
#   }
#   rmse[j] <- sqrt(mean((data_full$y-ok_preds)^2,na.rm=T))
# }
# 
# ok_optim <- ok_grid %>%
#   dplyr::mutate(rmse=rmse) %>%
#   arrange(rmse)
# 
# print(ok_optim)
# 
# write.csv(ok_optim, "data/ok_optim.csv")
