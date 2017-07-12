
# main.R is the main R script associated with epa-7q10-ml. It contains the basic
# set up, and then runs different parts of the analysis using function calls. 
#------------------------------------------------------------------------------

# SETUP AND DATA PREPARATION

## Install all packages required for entire analysis. The libraries needed for 
## each function are loaded in the function script. 
install.packages(c("dplyr","reshape2","randomForest","kknn","caret",
                   "xgboost","kernlab","Cubist","glmnet","devtools","AER",
                   "leaps","doMC","ICEbox","geoR"))

## install PUBAD package off of github
devtools::install_github("wfarmer-usgs/PUBAD")

## load libraries needed for main.R script
library(dplyr); library(PUBAD)

## set working directory
setwd("~/Documents/Ungaged basins/epa_7q10_ml")

## Load csv file: note, data should be in a "data" folder in working directory
data_full <- read.csv("data/lowflow_sc_ga_al_gagesII_2015.csv",header=T,na.strings = "-999") %>%
  setNames(tolower(names(.))) # make column names lower case

## Transform 7Q10 response variable
area <- data_full$drain_sqkm

# have to add 0.001 because some 7Q10's are zero and in order to take the 
# log of the transform we add 0.001
ln7q10_da <- log((data_full$y7q10+0.001)/area)

## use functions from PUBAD to cull covariates
expVars <- data.frame(gages = data_full$staid) %>%
  getBasinChar()

## create model data using clean basin chars
model_data <- expVars$cleanBCs %>% # start with basin chars
  mutate(CLASS = as.integer(as.factor(CLASS))) %>% # change class to integer
  mutate_each(funs(as.numeric)) %>% # make everything numeric
  scale() %>% # convert to z-score: (x-mu)/sigma
  as.data.frame() %>% # make data frame
  setNames(tolower(names(.))) %>% # make sure colname are lower case
  mutate(y=ln7q10_da) %>% # add the transformed response variable from above
  select(y, class,lat_gage:aspect_eastness) # reorder column positions

#------------------------------------------------------------------------------

# TRAIN BASELINE MODELS 
source("scripts/train_bl_models.R")

## The 'train_bl_models.R' script contains the train_bl_models() R function.
## the function takes only one inputs, data_full, and finds the optimal 
## hyperparameters for two censored regression models and one kriging 
## model. The unit area null model does not have parameters and the predictions are
## calculated in the 'gather_cv_predictions()' function. The Kriging model also uses 
## fixed parameters and is not fit in the train_bl_models() function. It is actually 
## possible to go ahead and load the predictions with the optimal hyperparameters now, 
## but for clarity, that happens at a later step. The output is a list of tuning 
## parameters and associated RMSE values for both models.

bl_train <- train_bl_models(data_full)

bl_train$lcr_tune
bl_train$roi_lcr_tune

#------------------------------------------------------------------------------

# TRAIN MACHINE LEARNING MODELS 
source("scripts/train_ml_models.R")

## The 'train_ml_models.R' script contains the train_ml_models() R function.
## The function takes only two inputs, model_data and data_full, and finds the  
## optimal hyperparameters for 8 machine learning models. It is actually
## possible to go ahead and load the predictions with the optimal 
## hyperparameters now, but for clarity, that happens at a later step. 

ml_train <- train_ml_models(model_data,data_full)

#------------------------------------------------------------------------------

# GATHER CROSS VALIDATED PREDICTIONS
source("scripts/gather_cv_predictions.R")

## The 'gather_cv_predictions.R' script contains the gather_cv_predictions()
## R function. The function requires the outputs of the train_bl_models() and
## train_ml_models() functions. The outputs are lists that contain the error 
## associated with various hyperparameters. The gather_cv_predictions() function
## is clunky, and actually unnecessary (the predictions are already calculated 
## during the training step), but it allows a user to verify that the inputs 
## shown in the paper recreate the correct error values. 

#cv_preds <- gather_cv_predictions(model_data,data_full)
cv_preds <- read.csv("data/cv_preds.csv")

#------------------------------------------------------------------------------

# STACKED REGRESSION
source("scripts/stack_models.R")

## The 'stack_models.R' script contains the stack_models() R function. The function
## requires the output of the gather_cv_predictions() function, a logical OS indicator, 
## and the number of cores to use if not a windows machine. It trains a Cubist model 
## using the loo-cv predictions from the machine learning and baseline models. The 
## stack_models() function returns all of the predictions in a single dataframe

#all_preds <- stack_models(cv_preds)
all_preds <- read.csv("data/all_preds.csv")

#------------------------------------------------------------------------------

# GOODNESS OF FIT MEASURES
source("scripts/fit_metrics.R")

## The 'fit_metrics.R' script contains the fit_metrics() R function and calculates
## the RMSE, unit area RMSE, median % error, and Nash-Sutcliffe coefficient. The
## input is the cross validated predictions from all the models and data_full

model_error <- fit_metrics(all_preds,data_full)

#------------------------------------------------------------------------------

# VARIABLE IMPORTANCE AND PARTIAL DEPENDENCE 
source("scripts/var_imp.R")
source("scripts/partial_dependence.R")

## The 'var_imp.R' script contains the var_imp() R function and requires model_data
## as an input. The 'partial_dependence.R' script contains the partial_dependence()
## R function and requires both model_data and var_imp_overall to calculate the 
## partial dependence of the top variables from var_imp and the SVMP, RF, GBM, and 
## M5-cubist models

var_imp_overall <- var_imp(model_data)
pdp_data <- partial_dependence(model_data,var_imp_overall)

#------------------------------------------------------------------------------

# GENERATE MAJOR PLOTS FROM PAPER
source("scripts/make_plots.R")

## The 'make_plots.R' R script contains the make_plots() R function that 
## recreates plots from the paper

plots <- make_plots(all_preds,model_error,var_imp_overall,pdp_data)

## access the plots
plots$rmse_vs_unitrmse
plots$pred_vs_obs
plots$error_decomp
plots$var_imp
plots$partial_dep
