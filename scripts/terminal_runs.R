
# Rscript '~/Documents/Ungaged basins/epa_7q10_ml/scripts/terminal_runs.R'

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
source('scripts/bayes_optim_caret.R')

ptm <- proc.time() 

svmp_bounds <-  list(C = c(0,2),
                     degree = c(1L,2L),
                     scale = c(0,0.05))

svmp_params <- bayes_optim_caret(model_data,'svmPoly',svmp_bounds,iter=10,acq = "ucb")

time <- proc.time() - ptm 

print(time)

write.csv(svmp_params$History,"data/svmp_params.csv",row.names = F)

svmp <- read.csv("data/svmp_params.csv")

seed <- svmp %>%
  mutate(iter_type = ifelse(Round>50,"bayes opt","random seed"),
         degree = as.character(degree)) %>%
  filter(iter_type=="random seed")

opt <- svmp %>%
  mutate(iter_type = ifelse(Round>50,"bayes opt","random seed")) %>%
  filter(iter_type=="bayes opt")

ggplot() +
  geom_segment(data=opt,
               aes(x=C,y=scale,
                   xend=c(tail(C, n=-1), NA),
                   yend=c(tail(scale, n=-1), NA),
                   color=Value),
               arrow=arrow(length=unit(0.2,"cm"))) +
  geom_point(data=seed,aes(x=C,y=scale,size=degree,fill=Value),shape=21) +
  geom_text(data=opt,aes(x=C,y=scale,label=Round-50)) +
  theme_bw() +
  scale_fill_viridis() +
  scale_color_viridis(guide=F)

