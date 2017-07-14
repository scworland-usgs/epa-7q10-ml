
make_plots <- function(all_preds,model_error,var_imp_overall,pdp_data) {
  
  all_preds <- all_preds[,-1] # remove staid
  
  # turn warnings off for function
  options(warn = -1)
  
  if(!require(gridExtra)){            
    install.packages("gridExtra")      
  }
  
  library(ggrepel)
  library(viridis)
  library(gridExtra)
  
  ## rmse vs unit rmse (figure 4)
  plot_error <- model_error %>%
    dplyr::mutate(models = c("null","OK","full tobit","roi tobit",
                      "RF","KKNN","GBM","SVMP","SVMG",
                      "M5-cubist", "elastic-net","meta cubist")) %>%
    select(models,rmse,unit.rmse) %>%
    dplyr::mutate(type = c(rep("baseline",4),rep("machine learning",8)))
  
  p4 <- ggplot(plot_error, aes(rmse,unit.rmse,fill=type))  + theme_bw() +
     geom_text_repel(aes(label=models),point.padding = unit(0.3, "lines"),
                    box.padding = unit(0.5, "lines"),segment.color = "grey") +
    geom_point(color="black",shape=21,alpha=1,size=3) +
    scale_fill_viridis(discrete=T,begin=0.95, end=0.15,name="model type: ") +
    labs(x="RMSE (cubic feet/second)",y="Unit Area RMSE (feet/year)") +
    scale_x_continuous(limits = c(20,80),breaks=seq(10,80,10)) +
    scale_y_continuous(limits = c(0.10,0.37),breaks=seq(0.10,0.35,0.05)) + 
    ggtitle("\n\n\n") +
    theme(legend.position=c(0,1), 
          legend.justification=c(0, -0.2),
          legend.direction="horizontal")
  
  ## predicted vs observed (figure 5)
  plot_error <- model_error %>%
    dplyr::mutate(models=row.names(.),
                  type = c(rep("baseline",4),
                           rep("machine learning",8)))
  
  pred_obs_plot <- melt(all_preds, id.vars="obs",variable.name = "models") %>%
    inner_join(plot_error,by="models") %>%
    group_by(models) %>%
    ungroup() %>%
    arrange(rmse) %>%
    dplyr::mutate(models=factor(models,unique(models),ordered=T)) 
  
  p5 <- ggplot(pred_obs_plot,aes(obs,value,fill=type)) + geom_point(size=1.5,alpha=0.7,shape=21) +
    facet_wrap(~models, ncol=4) + #scale_color_viridis() +
    geom_abline(slope=1,intercept=0, color="black", size=0.5,linetype="dashed") + theme_bw(base_size=10) +
    scale_x_sqrt(limits=c(0,1500),breaks=c(0,5,75,250,500,750,1000,1250,1500),
                 labels=c("",5,"",250,"",750,"",1000,"")) + 
    scale_y_sqrt(limits=c(0,1500),breaks=c(0,5,75,250,500,750,1000,1250,1500)) +
    labs(y="predicted (cfs)",x="observed (cfs)") +
    scale_fill_viridis(discrete=T,begin=0.95, end=0.15,name="model type: ") +
    guides(fill = guide_legend(override.aes = list(size=3))) +
    ggtitle("\n\n\n") +
    theme(legend.position=c(0,1), 
          legend.justification=c(0, -0.7),
          legend.direction="horizontal")
  
  # decomposed error (figure 6)
  obs <- all_preds$obs[all_preds$obs>0]
  preds <- all_preds[all_preds$obs>0,-1]
  
  A = function(yhat,y){2*sd(yhat)*sd(y)*(1-cor(yhat,y))}
  B = function(yhat,y){(sd(yhat) - sd(y))^2}
  C = function(yhat,y){(mean(yhat) - mean(y))^2}
  MSE = function(yhat,y){A(yhat,y) + B(yhat,y) + C(yhat,y)}
  
  bv <- data.frame(bias = apply(preds,2,C,obs),
                   variance = apply(preds,2,B,obs),
                   covariance = apply(preds,2,A,obs)) %>%
    mutate(models=row.names(.)) %>%
    arrange(-(bias+variance+covariance)) %>%
    mutate(models=factor(models,models))
  
  bvm <- melt(bv, id.vars="models") 
  
  p6 <- ggplot(bvm, aes(x = models, y = value, fill=variable)) +
    geom_bar(stat='identity') + 
    geom_col(position = position_stack(reverse = TRUE)) +
    coord_flip() +
    scale_fill_viridis(discrete=T,begin=0.15, end=0.95) +
    labs(fill="Error Component",x="", y="MSE") +
    theme_bw() +
    theme(legend.position=c(0.8,0.75),
          legend.background = element_rect(color = "grey45"),
          legend.key = element_rect(color = "white")) 
  
  # variable importance and partial dependence (figure 8)
  p8.1 <- ggplot(var_imp_overall,aes(reorder(variable, imp),imp)) + 
    geom_bar(stat="identity",width=0.05,color="black") + 
    geom_point(fill="white",size=2,color="black",shape=21) + 
    labs(x=NULL,y="Variable Importance") +
    ggtitle("\n\n\n") +
    theme_bw() +
    coord_flip()
  
  pdp_data2 <- left_join(pdp_data,var_imp_overall, by="variable") 
  
  p8.2 <- ggplot(pdp_data2, aes(x,value,color=model)) + 
    facet_wrap(~reorder(variable,-imp), scales="free", ncol=2) +
    geom_line() + theme_bw() + 
    labs(x="scaled predictor value",y="predicted ln(7Q10/area)") +
    scale_color_viridis(discrete=T,begin=0.85, end=0.05,name="model: ") +
    ggtitle("\n\n\n") +
    theme(axis.ticks=element_blank(), 
          axis.text=element_blank(),
          legend.position=c(0,1), 
          legend.justification=c(0, -0.7),
          legend.direction="horizontal") 
  
  # use this for paper
  # p8 <- grid.arrange(p8.1,p8.2,ncol=2)
  
  plots <- list('rmse_vs_unitrmse'=p4,
                'pred_vs_obs'=p5,
                'error_decomp'=p6,
                'var_imp'=p8.1,
                'partial_dep'=p8.2)
  
  return(plots)
  
  # turn warnings back on
  options(warn = 0)
}