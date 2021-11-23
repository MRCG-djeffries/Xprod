read_and_plot=function(){
  # read the outputs and plots
  icer_mat=matrix(data=0,nrow = 100,ncol = 100)
  mort_mat=icer_mat
  inci_mat=icer_mat
  both_mat=icer_mat
  for ( i in 1 : 100){
    X=readRDS(paste0("HPCruns/run1/out_",i,".rds"))
    #rows are current, cols are former
   mort_mat[i,]=X$M
   inci_mat[i,]=X$I
   icer_mat[i,]=X$C
   both_mat[i,]=X$B
  }
  
  p=plotoutmat(inci_mat-2000,"All current","All former","ICER")
  return(p)
}

plotoutmat = function(Z,labx,laby,legt,x=1:100,y=1:100){
  library(ggplot2)
  library(hrbrthemes)
  library(plotly)
  library(reshape2)
  library(scales)
  

  data = expand.grid(X=x, Y=y)
  data$Z = melt(Z)$value
  
  # new column: text for tooltip:
  # data <- data %>%
  #   mutate(text = paste0("x: ", x, "\n", "y: ", y, "\n", "Value: ",round(Z,2), "\n", "What else?"))
  
  # classic ggplot, with text in aes
  p <- ggplot(data, aes(X, Y, fill= Z)) + 
    geom_tile() +
    #theme_ipsum()+
    theme_bw()+
    labs(x=labx)+
    #xlab(labx)+
    ylab(laby) +
    labs(fill = legt) +
    #scale_fill_continuous(labels=comma)+
    theme(legend.key.height = unit(3, "cm"))+ # legend.position = "bottom", 
  scale_fill_viridis_c(option = "magma",labels=comma,na.value = "transparent")
  return(p)
}
