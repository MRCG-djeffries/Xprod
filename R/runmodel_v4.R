runmodel_v4=function(scenario_number){
  library(gridExtra)
  library(cowplot)
  library(ggplot2)
  library(RSQLite)
  # This runs the model
  # Fits the new models ot flat PWID trend and raising PWID trend
  # Updated June 2021
  L=coeffsof_v5()
  param_vals=readRDS(paste0("www/scenario_",scenario_number,"_param_PWID.rds"))#readRDS("www/Jparam_vals_PWIDs.rds")
  #param_vals=readRDS("www/Jparam_vals_PWIDs.rds")
  N=rep(0,720)
  N[1]=560;N[361]=396;N[367]=44
  dum=asode_v4(param_vals,N,L,scenario_number)
  L=cost_qaly_run(dum$X[,73:91],dum$scale00,dum$scale10,3.5)
  #L=NULL
  return(list(p1=dum$p1,p2=dum$p2,p3=dum$p3,p4=dum$p4,p5=dum$p5,scale00=dum$scale00,scale10=dum$scale10,X=dum$X,
              Qdatbase=L))
}

asode_v4=function(param_vals,N,L,scenario_number){
  library(deSolve)
  
  
  init       = N
  parameters = param_vals
  times      = seq(0, 90, by = 1)
  lt=length(times)
  X = ode(y=init, times=times, func=MODEL_US, parms=parameters,method="ode45")
  X=t(X[,2:721])
  # 2007 pop is 2248500
  delta=-param_vals[6]/param_vals[106] # the spontaneous recovery rate
  acuterate=-param_vals[106]
  curr_mort_pwid=param_vals[895:903]
  curr_mort_former=param_vals[904:912]
  TT=readRDS("www/scenario_values.rds")
  prop_HCV_are_PWID = TT$X[scenario_number]/100
  p1=plotstacked_v3(X,L,prop_HCV_are_PWID)
  p2=plotPWID_scaled(X,L,72,2,prop_HCV_are_PWID)
  p3= plot_prev_v4(X,L,72)
  p4= plotincidence_v2_change(X,L,72,delta,acuterate,curr_mort_pwid,curr_mort_former,prop_HCV_are_PWID)
  p5= plotincidence_v2(X,L,72,delta,acuterate,curr_mort_pwid,curr_mort_former,prop_HCV_are_PWID)
  
  # also return the model terms
  #scale00=dum$scale00,scale10=dum$scale10,X=dum$X
  pop2016=2248500*sum(X[361:720,67])/sum(X[361:720,58])
  perof=sum(X[L$chronic10,67])/sum(X[361:540,67])
  scale10_point = perof*pop2016
  scale00_point = prop_HCV_are_PWID*3.5*10^6-scale10_point # 
  scale10=scale10_point/sum(X[L$chronic10,67]); 
  scale00=scale00_point/sum(X[L$chronic00,67]);
  return(list(p1=p1,p2=p2,p3=p3,p4=p4,p5=p5,scale00=scale00,scale10=scale10,X=X))
}


MODEL_US <- function(time, state, parameters) {
  
  Mvec=parameters[1:400]
  M=t(matrix(Mvec,nrow=20,ncol=20))
  Mdashvec=parameters[401:800]
  Mdash=t(matrix(Mdashvec,nrow=20,ncol=20))
  extra_parms_vals=parameters[801:803]
  phiminusvals=parameters[804:808]
  phiplussvals=parameters[809:813]
  Magevec=parameters[814:894]
  age_matrix=t(matrix(Magevec,nrow=9,ncol=9))
  curr_mort_pwid=parameters[895:903]
  curr_mort_former=parameters[904:912]
  death_rate_dc=parameters[913]
  death_rate_hcc=parameters[914]
  death_rate_lt1=parameters[915]
  death_rate_lt2=parameters[916]
  fitType=parameters[917]
  pop_per1=parameters[918]
  pop_per2=parameters[919]
  scale_mult=parameters[920]
  relapse2=parameters[921]
  relapse3=parameters[922]
  
  mort_current = t(matrix(rep(curr_mort_pwid,20),9,20))
  mort_former = t(matrix(rep(curr_mort_former,20),9,20))
  
  Irows = c(6,7,8,9,10,11,12,13,14,15) # rows of infectious PWID
  #death_vec= c(-log(1-death_rate_dc), -log(1-death_rate_hcc), -log(1-death_rate_lt1), -log(1- death_rate_lt2)); #DC,HCC,LT1,LT2
  death_vec= c(death_rate_dc, death_rate_hcc, death_rate_lt1, death_rate_lt2); #DC,HCC,LT1,LT2
  t=time
  N=state
  # percentage PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(360,520,20),nrow=9,ncol=10)
  top10_index = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(540,700,20),nrow=9,ncol=10)
  top11_index =  matrix(t(dum),nrow=90,ncol=1)
  top = sum(N[top10_index])+sum(N[top11_index]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  bot = sum(N[361:length(N)]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  I = top/bot;
  #scaleI=infTrend(t,1) # always allow increase
  if (t<60){
    scaleI=1
  }else{
    scaleI=scale_mult
  }
  # if (t>92){
  #   scaleI=scale_mult-0.05
  # }
  # if (t>93){
  #   scaleI=scale_mult-0.1
  # }
  # if (t>94){
  #   scaleI=scale_mult-0.05
  # }
  # if (t>95){
  #   scaleI=scale_mult-0.1
  # }
  pop_per=popTrend3(t,pop_per1,pop_per2)
  endy=length(phiminusvals)
  phi = scaleI*extra_parms_vals[1]*I
  dum = Mdash[5,5]
  Mdash[phiminusvals[1:(endy-1)]]=-phi
  Mdash[phiminusvals[endy]]=-phi+dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash[phiplussvals]=phi;
  
  X00 = matrix(N[1:(20*9)],20,9)
  X01 = matrix(N[181:(180+20*9)],20,9)
  X10 = matrix(N[361:(360+20*9)],20,9)
  X11 = matrix(N[541:(540+20*9)],20,9)
  
  extra_parms_vals_vec=c(extra_parms_vals[2],relapse2,rep(relapse3,7))
  extra_parms_vals_mat = t(matrix(rep(extra_parms_vals_vec,20),ncol=20))
  
  # with deaths set as zero - this should be closed
  # age movement also set as zero
  # there are treatment so no failed treatment
  # so only D10 and D00
  # only for the youngest age group since no transistion
  d00=M%*%X00-extra_parms_vals_mat*X00+extra_parms_vals[3]*X10-mort_former*X00+t(age_matrix%*%t(X00))
  d01=M%*%X01-extra_parms_vals_mat*X01+extra_parms_vals[3]*X11-mort_former*X01+t(age_matrix%*%t(X01))
  d10=Mdash%*%X10+extra_parms_vals_mat*X00-extra_parms_vals[3]*X10-mort_current*X10+t(age_matrix%*%t(X10))
  d11=Mdash%*%X11+extra_parms_vals_mat*X01-extra_parms_vals[3]*X11-mort_current*X11+t(age_matrix%*%t(X11))
  d10[1,1] =d10[1,1] + sum(mort_former*X00) + sum(mort_former*X01) + sum(mort_current*X10) + sum(mort_current*X11) +
    death_vec[1]*sum(X00[12,])  + death_vec[2]*sum(X00[13,])  + death_vec[3]*sum(X00[14,])  + death_vec[4]*sum(X00[15,]) +
    death_vec[1]*sum(X01[12,])  + death_vec[2]*sum(X01[13,])  + death_vec[3]*sum(X01[14,])  + death_vec[4]*sum(X01[15,]) +
    death_vec[1]*sum(X10[12,])  + death_vec[2]*sum(X10[13,])  + death_vec[3]*sum(X10[14,])  + death_vec[4]*sum(X10[15,]) +
    death_vec[1]*sum(X11[12,])  + death_vec[2]*sum(X11[13,])  + death_vec[3]*sum(X11[14,])  + death_vec[4]*sum(X11[15,])  
  d10[1,1:9]=d10[1,1:9]+pop_per*1000
  return(list(c(as.vector(d00), as.vector(d01), as.vector(d10), as.vector(d11))))
  
}

popTrend3=function(t,pop_per1,pop_per2){
  # calculates the population scaling factor
  #print(t)
  if (t<60){ # t is 1950+(0:140)
    scaling_factor=0
  } else if ((t>=60)& (t<72)){
    scaling_factor=pop_per1
  } else if ((t>=72) & (t<=80)){
    scaling_factor=pop_per2
  } else if (t>80){
    scaling_factor=pop_per2
  }
  return(scaling_factor)
}

plotstacked_v3=function(X,L,prop_HCV_are_PWID){
  times      = seq(0, 90, by = 1)
  # 2007 pop is 2248500

  pop2016=2248500*sum(X[361:720,67])/sum(X[361:720,58])
  perof=sum(X[L$chronic10,67])/sum(X[361:540,67])
  scale10_point = perof*pop2016
  scale00_point = prop_HCV_are_PWID*3.5*10^6-scale10_point # 80% of adult pop acquired their infections through
  
  
  scale10=scale10_point/sum(X[L$chronic10,67]); 
  scale00=scale00_point/sum(X[L$chronic00,67]); 
  lt2=scale00*colSums(X[L$LT2_comps_00,])+scale10*colSums(X[L$LT2_comps_10,]);
  lt1=scale00*colSums(X[L$LT1_comps_00,])+scale10*colSums(X[L$LT1_comps_10,]);
  lt=lt1+lt2;
  dc=scale00*colSums(X[L$DC_comps_00,])+scale10*colSums(X[L$DC_comps_10,]);
  hcc=scale00*colSums(X[L$HCC_comps_00,])+scale10*colSums(X[L$HCC_comps_10,]);
  f4=scale00*colSums(X[L$F4_comps_00,])+scale10*colSums(X[L$F4_comps_10,]);
  f3=scale00*colSums(X[L$F3_comps_00,])+scale10*colSums(X[L$F3_comps_10,]);
  f2=scale00*colSums(X[L$F2_comps_00,])+scale10*colSums(X[L$F2_comps_10,]);
  f1=scale00*colSums(X[L$F1_comps_00,])+scale10*colSums(X[L$F1_comps_10,]);
  f0=scale00*colSums(X[L$F0_comps_00,])+scale10*colSums(X[L$F0_comps_10,]);
  y=c(f0,f1,f2,f3,f4,dc,hcc,lt)
  Stage=c(rep("F0",91),rep("F1",91),rep("F2",91),rep("F3",91),rep("F4",91),rep("DC",91),rep("HCC",91),rep("LT",91))
  t=rep(0:90,8)
  df=data.frame(y,t,Stage)
  maxy=max(pretty_breaks()(c(0, (f0+f1+f2+f3+f4+dc+hcc+lt))))
  vecy=pretty_breaks()(c(0, (f0+f1+f2+f3+f4+dc+hcc+lt)))
  df$Stage=factor(df$Stage,levels=c("F0","F1","F2","F3","F4","DC","HCC","LT"))
  p1=ggplot(data=subset(df,times>=71), aes(x=t+1950, y=y,fill=Stage)) + geom_area() +
    xlab("Year") + ylab("Population") + ggtitle(paste0("Number in chronic compartments"))+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2016,2050,1)),expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

plotPWID_scaled=function(XT,L,time_cut,leny,prop_HCV_are_PWID){
  # 2007 pop is 2248500
    pop2016=2248500*sum(XT[361:720,67])/sum(XT[361:720,58])
    perof=sum(XT[L$chronic10,67])/sum(XT[361:540,67])
   scale10_point = perof*pop2016
   scale00_point = prop_HCV_are_PWID*3.5*10^6-scale10_point # 80% of adult pop acquired their infections through
  
  
  scale10=scale10_point/sum(XT[L$chronic10,67]); 
  scale00=scale00_point/sum(XT[L$chronic00,67]); 
  
  #scale10 = 1
  aftertrt= scale10*colSums(XT[c(L$chronic10,L$A_comps_10),])+scale00*colSums(XT[c(L$chronic00,L$A_comps_00),])
  tend = dim(XT)[2]-1
  # total number of treatments
  if (leny==1){
    tend = time_cut+9
    tdum=1950+(time_cut:(tend))
    bt=tdum
  }else{
    tend=dim(XT)[2]
    tdum=1949+(time_cut:(tend))
    bt=c(seq(2007,(1950+tend),1))
  } # starts at zero
  df=data.frame(tdum=tdum,aftertrt=aftertrt[time_cut:(tend)])
  maxy=max(pretty_breaks()(c(0, df$aftertrt)))
  vecy=pretty_breaks()(c(0, df$aftertrt))
  p2=ggplot(data=df, aes(x=tdum, y=aftertrt)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of subjects") + ggtitle(paste0("# of PWID (current and former) with HCV"))+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = bt,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))
}

plot_prev_v4=function(X,L,time_cut){
  times      = seq(0, 90, by = 1)
  tend=dim(X)[2]-1
  tdum=1949+(time_cut:(tend))
  y=100*colSums(X[L$chronic10,])/colSums(X[361:540,])
  df=data.frame(times,y)
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=subset(df,times>=71), aes(x=times+1950, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Percentage") + ggtitle(paste0("% of current PWID with chronic HCV"))+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2021,2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p1)  
}

plotincidence_v2_change=function(XT,L,time_cut,delta,acuterate,curr_mort_pwid,curr_mort_former,prop_HCV_are_PWID){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  pop2016=2248500*sum(XT[361:720,67])/sum(XT[361:720,58])
  perof=sum(XT[L$chronic10,67])/sum(XT[361:540,67])
  scale10_point = perof*pop2016
  scale00_point = prop_HCV_are_PWID*3.5*10^6-scale10_point # 80% of adult pop acquired their infections through
  
  scale10=scale10_point/sum(XT[L$chronic10,67]); 
  scale00=scale00_point/sum(XT[L$chronic00,67]);

  T00=scale00*as.vector((1-curr_mort_former)%*%XT[L$A_comps_00,])
  T01=scale00*as.vector((1-curr_mort_former)%*%XT[L$A_comps_01,])
  T11=scale10*as.vector((1-curr_mort_pwid)%*%XT[L$A_comps_11,])
  T10=scale10*as.vector((1-curr_mort_pwid)%*%XT[L$A_comps_10,])

  topy=scale00*sum(XT[c(L$A_comps_00),69])+scale10*sum(XT[c(L$A_comps_10),69])
  boty=scale00*sum(XT[c(L$A_comps_00),60])+scale10*sum(XT[c(L$A_comps_10),60])
  fout3=topy/boty
  fout1 = sum(XT[L$chronic10,67])/sum(XT[361:540,67])
  # integrate
  t=time_cut-1 # since starts at 1950
  tend = dim(XT)[2]-1
  yinci_PWID=T00[t:tend]+T01[t:tend]+T10[t:tend]+T11[t:tend]
  df= data.frame(x=1950+(t:tend),y=100*((yinci_PWID/yinci_PWID[1] - 1)))
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Change %") + ggtitle("% incidence change compared to 2021")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(1950+t,1950+tend,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))
  return(p1)
}

plotincidence_v2=function(XT,L,time_cut,delta,acuterate,curr_mort_pwid,curr_mort_former,prop_HCV_are_PWID){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  pop2016=2248500*sum(XT[361:720,67])/sum(XT[361:720,58])
  perof=sum(XT[L$chronic10,67])/sum(XT[361:540,67])
  scale10_point = perof*pop2016
  scale00_point = prop_HCV_are_PWID*3.5*10^6-scale10_point # 80% of adult pop acquired their infections through
  
  scale10=scale10_point/sum(XT[L$chronic10,67]); 
  scale00=scale00_point/sum(XT[L$chronic00,67]);
  T00=scale00*as.vector((1-curr_mort_former)%*%XT[L$A_comps_00,])
  T01=scale00*as.vector((1-curr_mort_former)%*%XT[L$A_comps_01,])
  T11=scale10*as.vector((1-curr_mort_pwid)%*%XT[L$A_comps_11,])
  T10=scale10*as.vector((1-curr_mort_pwid)%*%XT[L$A_comps_10,])
  
  topy=scale00*sum(XT[c(L$A_comps_00),69])+scale10*sum(XT[c(L$A_comps_10),69])
  boty=scale00*sum(XT[c(L$A_comps_00),60])+scale10*sum(XT[c(L$A_comps_10),60])
  fout3=topy/boty
  fout1 = sum(XT[L$chronic10,67])/sum(XT[361:540,67])
  # integrate
  t=time_cut-1 # since starts at 1950
  tend = dim(XT)[2]-1
  yinci_PWID=(1-delta)*(T00[t:tend]+T01[t:tend]+T10[t:tend]+T11[t:tend])*acuterate
  df= data.frame(x=1950+(t:tend),y=yinci_PWID)
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of subjects") + ggtitle("# of incident chronic cases")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(1950+t,1950+tend,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))
  return(p1)
}
