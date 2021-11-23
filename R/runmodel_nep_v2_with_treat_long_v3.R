runmodel_nep_v2_with_treat_long_v3=function(scale00,scale10,TreatmentStartTime,
                                         DAAtreat,SCreening,NSPOST,s,
                                         scenario_number,
                                         treatduration,
                                         mortvec,incivec,
                                         pop_per="not used",
                                         #pcom=0.892,alpha=0.95,
                                         nep_rel_risk_red=0.59,nepdrop=0.01,idu_nep_cess=1/17, # note rel risk def is (1-nep_rel_risk_red)*inf rate
                                         ost_rel_risk_red=0.59,ostdrop=0.01,idu_ost_cess=1/17, # rel risk is 41% for OST Martin
                                         nep_ost_rel_risk_red=0.75,nep_ostdrop=0.01,idu_nep_ost_cess=1/17){
  #nep_ost_rel_risk_red=0.75,nep_ostdrop=0,idu_nep_ost_cess=0){
  # risk reduction heffernan for both at 75%
  # test data 0.41, 0.1,0.01,1/10
  # *_rel_risk, this is the risk reduction in transmission rate
  # = 0.41 means that transmission rate is reduced to 0.59*transmission rate
  # *cov is the intervention coverage per year as a proportion - converted to rate
  # *drop is the drop out probability per year from the relevant intervention - converted to rate
  # idu_*_cess is the cessation rate from the relevant intervention - 1/duration - go to former compartments
  # This models an extra set of NEP compartments
  # one for current PWID no fail and one for failed
  # Adds additional sets for OST and NSP and OST
  # April 17 run the long time model 0: 140 which is 1950 to 2090
  # April 27 this updates to the two fit flat PWID and increasing PWID
  # pop_per is now Yes or Flat
  library(gridExtra)
  library(cowplot)
  library(ggplot2)
  library(scales)
  # This runs the model
  L=coeffsof_v5()
  # now read from database
  dum=pickupDAA_efficacy()
  pcom=dum$pcom
  alpha = dum$alpha_current # at moment same in current and former
  N=rep(0,1800) # to allow for the NEP compartments
  N[1]=560;N[361]=396;N[367]=44

  # set up the new structure
  # old was  param_vals=param_setup_daa_v2(rin,treatduration) # length 916
  dum=readRDS(paste0("www/scenario_",scenario_number,"_param_PWID.rds"))
  #print(paste0("the scenario number is ",scenario_number))
  param_vals=dum[1:916]
  # add in the treatduration
  trt_ind = c(16,37,58,79,100,316,337,358,379,400) # 16,1 17,2 18,3 19,4 20,5 16,16 17,17 18,18 19,19 20,20
  param_vals[trt_ind[1:5]]=treatduration # ***************** second five are - the treatduiration
  param_vals[trt_ind[6:10]]=-treatduration
  param_vals[(trt_ind[1:5]+400)]=treatduration
  param_vals[(trt_ind[6:10]+400)]=-treatduration
  pop_per_growth=0 # not used now,left in as zero
  param_vals=c(param_vals,nep_rel_risk_red,NSPOST$NSPcov,nepdrop,idu_nep_cess, # nsp
               ost_rel_risk_red,NSPOST$OSTcov,ostdrop,idu_ost_cess,  # ost
               nep_ost_rel_risk_red,NSPOST$NSP_OSTcov,nep_ostdrop,idu_nep_ost_cess, # both
               scale00,scale10,pcom,alpha,TreatmentStartTime-1, # for model
               DAAtreat$Pcurrenmild,DAAtreat$Pcurrenadva, # note no former - need to subtract 2 from index
               SCreening$Ditimedur,SCreening$FixedyCov,pop_per_growth,
               #rin[4],rin[5],rin[6]) # note these three model terms no added here
               dum[918],dum[919],dum[920],
               dum[921],dum[922],
               DAAtreat$Pformermild,DAAtreat$Pformeradva) # These are the new relapse parameters
  dum=asode_nep_ost_daa_long_v3(param_vals,N,L) # this is the full version with DAA and nep and OST
  #print(NSPOST)
  X=dum$X[,1:91] # restrict the time for these output
  if (SCreening$FixedyCov==0){ # NSP coverage is ignored
    if (DAAtreat$Pcurrenmild ==0 & DAAtreat$Pcurrenadva==0 & DAAtreat$Pformermild ==0 & DAAtreat$Pformeradva==0){
      X=rbind(s$X,matrix(data=0,nrow=1080,ncol=91)) # no interventions set same as base for consistency
    }
  } else {
    if ((NSPOST$NSPcov==0 & NSPOST$OSTcov==0 & NSPOST$NSP_OSTcov==0)){
          X=rbind(s$X,matrix(data=0,nrow=1080,ncol=91)) # no interventions set same as base for consistency
    }
  }
  # if coverage is not zero, even if DAAs are zero still some effect so don't set to zero
  #print(DAAtreat)
  rA_F0= -param_vals[106]
  delta=param_vals[6]/rA_F0
  # added
  curr_mort_pwid=param_vals[895:903]
  curr_mort_former=param_vals[904:912]
  # source("~/HCV_vV/HCV_prison/global_newplots.R")
  # ptest = plot_Rt_PWID_prison_gp(X[,65:90],X[,65:90],X[,65:90],2015:2040,2,1,scale00,scale10,scale00,scale10)  

  
  #prop_HCV_are_PWID = TT$X[scenario_number]/100 # not needed here defined in the scale00
  p0=plot_output_chronic_all(0:90,L,X,scale00,scale10,TreatmentStartTime,1)
  p0_long=plot_output_chronic_all(0:90,L,X,scale00,scale10,TreatmentStartTime,2)
  p1=plot_output_incidence_new_chronic_cases_v2_all(X,scale00,scale10,delta,rA_F0,TreatmentStartTime,
                                                    L$A_comps_00,L$A_comps_01,L$A_comps_10,L$A_comps_11,1,
                                                    curr_mort_pwid,curr_mort_former) # changed 1-exp(-rA_F0) to rA_F0
  p2=plot_output_treats_v2_all(0:90,X,scale00,scale10,pcom,alpha,TreatmentStartTime,DAAtreat,SCreening,
                               NSPOST$NSPcov,NSPOST$OSTcov,NSPOST$NSP_OSTcov)
  LDAAtreat=cost_qaly_run_with_treats(X[,73:91],scale00,scale10,p2$data$y,treatduration,3.5)

  p3=plot_output_mortality_v2_all(X,1-exp(-param_vals[913:916]),scale00,scale10,TreatmentStartTime,
                                  0:90,pcom,alpha,DAAtreat,SCreening,NSPOST$NSPcov,NSPOST$OSTcov,NSPOST$NSP_OSTcov,1)
    
  #p3=plot_output_mortality_v2_all(X,1-exp(-param_vals[913:916]),scale00,scale10,TreatmentStartTime,1)
  p4=plot_output_cases_averted_all(0:90,L,s$X,X,scale00,scale10,pcom,alpha,TreatmentStartTime,DAAtreat,SCreening,
                                   NSPOST$NSPcov,NSPOST$OSTcov,NSPOST$NSP_OSTcov)
  C=getallpop_all(X,L,rep(1,9),rep(1,9),length(0:90),scale00,scale10)
  p5=plot_areacomps_v2(0:90,C)
  p6=plot_allcomps_v2_all(0:90,X,L$chronic10)
  p7=plot_attack_rate_v_treat_all(0:90,L,s$X,X,scale00,scale10,pcom,alpha,TreatmentStartTime,DAAtreat,SCreening,
                                  NSPOST$NSPcov,NSPOST$OSTcov,NSPOST$NSP_OSTcov)
  p8=plot_output_mortality_v2_all_long(X,param_vals[913:916],scale00,scale10,TreatmentStartTime)
  p9=plot_output_incidence_new_chronic_cases_v2_all_long(X,scale00,scale10,delta,rA_F0,TreatmentStartTime,
                                                         L$A_comps_00,L$A_comps_01,L$A_comps_10,L$A_comps_11)
  p10=plot_output_nep_ost_v3_all_count(0:90,X,scale10,time_ofint_inODEunits)
  p11=plot_allcomps_v3(0:90,X,scale10,L)
  p12=plot_output_pwid_all(0:90,L,X,scale00,scale10,TreatmentStartTime,1)
  p12_long=plot_output_pwid_all(0:90,L,X,scale00,scale10,TreatmentStartTime,2)
  
  plotmort_num=plot_output_mortality_v2_all(X,1-exp(-param_vals[913:916]),scale00,scale10,TreatmentStartTime,
                                            0:90,pcom,alpha,DAAtreat,SCreening,NSPOST$NSPcov,NSPOST$OSTcov,NSPOST$NSP_OSTcov,2)
  plotinci_num=plot_output_incidence_new_chronic_cases_v2_all(X,scale00,scale10,delta,rA_F0,TreatmentStartTime,
                                                    L$A_comps_00,L$A_comps_01,L$A_comps_10,L$A_comps_11,2,
                                                    curr_mort_pwid,curr_mort_former)  # changed 1-exp(-rA_F0) to rA_F0

  if (abs(sum(mortvec))>1e-01){
    # add in the deaths form general population
    p3=plot_output_mortality_v2_all_genpop_extra(X,1-exp(-param_vals[913:916]),scale00,scale10,TreatmentStartTime,
                                 0:90,pcom,alpha,DAAtreat,SCreening,NSPOST$NSPcov,NSPOST$OSTcov,NSPOST$NSP_OSTcov,1,mortvec)
    plotmort_num=plot_output_mortality_v2_all_genpop_extra(X,1-exp(-param_vals[913:916]),scale00,scale10,TreatmentStartTime,
                                              0:90,pcom,alpha,DAAtreat,SCreening,NSPOST$NSPcov,NSPOST$OSTcov,NSPOST$NSP_OSTcov,2,mortvec)
  }
  if (abs(sum(incivec))>1e-01){
    # add in the incidence from general population
    p1=plot_output_incidence_new_chronic_cases_v2_all_genpop_extra(X,scale00,scale10,delta,rA_F0,TreatmentStartTime,
                                                      L$A_comps_00,L$A_comps_01,L$A_comps_10,L$A_comps_11,1,
                                                      curr_mort_pwid,curr_mort_former,incivec) # changed 1-exp(-rA_F0) to rA_F0
    plotinci_num=plot_output_incidence_new_chronic_cases_v2_all_genpop_extra(X,scale00,scale10,delta,rA_F0,TreatmentStartTime,
                                                                L$A_comps_00,L$A_comps_01,L$A_comps_10,L$A_comps_11,2,
                                                                curr_mort_pwid,curr_mort_former,incivec)  # changed 1-exp(-rA_F0) to rA_F0
  }

  plot_both=plot_cases_deaths(plotmort_num$data$x,plotinci_num$data$y,plotmort_num$data$y)

  return(list(plotinci=p1,plotmort=p3,plottreats=p2,
              plotcasepertreat=p4,attackrate=p6,stages=p5,attackrate_per_treat=p7,
              plotmort_long=p8,plotinci_long=p9,prevent_count=p10,F0toF4current=p11,
              plotchronic_num=p0,plotchronic_num_long=p0_long,
              plotpwid_num=p12,plotpwid_num_long=p12_long,
              plotmort_num=plotmort_num,
              plotinci_num=plotinci_num,
              Qdattreat=LDAAtreat,
              plot_both=plot_both))
}

asode_nep_ost_daa_long_v3=function(param_vals,N,L){
  library(deSolve)
  
  
  init       = N
  parameters = param_vals
  times      = seq(0, 90, by = 1)
  lt=length(times)
  X = ode(y=init, times=times, func=MODEL_nep_ost_daa_long_v3, parms=parameters,method="ode45")
  X=t(X[,2:1801])
  a=NULL
  #a=plot_allcomps_v2nep(0:80,X,L$chronic10)
  return(list(a=a,X=X))
}
asode_nep_ost_daa_long_v4=function(param_vals,N,L){
  library(deSolve)
  
  
  init       = N
  parameters = param_vals
  times      = seq(0, 90, by = 1)
  lt=length(times)
  X = ode(y=init, times=times, func=MODEL_nep_ost_daa_long_v4, parms=parameters,method="ode45")
  X=t(X[,2:1801])
  a=NULL
  #a=plot_allcomps_v2nep(0:80,X,L$chronic10)
  return(list(a=a,X=X))
}

MODEL_nep_ost_daa_long_v4 <- function(time, state, parameters) {
  # Changed now uses the new trend
  
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
  nep_rel_risk_red_in=parameters[917];nepcov_in=parameters[918];nepdrop_in=parameters[919];idu_nep_cess_in=parameters[920]
  ost_rel_risk_red_in=parameters[921];ostcov_in=parameters[922];ostdrop_in=parameters[923];idu_ost_cess_in=parameters[924]
  nep_ost_rel_risk_red_in=parameters[925];nep_ostcov_in=parameters[926];nep_ostdrop_in=parameters[927];idu_nep_ost_cess_in=parameters[928]
  
  # intervention parameters
  scale00=parameters[929] # scale00 and scale10 are the former and current scaling factors
  scale10=parameters[930]
  pcom=parameters[931] # the probability of pwid completing treatment
  alpha=parameters[932] # alpha is the SVR probability
  time_ofint_inODEunits=parameters[933] # the year of intervention in the ODE time scale, 73 for 0:81 for 2022
  DAAtreat=data.frame(Pcurrenmild=parameters[934],Pcurrenadva=parameters[935])
  SCreening=data.frame(Ditimedur=parameters[936],FixedyCov=parameters[937])
  pop_per=parameters[938]
  pop_per1=parameters[939]
  pop_per2=parameters[940]
  scale_mult=parameters[941]
  t=time
  # These subjects go to the relevant former compartment
  if (t>=(time_ofint_inODEunits-1)) { # year 2022 and onwards
    enroll_nep=nepcov_in
    dropout_nep=-log(1-nepdrop_in)
    nep_rel_risk_red=nep_rel_risk_red_in
    idu_nep_cess=idu_nep_cess_in
    
    enroll_ost=ostcov_in
    dropout_ost=-log(1-ostdrop_in)
    ost_rel_risk_red=ost_rel_risk_red_in
    idu_ost_cess=idu_ost_cess_in    
    
    enroll_nep_ost=nep_ostcov_in
    dropout_nep_ost=-log(1-nep_ostdrop_in)    
    nep_ost_rel_risk_red=nep_ost_rel_risk_red_in
    idu_nep_ost_cess=idu_nep_ost_cess_in
    
    if  (SCreening$FixedyCov==0){
      # This removes all the prevention interventions
      nep_rel_risk_red=0
      enroll_nep=0
      dropout_nep=0
      idu_nep_cess=0
      
      ost_rel_risk_red=0
      enroll_ost=0
      dropout_ost=0
      idu_ost_cess=0
      
      nep_ost_rel_risk_red=0
      enroll_nep_ost=0
      dropout_nep_ost=0
      idu_nep_ost_cess=0
    }
    
  }else{
    nep_rel_risk_red=0
    enroll_nep=0
    dropout_nep=0
    idu_nep_cess=0
    
    ost_rel_risk_red=0
    enroll_ost=0
    dropout_ost=0
    idu_ost_cess=0
    
    nep_ost_rel_risk_red=0
    enroll_nep_ost=0
    dropout_nep_ost=0
    idu_nep_ost_cess=0
  }
  mort_current = t(matrix(rep(curr_mort_pwid,20),9,20))
  mort_former = t(matrix(rep(curr_mort_former,20),9,20))
  
  Irows = c(6,7,8,9,10,11,12,13,14,15) # rows of infectious PWID
  #death_vec= c(-log(1-death_rate_dc), -log(1-death_rate_hcc), -log(1-death_rate_lt1), -log(1- death_rate_lt2)); #DC,HCC,LT1,LT2
  # No already converted to rates in param_setup_daa and param_setup
  death_vec= c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  N=state
  # percentage PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(360,520,20),nrow=9,ncol=10)
  top10_index = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(540,700,20),nrow=9,ncol=10)
  top11_index =  matrix(t(dum),nrow=90,ncol=1)
  # add in the NEP PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(720,880,20),nrow=9,ncol=10)
  top10_index_nep = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(900,1060,20),nrow=9,ncol=10)
  top11_index_nep =  matrix(t(dum),nrow=90,ncol=1)
  # add in the OST PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1080,1240,20),nrow=9,ncol=10)
  top10_index_ost = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1260,1420,20),nrow=9,ncol=10)
  top11_index_ost =  matrix(t(dum),nrow=90,ncol=1)  
  # add in the NEP & OST PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1440,1600,20),nrow=9,ncol=10)
  top10_index_nep_ost = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1620,1780,20),nrow=9,ncol=10)
  top11_index_nep_ost =  matrix(t(dum),nrow=90,ncol=1)  
  
  top = sum(N[top10_index])+sum(N[top11_index]) +
    sum(N[top10_index_nep])+sum(N[top11_index_nep]) +
    sum(N[top10_index_ost])+sum(N[top11_index_ost]) +
    sum(N[top10_index_nep_ost])+sum(N[top11_index_nep_ost])
  
  bot = sum(N[361:length(N)]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  I = top/bot;
  if (t<60){
    scaleI=1
  }else{
    scaleI=scale_mult
  }
  pop_per_val=popTrend3(t,pop_per1,pop_per2)
  endy=length(phiminusvals)
  phi = scaleI*extra_parms_vals[1]*I
  dum = Mdash[5,5]
  
  Mdash_nep=Mdash
  Mdash_ost=Mdash
  Mdash_nep_ost=Mdash
  # no NSP or OST
  Mdash[phiminusvals[1:(endy-1)]]=-phi
  Mdash[phiminusvals[endy]]=-phi+dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash[phiplussvals]=phi;
  # NEP matrix
  Mdash_nep[phiminusvals[1:(endy-1)]]=-phi*(1 - nep_rel_risk_red) 
  Mdash_nep[phiminusvals[endy]]=-phi*(1 - nep_rel_risk_red) +dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash_nep[phiplussvals]=phi*(1 - nep_rel_risk_red) 
  # OST matrix
  Mdash_ost[phiminusvals[1:(endy-1)]]=-phi*(1 - ost_rel_risk_red) 
  Mdash_ost[phiminusvals[endy]]=-phi*(1 - ost_rel_risk_red) +dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash_ost[phiplussvals]=phi*(1 - ost_rel_risk_red) 
  # NEP_OST matrix
  Mdash_nep_ost[phiminusvals[1:(endy-1)]]=-phi*(1 - nep_ost_rel_risk_red) 
  Mdash_nep_ost[phiminusvals[endy]]=-phi*(1 - nep_ost_rel_risk_red) +dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash_nep_ost[phiplussvals]=phi*(1 - nep_ost_rel_risk_red)  
  
  X00 = matrix(N[1:(20*9)],20,9)
  X01 = matrix(N[181:(180+20*9)],20,9)
  X10 = matrix(N[361:(360+20*9)],20,9)
  X11 = matrix(N[541:(540+20*9)],20,9)
  X10_nep = matrix(N[721:(720+20*9)],20,9)
  X11_nep = matrix(N[901:(900+20*9)],20,9)
  X10_ost = matrix(N[1081:(1080+20*9)],20,9)
  X11_ost = matrix(N[1261:(1260+20*9)],20,9)
  X10_nep_ost = matrix(N[1441:(1440+20*9)],20,9)
  X11_nep_ost = matrix(N[1621:(1620+20*9)],20,9)  
  
  # extra_parms_vals_vec=c(extra_parms_vals[2],relapse2,rep(relapse3,7))
  extra_parms_vals_vec=c(extra_parms_vals[2],extra_parms_vals[2],rep(extra_parms_vals[2],7))
  extra_parms_mat = t(matrix(rep(extra_parms_vals_vec,20),ncol=20))
  
  
  
  d00=M%*%X00-extra_parms_mat*X00+extra_parms_vals[3]*X10-mort_former*X00+t(age_matrix%*%t(X00)) +
    idu_nep_cess*X10_nep+idu_ost_cess*X10_ost+idu_nep_ost_cess*X10_nep_ost
  d01=M%*%X01-extra_parms_mat*X01+extra_parms_vals[3]*X11-mort_former*X01+t(age_matrix%*%t(X01)) +
    idu_nep_cess*X11_nep+idu_ost_cess*X11_ost+idu_nep_ost_cess*X11_nep_ost
  d10=Mdash%*%X10+extra_parms_mat*X00-extra_parms_vals[3]*X10-mort_current*X10+t(age_matrix%*%t(X10))-
    enroll_nep*X10 + dropout_nep*X10_nep - enroll_ost*X10 + dropout_ost*X10_ost - enroll_nep_ost*X10 + dropout_nep_ost*X10_nep_ost
  d11=Mdash%*%X11+extra_parms_mat*X01-extra_parms_vals[3]*X11-mort_current*X11+t(age_matrix%*%t(X11))-
    enroll_nep*X11 + dropout_nep*X11_nep - enroll_ost*X11 + dropout_ost*X11_ost - enroll_nep_ost*X11 + dropout_nep_ost*X11_nep_ost
  
  
  # d00=M%*%X00-extra_parms_vals[2]*X00+extra_parms_vals[3]*X10-mort_former*X00+t(age_matrix%*%t(X00)) +
  #   idu_nep_cess*X10_nep+idu_ost_cess*X10_ost+idu_nep_ost_cess*X10_nep_ost
  # d01=M%*%X01-extra_parms_vals[2]*X01+extra_parms_vals[3]*X11-mort_former*X01+t(age_matrix%*%t(X01)) +
  #   idu_nep_cess*X11_nep+idu_ost_cess*X11_ost+idu_nep_ost_cess*X11_nep_ost
  # d10=Mdash%*%X10+extra_parms_vals[2]*X00-extra_parms_vals[3]*X10-mort_current*X10+t(age_matrix%*%t(X10))-
  #   enroll_nep*X10 + dropout_nep*X10_nep - enroll_ost*X10 + dropout_ost*X10_ost - enroll_nep_ost*X10 + dropout_nep_ost*X10_nep_ost
  # d11=Mdash%*%X11+extra_parms_vals[2]*X01-extra_parms_vals[3]*X11-mort_current*X11+t(age_matrix%*%t(X11))-
  #   enroll_nep*X11 + dropout_nep*X11_nep - enroll_ost*X11 + dropout_ost*X11_ost - enroll_nep_ost*X11 + dropout_nep_ost*X11_nep_ost
  # Add two extra models for d10_nep and d11_nep
  # The NEP enrollment rate is enroll_nep, they can come from any of the d10 or d11 compartments
  d10_nep = enroll_nep*X10 +
    Mdash_nep%*%X10_nep-mort_current*X10_nep+t(age_matrix%*%t(X10_nep))-
    dropout_nep*X10_nep - idu_nep_cess*X10_nep
  d11_nep = enroll_nep*X11 +
    Mdash_nep%*%X11_nep-mort_current*X11_nep+t(age_matrix%*%t(X11_nep))-
    dropout_nep*X11_nep - idu_nep_cess*X11_nep
  d10_ost = enroll_ost*X10 +
    Mdash_ost%*%X10_ost-mort_current*X10_ost+t(age_matrix%*%t(X10_ost))-
    dropout_ost*X10_ost - idu_ost_cess*X10_ost
  d11_ost = enroll_ost*X11 +
    Mdash_ost%*%X11_ost-mort_current*X11_ost+t(age_matrix%*%t(X11_ost))-
    dropout_ost*X11_ost - idu_ost_cess*X11_ost
  d10_nep_ost = enroll_nep_ost*X10 +
    Mdash_nep_ost%*%X10_nep_ost-mort_current*X10_nep_ost+t(age_matrix%*%t(X10_nep_ost))-
    dropout_nep_ost*X10_nep_ost - idu_nep_ost_cess*X10_nep_ost
  d11_nep_ost = enroll_nep_ost*X11 +
    Mdash_nep_ost%*%X11_nep_ost-mort_current*X11_nep_ost+t(age_matrix%*%t(X11_nep_ost))-
    dropout_nep_ost*X11_nep_ost - idu_nep_ost_cess*X11_nep_ost
  
  d10[1,1] =d10[1,1] + sum(mort_former*X00) + sum(mort_former*X01) + sum(mort_current*X10) + sum(mort_current*X11) +
    sum(mort_current*X10_nep) + sum(mort_current*X11_nep) +
    sum(mort_current*X10_ost) + sum(mort_current*X11_ost) +
    sum(mort_current*X10_nep_ost) + sum(mort_current*X11_nep_ost) +
    death_vec[1]*sum(X00[12,])  + death_vec[2]*sum(X00[13,])  + death_vec[3]*sum(X00[14,])  + death_vec[4]*sum(X00[15,]) +
    death_vec[1]*sum(X01[12,])  + death_vec[2]*sum(X01[13,])  + death_vec[3]*sum(X01[14,])  + death_vec[4]*sum(X01[15,]) +
    death_vec[1]*sum(X10[12,])  + death_vec[2]*sum(X10[13,])  + death_vec[3]*sum(X10[14,])  + death_vec[4]*sum(X10[15,]) +
    death_vec[1]*sum(X11[12,])  + death_vec[2]*sum(X11[13,])  + death_vec[3]*sum(X11[14,])  + death_vec[4]*sum(X11[15,]) +  
    death_vec[1]*sum(X10_nep[12,])  + death_vec[2]*sum(X10_nep[13,])  + death_vec[3]*sum(X10_nep[14,])  + death_vec[4]*sum(X10_nep[15,]) +
    death_vec[1]*sum(X11_nep[12,])  + death_vec[2]*sum(X11_nep[13,])  + death_vec[3]*sum(X11_nep[14,])  + death_vec[4]*sum(X11_nep[15,]) +  
    death_vec[1]*sum(X10_ost[12,])  + death_vec[2]*sum(X10_ost[13,])  + death_vec[3]*sum(X10_ost[14,])  + death_vec[4]*sum(X10_ost[15,]) +
    death_vec[1]*sum(X11_ost[12,])  + death_vec[2]*sum(X11_ost[13,])  + death_vec[3]*sum(X11_ost[14,])  + death_vec[4]*sum(X11_ost[15,]) + 
    death_vec[1]*sum(X10_nep_ost[12,])  + death_vec[2]*sum(X10_nep_ost[13,])  + death_vec[3]*sum(X10_nep_ost[14,])  + death_vec[4]*sum(X10_nep_ost[15,]) +
    death_vec[1]*sum(X11_nep_ost[12,])  + death_vec[2]*sum(X11_nep_ost[13,])  + death_vec[3]*sum(X11_nep_ost[14,])  + death_vec[4]*sum(X11_nep_ost[15,])   
  d10[1,1:9]=d10[1,1:9]+pop_per_val*1000
  dum=treat_comps_pv7(N,scale00,scale10,t,pcom,alpha,time_ofint_inODEunits,
                      DAAtreat,SCreening,
                      enroll_nep,enroll_ost,enroll_nep_ost)
  phi=dum$phi
  phidash=dum$phidash
  #print(paste0("sum phi < 0 ",sum(phi<0)," t=",t,"n"))
  #print(paste0("sum phidash < 0 ",sum(phidash<0)," t=",t,"n"))
  # These will actually be zero as only curren treated - left in for future
  trtmodel00=-phi[1:180]+phidash[1:180]
  trtmodel01=(1-alpha)*phi[1:180] # treatment failures,alpha is in phidash above
  trtmodel10=-phi[361:540]+phidash[361:540]
  trtmodel11=(1-alpha*pcom)*phi[361:540] #treatment failures aplha*pcom is in phidash above
  
  trtmodel10_nep=-phi[721:900]+phidash[721:900]
  trtmodel11_nep=(1-alpha*pcom)*phi[721:900] #treatment failures aplha*pcom is in phidash above
  
  trtmodel10_ost=-phi[1081:1260]+phidash[1081:1260]
  trtmodel11_ost=(1-alpha*pcom)*phi[1081:1260] #treatment failures aplha*pcom is in phidash above
  
  trtmodel10_nep_ost=-phi[1441:1620]+phidash[1441:1620]
  trtmodel11_nep_ost=(1-alpha*pcom)*phi[1441:1620] #treatment failures aplha*pcom is in phidash above
  
  return(list(c(as.vector(d00)+trtmodel00, as.vector(d01)+trtmodel01, as.vector(d10)+trtmodel10, as.vector(d11)+trtmodel11,
                as.vector(d10_nep)+trtmodel10_nep, as.vector(d11_nep)+trtmodel11_nep,
                as.vector(d10_ost)+trtmodel10_ost, as.vector(d11_ost)+trtmodel11_ost,
                as.vector(d10_nep_ost)+trtmodel10_nep_ost, as.vector(d11_nep_ost)+trtmodel11_nep_ost)))
  
}


MODEL_nep_ost_daa_long_v3 <- function(time, state, parameters) {
  # Changed now uses the new trend
  
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
  nep_rel_risk_red_in=parameters[917];nepcov_in=parameters[918];nepdrop_in=parameters[919];idu_nep_cess_in=parameters[920]
  ost_rel_risk_red_in=parameters[921];ostcov_in=parameters[922];ostdrop_in=parameters[923];idu_ost_cess_in=parameters[924]
  nep_ost_rel_risk_red_in=parameters[925];nep_ostcov_in=parameters[926];nep_ostdrop_in=parameters[927];idu_nep_ost_cess_in=parameters[928]
  
  # intervention parameters
  scale00=parameters[929] # scale00 and scale10 are the former and current scaling factors
  scale10=parameters[930]
  pcom=parameters[931] # the probability of pwid completing treatment
  alpha=parameters[932] # alpha is the SVR probability
  time_ofint_inODEunits=parameters[933] # the year of intervention in the ODE time scale, 73 for 0:81 for 2022
  DAAtreat=data.frame(Pcurrenmild=parameters[934],Pcurrenadva=parameters[935],Pformermild=parameters[944],Pformeradva=parameters[945])
  #cat(paste0(DAAtreat$Pcurrenadva,"\r"))
  SCreening=data.frame(Ditimedur=parameters[936],FixedyCov=parameters[937])
  pop_per=parameters[938]
  pop_per1=parameters[939]
  pop_per2=parameters[940]
  scale_mult=parameters[941]
  # new parameters
  relapse2=parameters[942]
  relapse3=parameters[943]
  t=time
  if (t>76){
    
    stopy=3
  }
  # These subjects go to the relevant former compartment
  if (t>=(time_ofint_inODEunits-1)) { # year 2022 and onwards
    enroll_nep=nepcov_in
    dropout_nep=-log(1-nepdrop_in)
    nep_rel_risk_red=nep_rel_risk_red_in
    idu_nep_cess=idu_nep_cess_in
    
    enroll_ost=ostcov_in
    dropout_ost=-log(1-ostdrop_in)
    ost_rel_risk_red=ost_rel_risk_red_in
    idu_ost_cess=idu_ost_cess_in    
    
    enroll_nep_ost=nep_ostcov_in
    dropout_nep_ost=-log(1-nep_ostdrop_in)    
    nep_ost_rel_risk_red=nep_ost_rel_risk_red_in
    idu_nep_ost_cess=idu_nep_ost_cess_in
    
    if  (SCreening$FixedyCov==0){
      # This removes all the prevention interventions
      nep_rel_risk_red=0
      enroll_nep=0
      dropout_nep=0
      idu_nep_cess=0
      
      ost_rel_risk_red=0
      enroll_ost=0
      dropout_ost=0
      idu_ost_cess=0
      
      nep_ost_rel_risk_red=0
      enroll_nep_ost=0
      dropout_nep_ost=0
      idu_nep_ost_cess=0
    }
    
  }else{
    nep_rel_risk_red=0
    enroll_nep=0
    dropout_nep=0
    idu_nep_cess=0
    
    ost_rel_risk_red=0
    enroll_ost=0
    dropout_ost=0
    idu_ost_cess=0
    
    nep_ost_rel_risk_red=0
    enroll_nep_ost=0
    dropout_nep_ost=0
    idu_nep_ost_cess=0
  }
  mort_current = t(matrix(rep(curr_mort_pwid,20),9,20))
  mort_former = t(matrix(rep(curr_mort_former,20),9,20))
  
  Irows = c(6,7,8,9,10,11,12,13,14,15) # rows of infectious PWID
  #death_vec= c(-log(1-death_rate_dc), -log(1-death_rate_hcc), -log(1-death_rate_lt1), -log(1- death_rate_lt2)); #DC,HCC,LT1,LT2
  # No already converted to rates in param_setup_daa and param_setup
  death_vec= c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  N=state
  # percentage PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(360,520,20),nrow=9,ncol=10)
  top10_index = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(540,700,20),nrow=9,ncol=10)
  top11_index =  matrix(t(dum),nrow=90,ncol=1)
  # add in the NEP PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(720,880,20),nrow=9,ncol=10)
  top10_index_nep = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(900,1060,20),nrow=9,ncol=10)
  top11_index_nep =  matrix(t(dum),nrow=90,ncol=1)
  # add in the OST PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1080,1240,20),nrow=9,ncol=10)
  top10_index_ost = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1260,1420,20),nrow=9,ncol=10)
  top11_index_ost =  matrix(t(dum),nrow=90,ncol=1)  
  # add in the NEP & OST PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1440,1600,20),nrow=9,ncol=10)
  top10_index_nep_ost = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1620,1780,20),nrow=9,ncol=10)
  top11_index_nep_ost =  matrix(t(dum),nrow=90,ncol=1)  
  
  top = sum(N[top10_index])+sum(N[top11_index]) +
    sum(N[top10_index_nep])+sum(N[top11_index_nep]) +
    sum(N[top10_index_ost])+sum(N[top11_index_ost]) +
    sum(N[top10_index_nep_ost])+sum(N[top11_index_nep_ost])
  
  bot = sum(N[361:length(N)]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  I = top/bot;
  if (t<60){
    scaleI=1
  }else{
    scaleI=scale_mult
  }
  pop_per_val=popTrend3(t,pop_per1,pop_per2)
  endy=length(phiminusvals)
  phi = scaleI*extra_parms_vals[1]*I
  dum = Mdash[5,5]
  
  Mdash_nep=Mdash
  Mdash_ost=Mdash
  Mdash_nep_ost=Mdash
  # no NSP or OST
  Mdash[phiminusvals[1:(endy-1)]]=-phi
  Mdash[phiminusvals[endy]]=-phi+dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash[phiplussvals]=phi;
  # NEP matrix
  Mdash_nep[phiminusvals[1:(endy-1)]]=-phi*(1 - nep_rel_risk_red) 
  Mdash_nep[phiminusvals[endy]]=-phi*(1 - nep_rel_risk_red) +dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash_nep[phiplussvals]=phi*(1 - nep_rel_risk_red) 
  # OST matrix
  Mdash_ost[phiminusvals[1:(endy-1)]]=-phi*(1 - ost_rel_risk_red) 
  Mdash_ost[phiminusvals[endy]]=-phi*(1 - ost_rel_risk_red) +dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash_ost[phiplussvals]=phi*(1 - ost_rel_risk_red) 
  # NEP_OST matrix
  Mdash_nep_ost[phiminusvals[1:(endy-1)]]=-phi*(1 - nep_ost_rel_risk_red) 
  Mdash_nep_ost[phiminusvals[endy]]=-phi*(1 - nep_ost_rel_risk_red) +dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash_nep_ost[phiplussvals]=phi*(1 - nep_ost_rel_risk_red)  
  
  X00 = matrix(N[1:(20*9)],20,9)
  X01 = matrix(N[181:(180+20*9)],20,9)
  X10 = matrix(N[361:(360+20*9)],20,9)
  X11 = matrix(N[541:(540+20*9)],20,9)
  X10_nep = matrix(N[721:(720+20*9)],20,9)
  X11_nep = matrix(N[901:(900+20*9)],20,9)
  X10_ost = matrix(N[1081:(1080+20*9)],20,9)
  X11_ost = matrix(N[1261:(1260+20*9)],20,9)
  X10_nep_ost = matrix(N[1441:(1440+20*9)],20,9)
  X11_nep_ost = matrix(N[1621:(1620+20*9)],20,9)  

  extra_parms_vals_vec=c(extra_parms_vals[2],relapse2,rep(relapse3,7))
  #extra_parms_vals_vec=c(extra_parms_vals[2],extra_parms_vals[2],rep(extra_parms_vals[2],7))
  extra_parms_mat = t(matrix(rep(extra_parms_vals_vec,20),ncol=20))
  
  # collect the coverage into one term to remove correct amount of subjects
  pnepy=1-exp(-enroll_nep)
  posty=1-exp(-enroll_ost)
  pboty=1-exp(-enroll_nep_ost)
  cov_val = pnepy + posty + pboty # will be 1 or less
  #print(cov_val)
  cov_val_rate = -log(1-min(cov_val,0.99999))
  if (cov_val==0){
    cov_probs = c(0,0,0)
  }else{
    cov_probs = c(pnepy,posty,pboty)/cov_val
  }  
  d00=M%*%X00-extra_parms_mat*X00+extra_parms_vals[3]*X10-mort_former*X00+t(age_matrix%*%t(X00)) +
    idu_nep_cess*X10_nep+idu_ost_cess*X10_ost+idu_nep_ost_cess*X10_nep_ost
  d01=M%*%X01-extra_parms_mat*X01+extra_parms_vals[3]*X11-mort_former*X01+t(age_matrix%*%t(X01)) +
    idu_nep_cess*X11_nep+idu_ost_cess*X11_ost+idu_nep_ost_cess*X11_nep_ost
  d10=Mdash%*%X10+extra_parms_mat*X00-extra_parms_vals[3]*X10-mort_current*X10+t(age_matrix%*%t(X10))-
    cov_val_rate*X10 + dropout_nep*X10_nep  + dropout_ost*X10_ost  + dropout_nep_ost*X10_nep_ost
  d11=Mdash%*%X11+extra_parms_mat*X01-extra_parms_vals[3]*X11-mort_current*X11+t(age_matrix%*%t(X11))-
    cov_val_rate*X11 + dropout_nep*X11_nep  + dropout_ost*X11_ost + dropout_nep_ost*X11_nep_ost
  # Add two extra models for d10_nep and d11_nep
  # The NEP enrollment rate is enroll_nep, they can come from any of the d10 or d11 compartments
  d10_nep = cov_probs[1]*cov_val_rate*X10 +
    Mdash_nep%*%X10_nep-mort_current*X10_nep+t(age_matrix%*%t(X10_nep))-
    dropout_nep*X10_nep - idu_nep_cess*X10_nep
  d11_nep = cov_probs[1]*cov_val_rate*X11 +
    Mdash_nep%*%X11_nep-mort_current*X11_nep+t(age_matrix%*%t(X11_nep))-
    dropout_nep*X11_nep - idu_nep_cess*X11_nep
  d10_ost = cov_probs[2]*cov_val_rate*X10 +
    Mdash_ost%*%X10_ost-mort_current*X10_ost+t(age_matrix%*%t(X10_ost))-
    dropout_ost*X10_ost - idu_ost_cess*X10_ost
  d11_ost = cov_probs[2]*cov_val_rate*X11  +
    Mdash_ost%*%X11_ost-mort_current*X11_ost+t(age_matrix%*%t(X11_ost))-
    dropout_ost*X11_ost - idu_ost_cess*X11_ost
  d10_nep_ost = cov_probs[3]*cov_val_rate*X10  +
    Mdash_nep_ost%*%X10_nep_ost-mort_current*X10_nep_ost+t(age_matrix%*%t(X10_nep_ost))-
    dropout_nep_ost*X10_nep_ost - idu_nep_ost_cess*X10_nep_ost
  d11_nep_ost = cov_probs[3]*cov_val_rate*X11  +
    Mdash_nep_ost%*%X11_nep_ost-mort_current*X11_nep_ost+t(age_matrix%*%t(X11_nep_ost))-
    dropout_nep_ost*X11_nep_ost - idu_nep_ost_cess*X11_nep_ost
  
  d10[1,1] =d10[1,1] + sum(mort_former*X00) + sum(mort_former*X01) + sum(mort_current*X10) + sum(mort_current*X11) +
    sum(mort_current*X10_nep) + sum(mort_current*X11_nep) +
    sum(mort_current*X10_ost) + sum(mort_current*X11_ost) +
    sum(mort_current*X10_nep_ost) + sum(mort_current*X11_nep_ost) +
    death_vec[1]*sum(X00[12,])  + death_vec[2]*sum(X00[13,])  + death_vec[3]*sum(X00[14,])  + death_vec[4]*sum(X00[15,]) +
    death_vec[1]*sum(X01[12,])  + death_vec[2]*sum(X01[13,])  + death_vec[3]*sum(X01[14,])  + death_vec[4]*sum(X01[15,]) +
    death_vec[1]*sum(X10[12,])  + death_vec[2]*sum(X10[13,])  + death_vec[3]*sum(X10[14,])  + death_vec[4]*sum(X10[15,]) +
    death_vec[1]*sum(X11[12,])  + death_vec[2]*sum(X11[13,])  + death_vec[3]*sum(X11[14,])  + death_vec[4]*sum(X11[15,]) +  
    death_vec[1]*sum(X10_nep[12,])  + death_vec[2]*sum(X10_nep[13,])  + death_vec[3]*sum(X10_nep[14,])  + death_vec[4]*sum(X10_nep[15,]) +
    death_vec[1]*sum(X11_nep[12,])  + death_vec[2]*sum(X11_nep[13,])  + death_vec[3]*sum(X11_nep[14,])  + death_vec[4]*sum(X11_nep[15,]) +  
    death_vec[1]*sum(X10_ost[12,])  + death_vec[2]*sum(X10_ost[13,])  + death_vec[3]*sum(X10_ost[14,])  + death_vec[4]*sum(X10_ost[15,]) +
    death_vec[1]*sum(X11_ost[12,])  + death_vec[2]*sum(X11_ost[13,])  + death_vec[3]*sum(X11_ost[14,])  + death_vec[4]*sum(X11_ost[15,]) + 
    death_vec[1]*sum(X10_nep_ost[12,])  + death_vec[2]*sum(X10_nep_ost[13,])  + death_vec[3]*sum(X10_nep_ost[14,])  + death_vec[4]*sum(X10_nep_ost[15,]) +
    death_vec[1]*sum(X11_nep_ost[12,])  + death_vec[2]*sum(X11_nep_ost[13,])  + death_vec[3]*sum(X11_nep_ost[14,])  + death_vec[4]*sum(X11_nep_ost[15,])   
  d10[1,1:9]=d10[1,1:9]+pop_per_val*1000
  dum=treat_comps_pv7(N,scale00,scale10,t,pcom,alpha,time_ofint_inODEunits,
                      DAAtreat,SCreening,
                      enroll_nep,enroll_ost,enroll_nep_ost)
  # if (sum(dum$total_not_T4)>0){
  #   cat(paste0("treats",sum(dum$total_not_T4),"\n"))
  # }
  phi=dum$phi
  phidash=dum$phidash
  #print(paste0("sum phi < 0 ",sum(phi<0)," t=",t,"n"))
  #print(paste0("sum phidash < 0 ",sum(phidash<0)," t=",t,"n"))
  # These will actually be zero as only curren treated - left in for future
  
  pcom_former=pcom
  #print(paste0("sum phi ",sum(phi[1:180])," and sum phidash ",sum(phidash[1:180])))
  
  trtmodel00=-phi[1:180]+phidash[1:180]
  trtmodel01=(1-alpha*pcom_former)*phi[1:180] -
              phi[181:360]+phidash[181:360] + # This moves the chronic to treated compartments
              (1-alpha*pcom_former)*phi[181:360] # adds back in the failures
    
  trtmodel10=-phi[361:540]+phidash[361:540]
  trtmodel11=(1-alpha*pcom)*phi[361:540] -
             phi[541:720]+phidash[541:720]+
             (1-alpha*pcom)*phi[541:720]
  
  trtmodel10_nep=-phi[721:900]+phidash[721:900]
  trtmodel11_nep=(1-alpha*pcom)*phi[721:900] #treatment failures aplha*pcom is in phidash above
  
  trtmodel10_ost=-phi[1081:1260]+phidash[1081:1260]
  trtmodel11_ost=(1-alpha*pcom)*phi[1081:1260] #treatment failures aplha*pcom is in phidash above
  
  trtmodel10_nep_ost=-phi[1441:1620]+phidash[1441:1620]
  trtmodel11_nep_ost=(1-alpha*pcom)*phi[1441:1620] #treatment failures aplha*pcom is in phidash above
  
  return(list(c(as.vector(d00)+trtmodel00, as.vector(d01)+trtmodel01, as.vector(d10)+trtmodel10, as.vector(d11)+trtmodel11,
                as.vector(d10_nep)+trtmodel10_nep, as.vector(d11_nep)+trtmodel11_nep,
                as.vector(d10_ost)+trtmodel10_ost, as.vector(d11_ost)+trtmodel11_ost,
                as.vector(d10_nep_ost)+trtmodel10_nep_ost, as.vector(d11_nep_ost)+trtmodel11_nep_ost)))
  
}

plot_allcomps_v2nep=function(times,X,chronic_nums10){
  # add in the NEP current no failure X10_nep
  Irows = c(6,7,8,9,10,11,12,13,14,15)
  dum=c(matrix(rep(Irows,each=9),nrow=9)+matrix(seq(720,880,20),nrow=9,ncol=10),
        matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1080,1240,20),nrow=9,ncol=10),
        matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1440,1600,20),nrow=9,ncol=10))
  top10_index_nep = matrix(t(dum),nrow=90,ncol=1)
  y=100*(colSums(X[chronic_nums10,])+colSums(X[top10_index_nep,]))/(colSums(X[361:540,])+colSums(X[721:900,])+
                                                                      colSums(X[1081:1260,])+colSums(X[1441:1620,]))
  df=data.frame(times,y)
  p1=ggplot(data=subset(df,times>=72), aes(x=times+1950, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Percentage") + ggtitle("% of current PWID with HCV from 2022 to 2030")+
    scale_y_continuous(breaks = c(seq(0,70,10)),labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),lim=c(0,70))+
    scale_x_continuous(breaks = c(seq(2022,2030,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(list(p1=p1,t=times,Y=y))
}

infTrend_daa=function(t,fitType){
  # calculates the infection scaling factor
  if (fitType==2){
    scaling_factor=1
  }else{
    if (t<60){ # t is 1950+(0:140)
      scaling_factor=1
    } else if (t>72){
      scaling_factor=5.1515/(1+exp(-0.3160*((72+1950)-2013.6)))
    } else{
      scaling_factor=5.1515/(1+exp(-0.3160*((t+1950)-2013.6)))
    }
  }
  return(scaling_factor)
}


popTrend_daa=function(t,pop_per){
  # calculates the population scaling factor
  
  if (t<60){ # t is 1950+(0:140)
    scaling_factor=1
  } else{
    scaling_factor=pop_per/100
  }
  return(scaling_factor)
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

asode_nep_ost_daa_long_v2=function(param_vals,N,L){
  library(deSolve)
  
  
  init       = N
  parameters = param_vals
  times      = seq(0, 90, by = 1)
  lt=length(times)
  X = ode(y=init, times=times, func=MODEL_nep_ost_daa_long_v2, parms=parameters,method="ode45")
  X=t(X[,2:1801])
  a=NULL
  #a=plot_allcomps_v2nep(0:80,X,L$chronic10)
  return(list(a=a,X=X))
}

MODEL_nep_ost_daa_long_v2 <- function(time, state, parameters) {
  # Changed now uses the new trend
  
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
  nep_rel_risk_red_in=parameters[917];nepcov_in=parameters[918];nepdrop_in=parameters[919];idu_nep_cess_in=parameters[920]
  ost_rel_risk_red_in=parameters[921];ostcov_in=parameters[922];ostdrop_in=parameters[923];idu_ost_cess_in=parameters[924]
  nep_ost_rel_risk_red_in=parameters[925];nep_ostcov_in=parameters[926];nep_ostdrop_in=parameters[927];idu_nep_ost_cess_in=parameters[928]
  
  # intervention parameters
  scale00=parameters[929] # scale00 and scale10 are the former and current scaling factors
  scale10=parameters[930]
  pcom=parameters[931] # the probability of pwid completing treatment
  alpha=parameters[932] # alpha is the SVR probability
  time_ofint_inODEunits=parameters[933] # the year of intervention in the ODE time scale, 73 for 0:81 for 2022
  DAAtreat=data.frame(Pcurrenmild=parameters[934],Pcurrenadva=parameters[935])
  SCreening=data.frame(Ditimedur=parameters[936],FixedyCov=parameters[937])
  pop_per=parameters[938]
  pop_per1=parameters[939]
  pop_per2=parameters[940]
  scale_mult=parameters[941]
  t=time
  # These subjects go to the relevant former compartment
  if (t>=(time_ofint_inODEunits-1)) { # year 2022 and onwards
    enroll_nep=nepcov_in
    dropout_nep=-log(1-nepdrop_in)
    nep_rel_risk_red=nep_rel_risk_red_in
    idu_nep_cess=idu_nep_cess_in
    
    enroll_ost=ostcov_in
    dropout_ost=-log(1-ostdrop_in)
    ost_rel_risk_red=ost_rel_risk_red_in
    idu_ost_cess=idu_ost_cess_in    
    
    enroll_nep_ost=nep_ostcov_in
    dropout_nep_ost=-log(1-nep_ostdrop_in)    
    nep_ost_rel_risk_red=nep_ost_rel_risk_red_in
    idu_nep_ost_cess=idu_nep_ost_cess_in
    
    if  (SCreening$FixedyCov==0){
      # This removes all the prevention interventions
      nep_rel_risk_red=0
      enroll_nep=0
      dropout_nep=0
      idu_nep_cess=0
      
      ost_rel_risk_red=0
      enroll_ost=0
      dropout_ost=0
      idu_ost_cess=0
      
      nep_ost_rel_risk_red=0
      enroll_nep_ost=0
      dropout_nep_ost=0
      idu_nep_ost_cess=0
    }
    
  }else{
    nep_rel_risk_red=0
    enroll_nep=0
    dropout_nep=0
    idu_nep_cess=0
    
    ost_rel_risk_red=0
    enroll_ost=0
    dropout_ost=0
    idu_ost_cess=0
    
    nep_ost_rel_risk_red=0
    enroll_nep_ost=0
    dropout_nep_ost=0
    idu_nep_ost_cess=0
  }
  mort_current = t(matrix(rep(curr_mort_pwid,20),9,20))
  mort_former = t(matrix(rep(curr_mort_former,20),9,20))
  
  Irows = c(6,7,8,9,10,11,12,13,14,15) # rows of infectious PWID
  #death_vec= c(-log(1-death_rate_dc), -log(1-death_rate_hcc), -log(1-death_rate_lt1), -log(1- death_rate_lt2)); #DC,HCC,LT1,LT2
  # No already converted to rates in param_setup_daa and param_setup
  death_vec= c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  N=state
  # percentage PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(360,520,20),nrow=9,ncol=10)
  top10_index = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(540,700,20),nrow=9,ncol=10)
  top11_index =  matrix(t(dum),nrow=90,ncol=1)
  # add in the NEP PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(720,880,20),nrow=9,ncol=10)
  top10_index_nep = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(900,1060,20),nrow=9,ncol=10)
  top11_index_nep =  matrix(t(dum),nrow=90,ncol=1)
  # add in the OST PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1080,1240,20),nrow=9,ncol=10)
  top10_index_ost = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1260,1420,20),nrow=9,ncol=10)
  top11_index_ost =  matrix(t(dum),nrow=90,ncol=1)  
  # add in the NEP & OST PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1440,1600,20),nrow=9,ncol=10)
  top10_index_nep_ost = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(1620,1780,20),nrow=9,ncol=10)
  top11_index_nep_ost =  matrix(t(dum),nrow=90,ncol=1)  
  
  top = sum(N[top10_index])+sum(N[top11_index]) +
    sum(N[top10_index_nep])+sum(N[top11_index_nep]) +
    sum(N[top10_index_ost])+sum(N[top11_index_ost]) +
    sum(N[top10_index_nep_ost])+sum(N[top11_index_nep_ost])
  
  bot = sum(N[361:length(N)]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  I = top/bot;
  if (t<60){
    scaleI=1
  }else{
    scaleI=scale_mult
  }
  pop_per_val=popTrend3(t,pop_per1,pop_per2)
  endy=length(phiminusvals)
  phi = scaleI*extra_parms_vals[1]*I
  dum = Mdash[5,5]
  
  Mdash_nep=Mdash
  Mdash_ost=Mdash
  Mdash_nep_ost=Mdash
  # no NSP or OST
  Mdash[phiminusvals[1:(endy-1)]]=-phi
  Mdash[phiminusvals[endy]]=-phi+dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash[phiplussvals]=phi;
  # NEP matrix
  Mdash_nep[phiminusvals[1:(endy-1)]]=-phi*(1 - nep_rel_risk_red) 
  Mdash_nep[phiminusvals[endy]]=-phi*(1 - nep_rel_risk_red) +dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash_nep[phiplussvals]=phi*(1 - nep_rel_risk_red) 
  # OST matrix
  Mdash_ost[phiminusvals[1:(endy-1)]]=-phi*(1 - ost_rel_risk_red) 
  Mdash_ost[phiminusvals[endy]]=-phi*(1 - ost_rel_risk_red) +dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash_ost[phiplussvals]=phi*(1 - ost_rel_risk_red) 
  # NEP_OST matrix
  Mdash_nep_ost[phiminusvals[1:(endy-1)]]=-phi*(1 - nep_ost_rel_risk_red) 
  Mdash_nep_ost[phiminusvals[endy]]=-phi*(1 - nep_ost_rel_risk_red) +dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash_nep_ost[phiplussvals]=phi*(1 - nep_ost_rel_risk_red)  
  
  X00 = matrix(N[1:(20*9)],20,9)
  X01 = matrix(N[181:(180+20*9)],20,9)
  X10 = matrix(N[361:(360+20*9)],20,9)
  X11 = matrix(N[541:(540+20*9)],20,9)
  X10_nep = matrix(N[721:(720+20*9)],20,9)
  X11_nep = matrix(N[901:(900+20*9)],20,9)
  X10_ost = matrix(N[1081:(1080+20*9)],20,9)
  X11_ost = matrix(N[1261:(1260+20*9)],20,9)
  X10_nep_ost = matrix(N[1441:(1440+20*9)],20,9)
  X11_nep_ost = matrix(N[1621:(1620+20*9)],20,9)  
  
 # extra_parms_vals_vec=c(extra_parms_vals[2],relapse2,rep(relapse3,7))
  extra_parms_vals_vec=c(extra_parms_vals[2],extra_parms_vals[2],rep(extra_parms_vals[2],7))
  extra_parms_mat = t(matrix(rep(extra_parms_vals_vec,20),ncol=20))
  
 
  
  d00=M%*%X00-extra_parms_mat*X00+extra_parms_vals[3]*X10-mort_former*X00+t(age_matrix%*%t(X00)) +
    idu_nep_cess*X10_nep+idu_ost_cess*X10_ost+idu_nep_ost_cess*X10_nep_ost
  d01=M%*%X01-extra_parms_mat*X01+extra_parms_vals[3]*X11-mort_former*X01+t(age_matrix%*%t(X01)) +
    idu_nep_cess*X11_nep+idu_ost_cess*X11_ost+idu_nep_ost_cess*X11_nep_ost
  d10=Mdash%*%X10+extra_parms_mat*X00-extra_parms_vals[3]*X10-mort_current*X10+t(age_matrix%*%t(X10))-
    enroll_nep*X10 + dropout_nep*X10_nep - enroll_ost*X10 + dropout_ost*X10_ost - enroll_nep_ost*X10 + dropout_nep_ost*X10_nep_ost
  d11=Mdash%*%X11+extra_parms_mat*X01-extra_parms_vals[3]*X11-mort_current*X11+t(age_matrix%*%t(X11))-
    enroll_nep*X11 + dropout_nep*X11_nep - enroll_ost*X11 + dropout_ost*X11_ost - enroll_nep_ost*X11 + dropout_nep_ost*X11_nep_ost
  
  
  # d00=M%*%X00-extra_parms_vals[2]*X00+extra_parms_vals[3]*X10-mort_former*X00+t(age_matrix%*%t(X00)) +
  #   idu_nep_cess*X10_nep+idu_ost_cess*X10_ost+idu_nep_ost_cess*X10_nep_ost
  # d01=M%*%X01-extra_parms_vals[2]*X01+extra_parms_vals[3]*X11-mort_former*X01+t(age_matrix%*%t(X01)) +
  #   idu_nep_cess*X11_nep+idu_ost_cess*X11_ost+idu_nep_ost_cess*X11_nep_ost
  # d10=Mdash%*%X10+extra_parms_vals[2]*X00-extra_parms_vals[3]*X10-mort_current*X10+t(age_matrix%*%t(X10))-
  #   enroll_nep*X10 + dropout_nep*X10_nep - enroll_ost*X10 + dropout_ost*X10_ost - enroll_nep_ost*X10 + dropout_nep_ost*X10_nep_ost
  # d11=Mdash%*%X11+extra_parms_vals[2]*X01-extra_parms_vals[3]*X11-mort_current*X11+t(age_matrix%*%t(X11))-
  #   enroll_nep*X11 + dropout_nep*X11_nep - enroll_ost*X11 + dropout_ost*X11_ost - enroll_nep_ost*X11 + dropout_nep_ost*X11_nep_ost
  # Add two extra models for d10_nep and d11_nep
  # The NEP enrollment rate is enroll_nep, they can come from any of the d10 or d11 compartments
  d10_nep = enroll_nep*X10 +
    Mdash_nep%*%X10_nep-mort_current*X10_nep+t(age_matrix%*%t(X10_nep))-
    dropout_nep*X10_nep - idu_nep_cess*X10_nep
  d11_nep = enroll_nep*X11 +
    Mdash_nep%*%X11_nep-mort_current*X11_nep+t(age_matrix%*%t(X11_nep))-
    dropout_nep*X11_nep - idu_nep_cess*X11_nep
  d10_ost = enroll_ost*X10 +
    Mdash_ost%*%X10_ost-mort_current*X10_ost+t(age_matrix%*%t(X10_ost))-
    dropout_ost*X10_ost - idu_ost_cess*X10_ost
  d11_ost = enroll_ost*X11 +
    Mdash_ost%*%X11_ost-mort_current*X11_ost+t(age_matrix%*%t(X11_ost))-
    dropout_ost*X11_ost - idu_ost_cess*X11_ost
  d10_nep_ost = enroll_nep_ost*X10 +
    Mdash_nep_ost%*%X10_nep_ost-mort_current*X10_nep_ost+t(age_matrix%*%t(X10_nep_ost))-
    dropout_nep_ost*X10_nep_ost - idu_nep_ost_cess*X10_nep_ost
  d11_nep_ost = enroll_nep_ost*X11 +
    Mdash_nep_ost%*%X11_nep_ost-mort_current*X11_nep_ost+t(age_matrix%*%t(X11_nep_ost))-
    dropout_nep_ost*X11_nep_ost - idu_nep_ost_cess*X11_nep_ost
  
  d10[1,1] =d10[1,1] + sum(mort_former*X00) + sum(mort_former*X01) + sum(mort_current*X10) + sum(mort_current*X11) +
    sum(mort_current*X10_nep) + sum(mort_current*X11_nep) +
    sum(mort_current*X10_ost) + sum(mort_current*X11_ost) +
    sum(mort_current*X10_nep_ost) + sum(mort_current*X11_nep_ost) +
    death_vec[1]*sum(X00[12,])  + death_vec[2]*sum(X00[13,])  + death_vec[3]*sum(X00[14,])  + death_vec[4]*sum(X00[15,]) +
    death_vec[1]*sum(X01[12,])  + death_vec[2]*sum(X01[13,])  + death_vec[3]*sum(X01[14,])  + death_vec[4]*sum(X01[15,]) +
    death_vec[1]*sum(X10[12,])  + death_vec[2]*sum(X10[13,])  + death_vec[3]*sum(X10[14,])  + death_vec[4]*sum(X10[15,]) +
    death_vec[1]*sum(X11[12,])  + death_vec[2]*sum(X11[13,])  + death_vec[3]*sum(X11[14,])  + death_vec[4]*sum(X11[15,]) +  
    death_vec[1]*sum(X10_nep[12,])  + death_vec[2]*sum(X10_nep[13,])  + death_vec[3]*sum(X10_nep[14,])  + death_vec[4]*sum(X10_nep[15,]) +
    death_vec[1]*sum(X11_nep[12,])  + death_vec[2]*sum(X11_nep[13,])  + death_vec[3]*sum(X11_nep[14,])  + death_vec[4]*sum(X11_nep[15,]) +  
    death_vec[1]*sum(X10_ost[12,])  + death_vec[2]*sum(X10_ost[13,])  + death_vec[3]*sum(X10_ost[14,])  + death_vec[4]*sum(X10_ost[15,]) +
    death_vec[1]*sum(X11_ost[12,])  + death_vec[2]*sum(X11_ost[13,])  + death_vec[3]*sum(X11_ost[14,])  + death_vec[4]*sum(X11_ost[15,]) + 
    death_vec[1]*sum(X10_nep_ost[12,])  + death_vec[2]*sum(X10_nep_ost[13,])  + death_vec[3]*sum(X10_nep_ost[14,])  + death_vec[4]*sum(X10_nep_ost[15,]) +
    death_vec[1]*sum(X11_nep_ost[12,])  + death_vec[2]*sum(X11_nep_ost[13,])  + death_vec[3]*sum(X11_nep_ost[14,])  + death_vec[4]*sum(X11_nep_ost[15,])   
  d10[1,1:9]=d10[1,1:9]+pop_per_val*1000
  dum=treat_comps_pv7(N,scale00,scale10,t,pcom,alpha,time_ofint_inODEunits,
                      DAAtreat,SCreening,
                      enroll_nep,enroll_ost,enroll_nep_ost)
  phi=dum$phi
  phidash=dum$phidash
  #print(paste0("sum phi < 0 ",sum(phi<0)," t=",t,"n"))
  #print(paste0("sum phidash < 0 ",sum(phidash<0)," t=",t,"n"))
  # These will actually be zero as only curren treated - left in for future
  print(sum(phi))
  print(paste0("phi is ",sum(phi)," phidash is ",sum(phidash)))
  trtmodel00=-phi[1:180]+phidash[1:180]
  trtmodel01=(1-alpha)*phi[1:180] # treatment failures,alpha is in phidash above
  trtmodel10=-phi[361:540]+phidash[361:540]
  trtmodel11=(1-alpha*pcom)*phi[361:540] #treatment failures aplha*pcom is in phidash above
  
  trtmodel10_nep=-phi[721:900]+phidash[721:900]
  trtmodel11_nep=(1-alpha*pcom)*phi[721:900] #treatment failures aplha*pcom is in phidash above
  
  trtmodel10_ost=-phi[1081:1260]+phidash[1081:1260]
  trtmodel11_ost=(1-alpha*pcom)*phi[1081:1260] #treatment failures aplha*pcom is in phidash above
  
  trtmodel10_nep_ost=-phi[1441:1620]+phidash[1441:1620]
  trtmodel11_nep_ost=(1-alpha*pcom)*phi[1441:1620] #treatment failures aplha*pcom is in phidash above
  
  return(list(c(as.vector(d00)+trtmodel00, as.vector(d01)+trtmodel01, as.vector(d10)+trtmodel10, as.vector(d11)+trtmodel11,
                as.vector(d10_nep)+trtmodel10_nep, as.vector(d11_nep)+trtmodel11_nep,
                as.vector(d10_ost)+trtmodel10_ost, as.vector(d11_ost)+trtmodel11_ost,
                as.vector(d10_nep_ost)+trtmodel10_nep_ost, as.vector(d11_nep_ost)+trtmodel11_nep_ost)))
  
}
pickupDAA_efficacy=function(){
  conn_param <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = 'www/HCV.db'
  )
  strsql="
  select value,'p1' as 'val'  from parameter_data where Parameter=='Proportion completing treatment'
  UNION
  select value,'p2' as 'val'  from parameter_data where Parameter=='Proportion treated who reach SVR' and Stratum=='Current PWID'
  UNION
  select value,'p3' as 'val'  from parameter_data where Parameter=='Proportion treated who reach SVR' and Stratum=='Former PWID'
   order by val"
  params=dbGetQuery(conn_param, strsql)
  dbDisconnect(conn_param)
  return(list(pcom=params$Value[1],alpha_current=params$Value[2],alpha_foregin=params$Value[3]))
}

plot_output_mortality_v2_all_special =function(XT,death_vec,scale00,scale10,time_cut,t1,pcom,alpha,DAAtreat,SCreening,enroll_nep,enroll_ost,enroll_nep_ost,typey){
  #t1,XT,scale00,scale10,pcom,alpha,time_ofint_inODEunits,DAAtreat,SCreening,
  #enroll_nep,enroll_ost,enroll_nep_ost
  # death_vec=c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  death_vals10=rep(0,4)
  death_vals00=rep(0,4)
  death_vals11=rep(0,4)
  death_vals01=rep(0,4)
  time_cut=time_cut-1
  time_end=dim(XT)[2]
  XTdeaths=rep(0,time_end-time_cut+1)
  # Now can have deaths in the failed treatment compartments
  testcompvec00=t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec10=t(matrix(360+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec01_failed=t(matrix(c(seq(192,360,20),seq(193,360,20),seq(194,360,20),seq(195,360,20)),nrow=9,ncol=4))
  testcompvec11_failed=t(matrix(c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
  
  # need to add back in deaths for DC/HCC and LT
  addbackindeaths00=matrix(data = 0, nrow = 4, ncol=ncol(XT)) # rows are deaths in DC/HCC/LT1/LT2
  addbackindeaths01=matrix(data = 0, nrow = 4, ncol=ncol(XT))
  addbackindeaths10=matrix(data = 0, nrow = 4, ncol=ncol(XT))
  addbackindeaths11=matrix(data = 0, nrow = 4, ncol=ncol(XT))
  for (i in 1 : ncol(XT)){
    N=XT[,i]
    dum=treat_comps_pv7(N,scale00,scale10,t1[i],pcom,alpha,time_cut,
                        DAAtreat,SCreening,
                        enroll_nep,enroll_ost,enroll_nep_ost)
    if (i > 73){
      addbackindeaths00[,i]=dum$direct_deaths[1,]
      addbackindeaths01[,i]=dum$direct_deaths[2,]
      addbackindeaths10[,i]=dum$direct_deaths[3,]
      addbackindeaths11[,i]=dum$direct_deaths[4,]
    }
  }
  
  death_vec=1-exp(-death_vec)
  Xbot_deaths=0
  tval = time_cut-6
  for (i in 1 : 4){
    Xtest=death_vec[i]*scale10*colSums(XT[testcompvec10[i,],])
    Xtest=Xtest + death_vec[i]*addbackindeaths10[i,]
    Ytest=death_vec[i]*scale00*colSums(XT[testcompvec00[i,],])
    Ytest=Ytest + death_vec[i]*addbackindeaths00[i,]
    X1test=death_vec[i]*scale10*colSums(XT[testcompvec11_failed[i,],])
    X1test=X1test + death_vec[i]*addbackindeaths11[i,]
    Y1test=death_vec[i]*scale00*colSums(XT[testcompvec01_failed[i,],])
    Y1test=Y1test + death_vec[i]*addbackindeaths01[i,]
    
    XTdeaths=XTdeaths+Xtest[time_cut:time_end]+Ytest[time_cut:time_end]+
      X1test[time_cut:time_end]+Y1test[time_cut:time_end]
    Xbot_deaths = Xbot_deaths + Xtest[tval]+Ytest[tval]+
      X1test[tval]+Y1test[tval]
    
  }
  
  
  
  
  
  
  #print(XTdeaths)
  if (typey==1){
    df= data.frame(x=-1+1950+(time_cut:time_end),y=100*((XTdeaths/Xbot_deaths) - 1)) # XTdeaths[1]
    titly="% change in liver related mortality (DCC,HCC,LT) compared to 2015"
    ylaby="Change %"
    ywholim=-65
    minylim= -65*105.5/100
  }else{
    df= data.frame(x=-1+1950+(time_cut:time_end),y=XTdeaths)
    titly="Number of liver related deaths (DCC,HCC,LT)"
    ylaby="# deaths"
    ywholim = 0.35*Xbot_deaths
    minylim = 0.95*ywholim
  }
  
  maxy=max(pretty_breaks()(c(0, df$y)))
  miny=min(min(pretty_breaks()(c(0, df$y))),minylim)
  vecy=pretty_breaks()(c(miny, df$y))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab(ylaby) + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(miny,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(1950+time_cut-1,1950+time_end-1,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_hline(yintercept = ywholim,color="blue",size=2) + 
    annotate("text", min(1950+time_cut+5), ywholim, vjust = -1, label = "WHO mortality target")
  return(p1)
}

