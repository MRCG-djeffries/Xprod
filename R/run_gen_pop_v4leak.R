run_gen_pop_v4leak=function(treat_cov_mild,treat_cov_severe,
                        screening_cov,screening_diagnosis_duration,
                        ignore_screening,time_ofint_inODEunits,runy,
                        version_num,
                        pcom=0.892,alpha=0.95,
                        delta=0.26, rateA_F0=52/12,
                        treatduration=52/16.56){
  # fits to endemic levels
  library(deSolve)
  library(scales)
  library(cowplot)
  library(ggplot2)
  library(gridExtra)
  # set up values from database to rds files
  L=pickup_from_gen_db(0.1,0.1,0.1,
                       0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0)
  if (runy==1){
    Nint<<-fitN_v2(1,0.2)
    solveGA_US_gen_pop(version_num)
    #dbDisconnect(conn)
    x=readRDS(paste0("www/genpop_fit_raw_v",version_num,".rds"))
    param_vals=param_setup_gen_pop(x,1)
    dum=runmodel_fit_genpop(param_vals,x,0,0,0,1,0,6,0,0,0)
    saveRDS(dum$X,paste0("www/genpop_notreat_v",version_num,".rds")) # This is gen pop X matrix without treats form 2016 to 2050
    return()
  }else{
    x=readRDS(paste0("www/genpop_fit_raw_v",version_num,".rds"))
    param_vals=param_setup_gen_pop(x,0)
    #dbDisconnect(conn)
    Xnotreat=readRDS(paste0("www/genpop_notreat_v",version_num,".rds")) # baseline
  }
  # increased by 10% as in general pop
  pcom=(1+0.0)*pcom
  alpha=(1+0.0)*alpha
  dum=runmodel_fit_genpop(param_vals,x,
                          treat_cov_mild,treat_cov_severe,
                          screening_cov,screening_diagnosis_duration,
                          ignore_screening,time_ofint_inODEunits,
                          pcom,alpha,treatduration)
  L=coeffsof_v5()
  p1=plotstacked_v3_noscaling(dum$X,L)
  p2=plotPWID_noscaled(dum$X,L,6,1)
  p3= plot_prev_v4_noscaling(dum$X,L,6)
  delta=-param_vals[6]/param_vals[106] # the spontaneous recovery rate
  acuterate=-param_vals[106]
  curr_mort_pwid=param_vals[895:903]
  curr_mort_former=param_vals[904:912]
  p4= plotincidence_v2_change_noscaling(dum$X,L,6,delta,acuterate,curr_mort_pwid,curr_mort_former)
  p5= plotincidence_v2_noscaling(dum$X,L,6,delta,acuterate,curr_mort_pwid,curr_mort_former)
  
  plotchronic_num=plot_output_chronic_all_genpop(0:25,L,dum$X,7,1)
  plotinci=plot_output_incidence_new_chronic_cases_v2_all_genpop(dum$X,dum$delta,dum$rA_F0,7,
                                                                 L$A_comps_00,L$A_comps_01)
  plottreats=plot_output_treats_v2_all_genpop(0:24,dum$X,pcom,alpha,7,treat_cov_mild,treat_cov_severe,
                                              screening_cov,screening_diagnosis_duration,
                                              ignore_screening)
  # start here
  plotmort=plot_output_mortality_v2_all_genpop(dum$X,1-exp(-param_vals[913:916]),7)
  plotcasepertreat=plot_output_cases_averted_all_genpop(0:34,L,Xnotreat,dum$X,pcom,alpha,7,treat_cov_mild,treat_cov_severe,
                                                        screening_cov,screening_diagnosis_duration,
                                                        ignore_screening)
  plotinci_num = plotincidence_v2_noscaling(dum$X,L,7,delta,acuterate,curr_mort_pwid,curr_mort_former)
  plotmort_num = plot_output_mortality_v2_all_num_genpop(dum$X,1-exp(-param_vals[913:916]),7)
  plotstages=p1
  plotattackrate=p3
  plotattackrate_per_treat=plot_attack_rate_v_treat_all_genpop(0:34,L,Xnotreat,dum$X,pcom,alpha,7,treat_cov_mild,treat_cov_severe,
                                                               screening_cov,screening_diagnosis_duration,
                                                               ignore_screening)
  plotprevent_count=plot_output_screened_count_genpop(0:90,dum$X,7)
  # p11=plot_allcomps_v3(0:90,X,scale10,L)
  # p12=plot_output_pwid_all(0:90,L,X,scale00,scale10,TreatmentStartTime,1)
  
  return(list(p1=p1,p2=p2,p3=p3,p4=p4,p5=p5,
              plotchronic_num=plotchronic_num,
              plotinci=plotinci,
              plottreats=plottreats,
              plotcasepertreat=plotcasepertreat,
              plotmort=plotmort,
              plotstages=plotstages,
              plotattackrate=plotattackrate,
              plotattackrate_per_treat=plotattackrate_per_treat,
              plotprevent_count=plotprevent_count,
              plotinci_num=plotinci_num,
              plotmort_num=plotmort_num
  ))
  
  
  # 2016+(0:74), 2016 to 2090
  # p11=plot_areacomps_gen(5:14,dum$XT) # 5:14 is 2021:2030
  # p0_long=plot_output_chronic(0:74,dum$XT,2)
  # p0_short=plot_output_chronic(0:74,dum$XT,1)
  # p1=plot_output_cases_averted_gen_pop(0:74,Xnotreat,dum$XT,pcom,alpha,time_ofint_inODEunits,
  #                                      treat_cov_mild,treat_cov_severe,
  #                                      screening_cov,screening_diagnosis_duration,
  #                                      ignore_screening)
  # p2=plot_output_treats_gen_pop(0:74,dum$XT,pcom,alpha,time_ofint_inODEunits,
  #                               treat_cov_mild,treat_cov_severe,
  #                               screening_cov,screening_diagnosis_duration,
  #                               ignore_screening)
  # p9=plot_attack_rate_v_treat_all_gen_pop(0:74,Xnotreat,dum$XT,pcom,alpha,time_ofint_inODEunits,
  # treat_cov_mild,treat_cov_severe,
  # screening_cov,screening_diagnosis_duration,
  # ignore_screening)
  # p10=plot_attack_rate_gen_pop(0:74,dum$XT)
  # # note delta and rA_F0 changed
  # p3=plot_output_incidence_new_chronic_cases_gen_pop(dum$XT,x[2],x[3],time_ofint_inODEunits,1)
  # p4=plot_output_incidence_new_chronic_cases_gen_pop(dum$XT,x[2],x[3],time_ofint_inODEunits,2)
  # parameters=param_setup_gen_pop(rep(0,11)) # args just place holders here , need death rates
  # death_rate_dc=parameters[913]
  # death_rate_hcc=parameters[914]
  # death_rate_lt1=parameters[915]
  # death_rate_lt2=parameters[916]
  # deaths= c(death_rate_dc, death_rate_hcc, death_rate_lt1, death_rate_lt2); #DC,HCC,LT1,LT2
  # 
  # p5=plot_output_mortality_gen_pop(dum$XT,deaths,time_ofint_inODEunits,1)
  # p6=plot_output_mortality_gen_pop(dum$XT,deaths,time_ofint_inODEunits,2)
  # p7=plot_output_screened_gen_pop(dum$XT,screening_cov,time_ofint_inODEunits,1)
  # p8=plot_output_prev_screened_gen_pop(dum$XT,screening_cov,time_ofint_inODEunits,1)
  # p12_short=plot_output_gen_total(dum$XT,time_ofint_inODEunits,1)
  # p12_long=plot_output_gen_total(dum$XT,time_ofint_inODEunits,2)
  # return(list(case_averted_gen_pop=p1,number_of_treats=p2,incidence_reduction=p3,incidence_reduction_long=p4,
  #             mortality_reduction=p5,mortality_reduction_long=p6,
  #             year_screened=p7, prev_screened=p8,attack_rate_two_axis=p9,chronic_long=p0_long,
  #             chronic_short=p0_short,attack_rate=p10,chronic_stages=p11,
  #             model_all_short=p12_short,model_all_long=p12_long))
}


solveGA_US_gen_pop = function(version_num){
  # 15 March fits three and five parameter model to the US data
  # 12 April updated  error in dates
  #death_vec= c(-log(1-death_rate_dc), -log(1-death_rate_hcc), -log(1-death_rate_lt1), -log(1- death_rate_lt2)); #DC,HCC,LT1,LT2
  # *************** TO *********************
  #death_vec= c(death_rate_dc, death_rate_hcc, death_rate_lt1, death_rate_lt2); #DC,HCC,LT1,LT2
  #
  library(GA)
  library(deSolve)
  x=readRDS(paste0("www/genpop_fit_raw_v",version_num,".rds"))
  svals =c(0.02,rep(0.01,10))
  evals=c(0.1,0.3,rep(0.2,9))
  GA <- ga(type = "real-valued", fitness = asode_US_genpop, lower = svals, upper = evals,monitor=TRUE,
           parallel=FALSE,suggestions=x,maxiter=100)
  #saveRDS(GA,paste0("www/genpop_fit_raw_v",version_num,".rds"))
  saveRDS(GA@solution,paste0("www/genpop_fit_raw_v",version_num,".rds")) # 0.03794405
}
asode_US_genpop=function(x){
  # In general population 20 compartments for each age group
  # only S0, A, F0-F4,DCC,HCC and LT1 and LT2 have subjects
  #paste0("here")
  A_comps_00=seq(6,180,20)
  S_comps_00=seq(1,180,20)
  F0_comps_00 = seq(7,180,20)
  F1_comps_00 = seq(8,180,20)
  F2_comps_00 = seq(9,180,20)
  F3_comps_00 = seq(10,180,20)
  F4_comps_00 = seq(11,180,20)
  DCC_comps_00 = seq(12,180,20)
  HCC_comps_00 = seq(13,180,20)
  LT1_comps_00 = seq(14,180,20)
  LT2_comps_00 = seq(15,180,20)
  
  chronic00=c(F0_comps_00,F1_comps_00,F2_comps_00,F3_comps_00,F4_comps_00,DCC_comps_00,HCC_comps_00,LT1_comps_00,LT2_comps_00)
  #x=readRDS("www/genpop_fit_raw_v3.rds")
  param_vals=param_setup_gen_pop(x,1)
  init       = Nint # global
  parameters = param_vals
  #times      = seq(0, 10, by = 1)
  times      = seq(0, 74, by = 1)
  lt=length(times)
  X = ode(y=init, times=times, func=MODEL_US_gen_pop, parms=parameters,method="ode45")
  X=t(X[,2:181])
  
  lt2=colSums(X[LT2_comps_00,])
  lt1=colSums(X[LT1_comps_00,])
  lt=lt1+lt2;
  dc=colSums(X[DCC_comps_00,])
  hcc=colSums(X[HCC_comps_00,])
  f4=colSums(X[F4_comps_00,])
  f3=colSums(X[F3_comps_00,])
  f2=colSums(X[F2_comps_00,])
  f1=colSums(X[F1_comps_00,])
  f0=colSums(X[F0_comps_00,])
  # tx=10:15
  tot = f0+f1+f2+f3+f4+hcc+dc+lt
  # m=mean(tot[tx])
  # f=-sum((tot[tx]-m)^2) # rss
  #plot(tot,type="l")
  
  tx=1:74
  fout2=rep(0,74)
  for (i in 1:74){
    fout2[i]= (f0[i]+f1[i]+f2[i]+f3[i]+f4[i]+hcc[i]+dc[i]+lt[i]);
  }
  
  b=lm(fout2~tx)[[1]][2]
  #z=colSums(X[seq(6,180,20),])/6000
  f=-b^2# +(z-1)^2
  return(f)
}



MODEL_US_gen_pop <- function(time, state, parameters) {
  
  Mvec=parameters[1:400]
  M=t(matrix(Mvec,nrow=20,ncol=20))
  Mdashvec=parameters[401:800]
  #Mdash=t(matrix(Mdashvec,nrow=20,ncol=20))
  Mdash=t(matrix(Mvec,nrow=20,ncol=20))
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
  
  mort_current = t(matrix(rep(curr_mort_pwid,20),9,20))
  mort_former = t(matrix(rep(curr_mort_former,20),9,20))
  
  Irows = c(6,7,8,9,10,11,12,13,14,15) # rows of infectious PWID
  #death_vec= c(-log(1-death_rate_dc), -log(1-death_rate_hcc), -log(1-death_rate_lt1), -log(1- death_rate_lt2)); #DC,HCC,LT1,LT2
  death_vec= c(death_rate_dc, death_rate_hcc, death_rate_lt1, death_rate_lt2); #DC,HCC,LT1,LT2
  t=time
  N=state
  # percentage PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(0,160,20),nrow=9,ncol=10)
  top00_index = matrix(t(dum),nrow=90,ncol=1)
  top = sum(N[top00_index]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  bot = sum(N[1:length(N)]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  I = top/bot;
  
  # if (t>=0 && t<=63){
  #   scaleI = 1
  # }else if (t>63 & t<=64){
  #   scaleI = 1;
  # }else if (t>64 & t<=65){
  #   scaleI = 1.8/1.6;
  # }else if (t>65 & t<=66){
  #   scaleI = 1.9/1.6;
  # }else if (t>66 & t<=67){
  #   scaleI = 2.2/1.6;
  # }else if (t>67 & t<=68){
  #   scaleI = 2.3/1.6;
  # }else if (t>68 & t<=69){
  #   scaleI = 2.6/1.6;
  # }else{
  #   scaleI=2.6/1.6;
  # }
  scaleI=1
  endy=length(phiminusvals)
  phi = scaleI*extra_parms_vals[1]*I
  dum = Mdash[5,5]
  Mdash[phiminusvals[1:(endy-1)]]=-phi
  Mdash[phiminusvals[endy]]=-phi+dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash[phiplussvals]=phi;
  
  X00 = matrix(N[1:(20*9)],20,9)
  
  # with deaths set as zero - this should be closed
  # age movement also set as zero
  # there are treatment so no failed treatment
  # so only D10 and D00
  # only for the youngest age group since no transition
  
  #d00=Mdash%*%X00
  
  d00=Mdash%*%X00-mort_former*X00+t(age_matrix%*%t(X00))
  d00[1,1] =d00[1,1] + sum(mort_former*X00) +
    death_vec[1]*sum(X00[12,])  + death_vec[2]*sum(X00[13,])  +
    death_vec[3]*sum(X00[14,])  + death_vec[4]*sum(X00[15,])
  return(list(c(as.vector(d00))))
  
}

test_for_leak=function(parameters,state){
  Mvec=parameters[1:400]
  M=t(matrix(Mvec,nrow=20,ncol=20))
  Mdashvec=parameters[401:800]
  #Mdash=t(matrix(Mdashvec,nrow=20,ncol=20))
  Mdash=t(matrix(Mvec,nrow=20,ncol=20))
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
  
  mort_current = t(matrix(rep(curr_mort_pwid,20),9,20))
  mort_former = t(matrix(rep(curr_mort_former,20),9,20))
  
  Irows = c(6,7,8,9,10,11,12,13,14,15) # rows of infectious PWID
  #death_vec= c(-log(1-death_rate_dc), -log(1-death_rate_hcc), -log(1-death_rate_lt1), -log(1- death_rate_lt2)); #DC,HCC,LT1,LT2
  death_vec= c(death_rate_dc, death_rate_hcc, death_rate_lt1, death_rate_lt2); #DC,HCC,LT1,LT2
  #t=time
  N=state
  # percentage PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(0,160,20),nrow=9,ncol=10)
  top00_index = matrix(t(dum),nrow=90,ncol=1)
  top = sum(N[top00_index]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  bot = sum(N[1:length(N)]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  I = top/bot;
  scaleI=1
  endy=length(phiminusvals)
  phi = scaleI*extra_parms_vals[1]*I
  dum = Mdash[5,5]
  Mdash[phiminusvals[1:(endy-1)]]=-phi
  Mdash[phiminusvals[endy]]=-phi+dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash[phiplussvals]=phi;
  
  X00 = matrix(N[1:(20*9)],20,9)
  age_matrix=matrix(data = 0,nrow=9,ncol=9)
  age_matrix[1,1] = -1/5;
  age_matrix[2,1] = 1/5;age_matrix[2,2] = -1/5;
  age_matrix[3,2] = 1/5;age_matrix[3,3] = -1/5;
  age_matrix[4,3] = 1/5;age_matrix[4,4] = -1/10;
  age_matrix[5,4] = 1/10;age_matrix[5,5] = -1/10;
  age_matrix[6,5] = 1/10;age_matrix[6,6] = -1/10;
  age_matrix[7,6] = 1/10;age_matrix[7,7] = -1/10;
  age_matrix[8,7] = 1/10;age_matrix[8,8] = -1/10;
  age_matrix[9,8] = 1/10;
  # with deaths set as zero - this should be closed
  # age movement also set as zero
  # there are treatment so no failed treatment
  # so only D10 and D00
  # only for the youngest age group since no transition
  
  d00=Mdash%*%X00-mort_former*X00+t(age_matrix%*%t(X00))
  d00[1,1] =d00[1,1] + sum(mort_former*X00) +      
    death_vec[1]*sum(X00[12,])  + death_vec[2]*sum(X00[13,])  + 
    death_vec[3]*sum(X00[14,])  + death_vec[4]*sum(X00[15,]) 
  return(list(c(as.vector(d00))))
}

param_setup_gen_pop=function(x,runy){
  r6=x[1]; # infection rate to solve
  rspont_clear = x[2] # was 0.26
  r3=x[3] # F0 to F1 former
  r2=r3; # not actually used only using former, holder
  rf1_f2=x[4]
  rf2_f3=x[5]
  rf3_f4=x[6]
  rf4_dcc=x[7]
  rf4_hcc=x[8]
  rdcc_hcc=x[9]
  rdcc_lt=x[10]
  rhcc_lt=x[11]
  # just place holders not used
  r5=0.027 # default values
  r4=17
  
  # rf1_f2=0.074
  # rf2_f3=0.106
  # rf3_f4=0.105
  # rf4_dcc=0.037
  # rf4_hcc=0.01
  # rdcc_hcc=0.068
  # rdcc_lt=0.033
  # rhcc_lt=0.1
  # This creates two matrices for the static parameters
  # M is for former i=0
  # Mdash is for current i=1, i.e. PWID
  parm_names = c('delta','r_AF0','w','r_SVR4DC','r_SVR4HCC','r_F0F1','r_F1F2','r_F2F3','r_F3F4','r_F4DC','r_F4HCC',
                 'r_DCHCC','r_DCLT','r_DCdeath','r_HCCLT','r_HCCdeath',
                 'r_LT1death','r_LT2death')
  L=pickup_from_gen_db(rspont_clear,r2,r3,
                       rf1_f2,rf2_f3,rf3_f4,rf4_dcc,rf4_hcc,rdcc_hcc,rdcc_lt,rhcc_lt,runy)
  parm_current=L$parm_current
  parm_former=L$parm_former
  pwid_vec=unlist(L$curr_mort_pwid)
  form_vec=unlist(L$curr_mort_former)
  
  # parm_current=rep(0,18)
  # parm_former=rep(0,18)
  # parm_current[1]=rspont_clear; parm_former[1] = parm_current[1]; # spontaneous clearance
  # parm_current[2]=52/param_df$p2; parm_former[2] = parm_current[2];# acute duration is 12 weeks
  # parm_current[3]=0; parm_former[3] = parm_current[3];# duration to return to susceptible after treated - no treats for burden model
  # parm_current[4]=0; parm_former[4] = parm_current[4];# SVR4 to DC
  # parm_current[5]=0; parm_former[5] = parm_current[5];# SVR4 to HCC
  # # note the current are not actually used
  # parm_current[6] = -log(1- r2 ); parm_former[6] = -log(1-r3); # F0 to F1 current then former
  # 
  # parm_current[7] = -log(1- param_df$p5 ); parm_former[7] = -log(1- rf1_f2); # F1 to F2 current then former
  # parm_current[8] = -log(1-param_df$p7); parm_former[8] = -log(1-rf2_f3); # F2 to F3 current then former
  # parm_current[9] = -log(1-param_df$p9); parm_former[9] = -log(1-rf3_f4); # F3 to F4 current then former
  # 
  # parm_current[10] = -log(1-param_df$p11); parm_former[10] = -log(1-rf4_dcc); # F4 to DC current then former
  #                                                             # notice this
  # parm_current[11] = -log(1-param_df$p12); parm_former[11] = -log(1-rf4_dcc); # F4 to HCC current then former
  # parm_current[12] = -log(1-param_df$p13); parm_former[12] = -log(1-rdcc_hcc); # DC to HCC
  # parm_current[13] = -log(1-param_df$p14); parm_former[13] = -log(1-rdcc_lt); # DC to LT
  # parm_current[15] = -log(1-param_df$p15); parm_former[15] = -log(1-rhcc_lt); # HCC to LT
  # 
  # parm_current[14] = -log(1-param_df$p_16) ; parm_former[14] = parm_current[14]; # DC to death 
  # parm_current[16] = -log(1-param_df$p_17); parm_former[16] = parm_current[16]; # HCC death
  # parm_current[17] = -log(1-param_df$p_18); parm_former[17] = parm_current[17]; # LT to death year 1
  # parm_current[18] = -log(1-param_df$p_19); parm_former[18] = parm_current[18]; # LT to death year 2
  
  phi = 0;# place holder
  
  Mdash=matrix(data=0,nrow=20,ncol=20);
  phiminusvals1=1;phiminusvals2=22;phiminusvals3=43;phiminusvals4=64;phiminusvals5=85
  phiplussvals1=6;phiplussvals2=28;phiplussvals3=49;phiplussvals4=70;phiplussvals5=91
  Mdash[1,1]=-phi;Mdash[1,6]=parm_current[1]*parm_current[2];Mdash[1,16]=parm_current[3];
  Mdash[2,2]=-phi;Mdash[2,17]=parm_current[3];
  Mdash[3,3]=-phi;Mdash[3,18]=parm_current[3];
  Mdash[4,4]=-phi;Mdash[4,19]=parm_current[3];
  Mdash[5,5]=-phi-parm_current[4]-parm_current[5];Mdash[5,20]=parm_current[3];
  Mdash[6,1]=phi;Mdash[6,6]=-parm_current[2];
  Mdash[7,6]=(1-parm_current[1])*parm_current[2];Mdash[7,7]=-parm_current[6];
  Mdash[8,2]=phi;Mdash[8,7]=parm_current[6];Mdash[8,8]=-parm_current[7];
  Mdash[9,3]=phi;Mdash[9,8]=parm_current[7];Mdash[9,9]=-parm_current[8];
  Mdash[10,4]=phi;Mdash[10,9]=parm_current[8];Mdash[10,10]=-parm_current[9];
  Mdash[11,5]=phi;Mdash[11,10]=parm_current[9];Mdash[11,11]=-parm_current[10]-parm_current[11];
  # note error in Scott paper
  # Mdash[12,11]=parm_current[10]+parm_current[4];
  # Mdash[13,11]=parm_current[11]+parm_current[5];
  Mdash[12,11]=parm_current[10];Mdash[12,5]=parm_current[4];
  Mdash[13,11]=parm_current[11];Mdash[13,5]=parm_current[5];
  Mdash[12,12] = -parm_current[12]-parm_current[13]-parm_current[14];
  Mdash[13,12]=parm_current[12];Mdash[13,13]=-parm_current[15]-parm_current[16];
  Mdash[14,12]=parm_current[13];Mdash[14,13]=parm_current[15];Mdash[14,14]=-1-parm_current[17];
  Mdash[15,14]=1;Mdash[15,15]=-parm_current[18];
  Mdash[16,16]=-parm_current[3];
  Mdash[17,17]=-parm_current[3];
  Mdash[18,18]=-parm_current[3];
  Mdash[19,19]=-parm_current[3];
  Mdash[20,20]=-parm_current[3];
  
  
  M=matrix(data= 0,nrow=20,ncol=20);
  M[1,1]=-phi;M[1,6]=parm_former[1]*parm_former[2];M[1,16]=parm_former[3];
  M[2,2]=-phi;M[2,17]=parm_former[3];
  M[3,3]=-phi;M[3,18]=parm_former[3];
  M[4,4]=-phi;M[4,19]=parm_former[3];
  M[5,5]=-phi-parm_former[4]-parm_former[5];M[5,20]=parm_former[3];
  M[6,1]=phi;M[6,6]=-parm_former[2];
  M[7,6]=(1-parm_former[1])*parm_former[2];M[7,7]=-parm_former[6];
  M[8,2]=phi;M[8,7]=parm_former[6];M[8,8]=-parm_former[7];
  M[9,3]=phi;M[9,8]=parm_former[7];M[9,9]=-parm_former[8];
  M[10,4]=phi;M[10,9]=parm_former[8];M[10,10]=-parm_former[9];
  M[11,5]=phi;M[11,10]=parm_former[9];M[11,11]=-parm_former[10]-parm_former[11];
  # note error in Scott paper
  # M[12,11]=parm_former[10]+parm_former[4];
  # M[13,11]=parm_former[11]+parm_former[5];
  M[12,11]=parm_former[10];M[12,5]=parm_former[4];
  M[13,11]=parm_former[11];M[13,5]=parm_former[5];
  
  M[12,12] = -parm_former[12]-parm_former[13]-parm_former[14];
  M[13,12]=parm_former[12];M[13,13]=-parm_former[15]-parm_former[16];
  M[14,12]=parm_former[13];M[14,13]=parm_former[15];M[14,14]=-1-parm_former[17];
  M[15,14]=1;M[15,15]=-parm_former[18];
  M[16,16]=-parm_former[3];
  M[17,17]=-parm_former[3];
  M[18,18]=-parm_former[3];
  M[19,19]=-parm_former[3];
  M[20,20]=-parm_former[3];
  
  age_matrix=matrix(data = 0,nrow=9,ncol=9)
  age_matrix[1,1] = -1/5;
  age_matrix[2,1] = 1/5;age_matrix[2,2] = -1/5;
  age_matrix[3,2] = 1/5;age_matrix[3,3] = -1/5;
  age_matrix[4,3] = 1/5;age_matrix[4,4] = -1/10;
  age_matrix[5,4] = 1/10;age_matrix[5,5] = -1/10;
  age_matrix[6,5] = 1/10;age_matrix[6,6] = -1/10;
  age_matrix[7,6] = 1/10;age_matrix[7,7] = -1/10;
  age_matrix[8,7] = 1/10;age_matrix[8,8] = -1/10;
  age_matrix[9,8] = 1/10;
  
  #curr_mort_pwid=c(0.96 ,0.96 ,1.12 ,0.18 ,0.22 ,0.53 ,1.38 ,4.28 ,14.96)/1000; # mortality per year for PWID
  #curr_mort_former=c(0.044 ,0.051 ,0.062 ,0.1 ,0.222 ,0.534 ,1.376 ,4.282 ,14.956 )/1000; # mortality per year for PWID
  curr_mort_pwid=pwid_vec/1000; # mortality per year for PWID
  curr_mort_former=form_vec/1000; # mortality per year for PWID
  
  mort_current = t(matrix(rep(curr_mort_pwid,20),ncol=20))
  mort_former = t(matrix(rep(curr_mort_former,20),ncol=20))
  extra_parms_nams=c('piv','relapse','nu'); # infection rate (piv instead of pi), relapse to IDU, 1/duration of injecting span
  
  extra_parms_vals=c(r6,-log(1-r5),1/r4) # was 5.6%, nu was 1/17
  rout=c(r6,r5,r4,r2,r3)
  
  # starting values t = 1950:2030
  # S001 = 560 # formwer PWID compartment 161
  # S101 = 400 # current PWID compartment 361
  # F101 = 40 # acutely infected with HCV  366 (A is comp 6 in X list)
  N0=matrix(nrow=20*9*4,ncol=1);
  N0[1]=560;
  N0[361]=396;
  N0[367]=44;
  Mvec=as.vector(t(M))
  Mdashvec=as.vector(t(Mdash))
  Magevec=as.vector(t(age_matrix))
  
  death_rate_dc=L$death_rate_dc
  death_rate_hcc=L$death_rate_hcc
  death_rate_lt1=L$death_rate_lt1
  death_rate_lt2=L$death_rate_lt2
  
  param_vals=c(Mvec,Mdashvec,extra_parms_vals,
               phiminusvals1,phiminusvals2,phiminusvals3,phiminusvals4,phiminusvals5,
               phiplussvals1,phiplussvals2,phiplussvals3,phiplussvals4,phiplussvals5,
               Magevec,
               curr_mort_pwid,curr_mort_former,
               death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  
  return(param_vals)
  
}

sql_pickup_gen=function(rspont_clear,r2,r3,
                        rf1_f2,rf2_f3,rf3_f4,rf4_dcc,rf4_hcc,rdcc_hcc,rdcc_lt,rhcc_lt,runy){
  
  strsql="select value,'p1' as 'val'  from parameter_data where Parameter=='Spontaneous clearance'
  UNION
  select value,'p2' as 'val'  from parameter_data where Parameter=='Acute infection to mild (F0)'
  UNION
  select value,'p3' as 'val'  from parameter_data where Parameter=='F0 to F1' and Stratum=='Current PWID'
  UNION
  select value,'p4' as 'val'  from parameter_data where Parameter=='F0 to F1' and Stratum=='Former PWID'
  UNION
  select value,'p5' as 'val'  from parameter_data where Parameter=='F1 to F2' and Stratum=='Current PWID'
  UNION
  select value,'p6' as 'val'  from parameter_data where Parameter=='F1 to F2' and Stratum=='Former PWID'
  UNION
  select value,'p7' as 'val'  from parameter_data where Parameter=='F2 to F3' and Stratum=='Current PWID'
  UNION
  select value,'p8' as 'val'  from parameter_data where Parameter=='F2 to F3' and Stratum=='Former PWID'
  UNION
  select value,'p9' as 'val'  from parameter_data where Parameter=='F3 to F4' and Stratum=='Current PWID'
  UNION
  select value,'p_10' as 'val'  from parameter_data where Parameter=='F3 to F4' and Stratum=='Former PWID'
  UNION
  select value,'p_11' as 'val'  from parameter_data where Parameter=='F4 to DC'
  UNION
  select value,'p_12' as 'val'  from parameter_data where Parameter=='F4 to HCC'
  UNION
  select value,'p_13' as 'val'  from parameter_data where Parameter=='DC to HCC'
  UNION
  select value,'p_14' as 'val'  from parameter_data where Parameter=='DC to LT'
  UNION
  select value,'p_15' as 'val'  from parameter_data where Parameter=='HCC to LT'
  UNION
  select value,'p_16' as 'val'  from parameter_data where Parameter=='DC to death'
  UNION
  select value,'p_17' as 'val'  from parameter_data where Parameter=='HCC to death'
  UNION
  select value,'p_18' as 'val'  from parameter_data where Parameter=='LT Year 1 to death'
  UNION
  select value,'p_19' as 'val'  from parameter_data where Parameter=='LT Year 2 to death'
  order by val"
  if (runy==1){
    TT=dbGetQuery(conn, strsql)
    param_df=data.frame(t(TT$Value))
    colnames(param_df)=unlist(TT$val)    
    saveRDS(param_df,file="www/fitparam_df.rds")
  } else {
    param_df=readRDS(file="www/fitparam_df.rds")
  }
  param_df$p1 = rspont_clear
  param_df$p3 = r2
  param_df$p4 = r3
  
  param_df$p5 = rf1_f2
  param_df$p6 = rf2_f3
  param_df$p7 = rf3_f4
  
  param_df$p8 = rf4_dcc
  param_df$p9 = rf4_hcc # note should be rf4_hcc
  
  param_df$p_10 = rdcc_hcc
  param_df$p_11 = rdcc_lt
  param_df$p_12 = rhcc_lt
  
  if (runy==1){
    strsql="select value from parameter_data where Variable=='Non liver related mortality' and Stratum=='Current PWID' order by vnum"
    pwid_vec=dbGetQuery(conn, strsql)
    strsql="select value from parameter_data where Variable=='Non liver related mortality' and Stratum=='Former PWID' order by vnum"
    form_vec=dbGetQuery(conn, strsql)
    strsql="select value from parameter_data where Parameter=='S4 to DC' or Parameter=='S4 to HCC' order by Parameter"
    s4_vec=dbGetQuery(conn, strsql)
    saveRDS(pwid_vec,file="www/fitpwid_vec.rds")
    saveRDS(form_vec,file="www/fitform_vec.rds")
    saveRDS(s4_vec,file="www/fits4_vec.rds")
    #dbDisconnect(conn)
  }else{
    pwid_vec = readRDS(file="www/fitpwid_vec.rds")
    form_vec = readRDS(file="www/fitform_vec.rds")
    s4_vec = readRDS(file="www/fits4_vec.rds")
  }
  #
  return(list(param_df=param_df,pwid_vec=pwid_vec,form_vec=form_vec,s4_vec=s4_vec))
}

pickup_from_gen_db=function(rspont_clear,r2,r3,
                            rf1_f2,rf2_f3,rf3_f4,rf4_dcc,rf4_hcc,rdcc_hcc,rdcc_lt,rhcc_lt,runy){
  # this picks up all the parameters directly from the sqlite database
  L= sql_pickup_gen(rspont_clear,r2,r3,
                    rf1_f2,rf2_f3,rf3_f4,rf4_dcc,rf4_hcc,rdcc_hcc,rdcc_lt,rhcc_lt,runy)

  param_df=L$param_df
  pwid_vec=unlist(L$pwid_vec)
  form_vec=unlist(L$form_vec)
  parm_current=rep(0,18) # not used place holder
  parm_former=rep(0,18)
  parm_current[1]=param_df$p1 # spontaneous recovery rate
  parm_current[2]=52/param_df$p2
  parm_former[1] = parm_current[1]; # spontaneous clearance
  parm_former[2] = parm_current[2];# acute duration is 12 weeks
  parm_current[3]=0; parm_former[3] = parm_current[3];# duration to return to susceptible after treated - no treats for burden model
  parm_current[4]=L$s4_vec$Value[1]; parm_former[4] = parm_current[4];# SVR4 to DC
  parm_current[5]=L$s4_vec$Value[2]; parm_former[5] = parm_current[5];# SVR4 to HCC
  parm_current[6] = -log(1- param_df$p3 ); parm_former[6] = -log(1- param_df$p4); # F0 to F1 current then former
  
  parm_current[7] = -log(1- param_df$p5 ); parm_former[7] = -log(1- param_df$p5); # F1 to F2 current then former
  parm_current[8] = -log(1- param_df$p6);  parm_former[8] = -log(1- param_df$p6); # F2 to F3 current then former
  parm_current[9] = -log(1- param_df$p7);  parm_former[9] = -log(1- param_df$p7); # F3 to F4 current then former
  
  parm_current[10] = -log(1- param_df$p8); parm_former[10] = -log(1- param_df$p8); # F4 to DC current then former
  parm_current[11] = -log(1- param_df$p9); parm_former[11] = -log(1- param_df$p9); # F4 to HCC current then former
  parm_current[12] = -log(1- param_df$p_10); parm_former[12] = -log(1- param_df$p_10); # DC to HCC
  parm_current[13] = -log(1- param_df$p_11); parm_former[13] = -log(1- param_df$p_11); # DC to LT
  parm_current[15] = -log(1- param_df$p_12); parm_former[15] = -log(1- param_df$p_12); # HCC to LT
  parm_current[14] = -log(1- param_df$p_16) ; parm_former[14] = parm_current[14]; # DC to death 
  parm_current[16] = -log(1- param_df$p_17); parm_former[16] = parm_current[16]; # HCC death
  parm_current[17] = -log(1- param_df$p_18); parm_former[17] = parm_current[17]; # LT to death year 1
  parm_current[18] = -log(1- param_df$p_19); parm_former[18] = parm_current[18]; # LT to death year 2
  death_rate_dc=  -log(1- param_df$p_16) 
  death_rate_hcc= -log(1- param_df$p_17) 
  death_rate_lt1= -log(1- param_df$p_18) 
  death_rate_lt2= -log(1- param_df$p_19) 
  curr_mort_pwid=pwid_vec; # mortality per year for PWID
  curr_mort_former=form_vec; # mortality per year for PWID
  return(list(parm_current=parm_current,
              parm_former=parm_former,
              death_rate_dc=death_rate_dc,death_rate_hcc=death_rate_hcc,
              death_rate_lt1=death_rate_lt1,death_rate_lt2=death_rate_lt2,
              curr_mort_pwid=curr_mort_pwid,
              curr_mort_former=curr_mort_former))
}

runmodel_fit_genpop=function(param_vals,x,
                             treat_cov_mild,treat_cov_severe,
                             screening_cov,screening_diagnosis_duration,
                             ignore_screening,time_ofint_inODEunits,
                             pcom,alpha,treatduration){
  #x are the 11 calibrated parameters
  # rin ar ethe extra parameters
  #treat_cov_mild,treat_cov_severe, coverage RATES for F0-F2 and F3-F4
  #screening_cov,screening_diagnosis_duration
  #ignore_screening =1, fit the whole treat coverage immediately otherwise zero
  #param_vals=param_setup_gen_pop(x)
  Nint=fitN_v2(1,0.2)
  init       = c(Nint,rep(0,180))
  parameters = c(param_vals,treat_cov_mild,treat_cov_severe,
                 screening_cov,screening_diagnosis_duration,
                 ignore_screening,
                 pcom,alpha,(time_ofint_inODEunits-1),treatduration)
  test_for_leak(parameters,Nint)
  rA_F0= -param_vals[106]
  delta=param_vals[6]/rA_F0
  times      = seq(0, 34, by = 1)
  lt=length(times)
  X = ode(y=init[1:180], times=times, func=MODEL_US_gen_pop, parms=parameters,method="ode45") #_with_treat
  #X = ode(y=init, times=times, func=MODEL_US_gen_pop_with_treat, parms=parameters,method="ode45") #
  X=t(X[,2:361])
  return(list(XT=X,X=X,rA_F0=rA_F0,delta=delta))
}


MODEL_US_gen_pop_with_treat <- function(time, state, parameters) {
  
  Mvec=parameters[1:400]
  M=t(matrix(Mvec,nrow=20,ncol=20))
  Mdashvec=parameters[401:800]
  #Mdash=t(matrix(Mdashvec,nrow=20,ncol=20))
  Mdash=t(matrix(Mvec,nrow=20,ncol=20))
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
  
  treat_cov_mild=parameters[917]
  treat_cov_severe=parameters[918]
  screening_cov=parameters[919]
  screening_diagnosis_duration=parameters[920]
  ignore_screening=parameters[921]
  pcom=parameters[922]
  alpha=parameters[923]
  time_ofint_inODEunits=parameters[924]
  treatduration=parameters[925]
  Mdash[1,16]=treatduration
  Mdash[2,17]=treatduration
  Mdash[3,18]=treatduration
  Mdash[4,19]=treatduration
  Mdash[5,20]=treatduration
  Mdash[16,16]=-treatduration
  Mdash[17,17]=-treatduration
  Mdash[18,18]=-treatduration
  Mdash[19,19]=-treatduration
  Mdash[20,20]=-treatduration
  
  mort_current = t(matrix(rep(curr_mort_pwid,20),9,20))
  mort_former = t(matrix(rep(curr_mort_former,20),9,20))
  
  Irows = c(6,7,8,9,10,11,12,13,14,15) # rows of infectious PWID
  #death_vec= c(-log(1-death_rate_dc), -log(1-death_rate_hcc), -log(1-death_rate_lt1), -log(1- death_rate_lt2)); #DC,HCC,LT1,LT2
  death_vec= c(death_rate_dc, death_rate_hcc, death_rate_lt1, death_rate_lt2); #DC,HCC,LT1,LT2
  t=time
  N=state
  # percentage PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(0,160,20),nrow=9,ncol=10)
  top00_index = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(180,340,20),nrow=9,ncol=10)
  top00_index_S = matrix(t(dum),nrow=90,ncol=1)
  top = sum(N[top00_index]) + sum(N[top00_index_S])# 40 is j = 0, 60 is j = 1 and i = 1 for both
  bot = sum(N[1:length(N)]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  I = top/bot;
  
  # if (t>=0 && t<=63){
  #   scaleI = 1
  # }else if (t>63 & t<=64){
  #   scaleI = 1;
  # }else if (t>64 & t<=65){
  #   scaleI = 1.8/1.6;
  # }else if (t>65 & t<=66){
  #   scaleI = 1.9/1.6;
  # }else if (t>66 & t<=67){
  #   scaleI = 2.2/1.6;
  # }else if (t>67 & t<=68){
  #   scaleI = 2.3/1.6;
  # }else if (t>68 & t<=69){
  #   scaleI = 2.6/1.6;
  # }else{
  #   scaleI=2.6/1.6;
  # }
  scaleI=1
  endy=length(phiminusvals)
  phi = scaleI*extra_parms_vals[1]*I
  dum = Mdash[5,5]
  Mdash[phiminusvals[1:(endy-1)]]=-phi
  Mdash[phiminusvals[endy]]=-phi+dum; #dum = -r_svr4DC-rsvr4HCC
  Mdash[phiplussvals]=phi;
  
  if (t>=(time_ofint_inODEunits-1)) { # year 2022 and onwards
    enroll_S=screening_cov
    if  (ignore_screening==1){
      enroll_S=0
    }
  }else{
    enroll_S=0
  }
  
  X00 = matrix(N[1:(20*9)],20,9)
  X00_S = matrix(N[181:(180+20*9)],20,9)
  
  # with deaths set as zero - this should be closed
  # age movement also set as zero
  # there are treatment so no failed treatment
  # so only D10 and D00
  # only for the youngest age group since no transition
  
  d00=Mdash%*%X00-mort_former*X00+t(age_matrix%*%t(X00))-enroll_S*X00
  d00_S=enroll_S*X00+Mdash%*%X00_S-mort_former*X00_S+t(age_matrix%*%t(X00_S))
  
  d00[1,1] =d00[1,1] + sum(mort_former*X00) +      
    death_vec[1]*sum(X00[12,])  + death_vec[2]*sum(X00[13,])  + 
    death_vec[3]*sum(X00[14,])  + death_vec[4]*sum(X00[15,])  +
    sum(mort_former*X00_S) +
    death_vec[1]*sum(X00_S[12,])  + death_vec[2]*sum(X00_S[13,])  + 
    death_vec[3]*sum(X00_S[14,])  + death_vec[4]*sum(X00_S[15,])  
  
  
  
  # now deal with the treatments
  #print(paste0("N=",sum(N)," and t=",t))
  dum=treat_comps_pv7_gen_pop(N,t,pcom,alpha,time_ofint_inODEunits,
                              treat_cov_mild,treat_cov_severe,
                              enroll_S,screening_diagnosis_duration,
                              ignore_screening)
  #print(paste0("N=",dum$phi[1:180]," and t=",t))
  #print(paste0("N=",dum$phi[181:360]," and t=",t))
  #print(paste0("N=",sum(N[181:360])," and t=",t))
  
  trtmodel00=-pcom*alpha*dum$phi[1:180]+dum$phidash[1:180]
  trtmodel00_S=-pcom*alpha*dum$phi[181:360]+dum$phidash[181:360]
  #print(sum(as.vector(d00)+as.vector(d00_S)))
  return(list(c(as.vector(d00)+trtmodel00,as.vector(d00_S)+trtmodel00_S)))
  
}

treat_comps_pv7_gen_pop=function(X,t,pcom,alpha,time_ofint_inODEunits,
                                 treat_cov_mild,treat_cov_severe,
                                 screening_cov,screening_diagnosis_duration,
                                 ignore_screening){
  # The enrollment rates are required as they define the screening effort within NEP/OST
  # Define the intervention populations for DAA
  
  # These are screened, 1 -180 is the general model and 181 to 360 are those
  # that have moved to screening
  F0_screen = seq(7,180,20) +180
  F1_screen = seq(8,180,20) +180
  F2_screen = seq(9,180,20) +180
  F3_screen = seq(10,180,20) +180
  F4_screen = seq(11,180,20) +180
  
  r_10=c(F3_screen,F4_screen) # advanced stages former
  c_10=c(F0_screen ,F1_screen, F2_screen)
  
  phi=rep(0,360)
  
  # Total number of subjects
  Mild=sum(X[c_10])
  Ad=sum(X[r_10])
  
  
  if (t>=time_ofint_inODEunits){
    #print(Mild)
    # this is the number of treatments available
    dum=move_DAA_v3_gen_pop(X,treat_cov_mild,treat_cov_severe,
                            screening_cov,screening_diagnosis_duration,
                            ignore_screening,
                            Mild,Ad,
                            F0_screen,F1_screen,F2_screen,F3_screen,F4_screen,
                            pcom,alpha)
  }else{
    # no treatments so values are zero (9 age groups for the totals)
    dum=data.frame(phi=phi,phidash=phi,total_not_T4=rep(0,9),total_T4=rep(0,9))
  }
  return(list(phi=dum$phi,phidash=dum$phidash,total_not_T4=dum$total_not_T4,total_T4=dum$total_T4))
  
}

move_DAA_v2_gen_pop=function(X,treat_cov_mild,treat_cov_severe,
                             screening_cov,screening_diagnosis_duration,
                             ignore_screening,
                             Mild,Ad,
                             F0_screen,F1_screen,F2_screen,F3_screen,F4_screen,
                             pcom,alpha){
  
  # convert rates to probability
  Ptreat_cov_mild=1-exp(-treat_cov_mild) 
  Ptreat_cov_severe=1-exp(-treat_cov_severe)
  screening_cov=1-exp(-screening_cov)
  
  # This is the maximum number in each intervention
  N_Mild=Ptreat_cov_mild*Mild
  N_Ad=Ptreat_cov_severe*Ad
  phi=rep(0,360)
  
  if (ignore_screening==1){
    # ignores prevention measures
    multy = 1
  } else {
    multy=(1-exp(-1/screening_diagnosis_duration)) # no screening cov as already moved to the screened compartments
  }
  if (Mild>0){   
    phi[F0_screen]= N_Mild*multy*(sum(X[F0_screen])/Mild)*(X[F0_screen]/sum(X[F0_screen]))
    phi[F1_screen]=N_Mild*multy*(sum(X[F1_screen])/Mild)*(X[F1_screen]/sum(X[F1_screen]))
    phi[F2_screen]=N_Mild*multy*(sum(X[F2_screen])/Mild)*(X[F2_screen]/sum(X[F2_screen]))
  }
  if (Ad>0){ 
    phi[F3_screen]=N_Ad*multy*(sum(X[F3_screen])/Ad)*(X[F3_screen]/sum(X[F3_screen]))
    phi[F4_screen]=N_Ad*multy*(sum(X[F4_screen])/Ad)*(X[F4_screen]/sum(X[F4_screen]))
  }
  
  phidash=rep(0,360);
  T0_screen = seq(16,180,20)+180
  T1_screen = seq(17,180,20)+180
  T2_screen = seq(18,180,20)+180
  T3_screen = seq(19,180,20)+180
  T4_screen = seq(20,180,20)+180
  
  phidash[T0_screen]=alpha*pcom*phi[F0_screen]
  phidash[T1_screen]=alpha*pcom*phi[F1_screen]
  phidash[T2_screen]=alpha*pcom*phi[F2_screen]
  phidash[T3_screen]=alpha*pcom*phi[F3_screen]
  phidash[T4_screen]=alpha*pcom*(phi[F4_screen])
  
  
  # These are used to count the number of treatments
  total_not_T4 = phi[F0_screen]+phi[F1_screen]+phi[F2_screen]+phi[F3_screen]
  total_T4=phi[F4_screen]    
  
  return(list(phi=phi,phidash=phidash,total_not_T4=total_not_T4,total_T4=total_T4))
}

move_DAA_v3_gen_pop=function(X,treat_cov_mild,treat_cov_severe,
                             screening_cov,screening_diagnosis_duration,
                             ignore_screening,
                             Mild_screen,Ad_screen,
                             F0_screen,F1_screen,F2_screen,F3_screen,F4_screen,
                             pcom,alpha){
  
  F0 = seq(7,180,20) 
  F1 = seq(8,180,20) 
  F2 = seq(9,180,20) 
  F3 = seq(10,180,20) 
  F4 = seq(11,180,20)
  r=c(F3,F4) # advanced stages former
  c=c(F0 ,F1, F2)
  # Total number of subjects
  Mild=sum(X[c])
  Ad=sum(X[r])
  
  tot= sum(X[1:360])
  # convert to treatments in mild and advanced
  ntreat_nmild=(1-exp(-treat_cov_mild) )*tot
  ntreat_nadva=(1-exp(-treat_cov_severe))*tot
  # N_Mild=(1-exp(-treat_cov_mild) )*Mild
  # N_Ad=1-exp(-treat_cov_severe)()*Ad
  phi=rep(0,360)
  ntreats=c(ntreat_nmild,ntreat_nadva)
  if (ignore_screening==1){
    # ignores prevention measures
    if (Mild>0){   
      nDistributedTreats=min(ntreats[1],Mild)
      phi[F0]=nDistributedTreats*(sum(X[F0])/Mild)*(X[F0]/sum(X[F0]))
      phi[F1]=nDistributedTreats*(sum(X[F1])/Mild)*(X[F1]/sum(X[F1]))
      phi[F2]=nDistributedTreats*(sum(X[F2])/Mild)*(X[F2]/sum(X[F2]))
    }
    if (Ad>0){ 
      nDistributedTreats=min(ntreats[2],Ad)
      phi[F3]=nDistributedTreats*(sum(X[F3])/Ad)*(X[F3]/sum(X[F3]))
      phi[F4]=nDistributedTreats*(sum(X[F4])/Ad)*(X[F4]/sum(X[F4]))
    }
    phidash=rep(0,360);
    T0= seq(16,180,20)
    T1 = seq(17,180,20)
    T2 = seq(18,180,20)
    T3 = seq(19,180,20)
    T4 = seq(20,180,20)
    
    phidash[T0]=alpha*pcom*phi[F0]
    phidash[T1]=alpha*pcom*phi[F1]
    phidash[T2]=alpha*pcom*phi[F2]
    phidash[T3]=alpha*pcom*phi[F3]
    phidash[T4]=alpha*pcom*phi[F4]
  } else {
    mult_N_Mild_screen=Mild_screen*(1-exp(-1/screening_diagnosis_duration))#*(1-exp(-screening_cov))
    mult_N_Ad_screen=Ad_screen*(1-exp(-1/screening_diagnosis_duration))#*(1-exp(-screening_cov)) they are already screened
    if (Mild_screen>0){   
      nDistributedTreats=min(ntreats[1],mult_N_Mild_screen)
      phi[F0_screen]= nDistributedTreats*(sum(X[F0_screen])/Mild_screen)*(X[F0_screen]/sum(X[F0_screen]))
      phi[F1_screen]=nDistributedTreats*(sum(X[F1_screen])/Mild_screen)*(X[F1_screen]/sum(X[F1_screen]))
      phi[F2_screen]=nDistributedTreats*(sum(X[F2_screen])/Mild_screen)*(X[F2_screen]/sum(X[F2_screen]))
    }
    if (Ad_screen>0){ 
      nDistributedTreats=min(ntreats[2],mult_N_Ad_screen)
      phi[F3_screen]=nDistributedTreats*(sum(X[F3_screen])/Ad_screen)*(X[F3_screen]/sum(X[F3_screen]))
      phi[F4_screen]=nDistributedTreats*(sum(X[F4_screen])/Ad_screen)*(X[F4_screen]/sum(X[F4_screen]))
    }
    phidash=rep(0,360);
    T0_screen = seq(16,180,20)+180
    T1_screen = seq(17,180,20)+180
    T2_screen = seq(18,180,20)+180
    T3_screen = seq(19,180,20)+180
    T4_screen = seq(20,180,20)+180
    
    phidash[T0_screen]=alpha*pcom*phi[F0_screen]
    phidash[T1_screen]=alpha*pcom*phi[F1_screen]
    phidash[T2_screen]=alpha*pcom*phi[F2_screen]
    phidash[T3_screen]=alpha*pcom*phi[F3_screen]
    phidash[T4_screen]=alpha*pcom*(phi[F4_screen])
  }
  
  
  
  
  
  # These are used to count the number of treatments
  total_not_T4 = phi[F0]+phi[F1]+phi[F2]+phi[F3]+
    phi[F0_screen]+phi[F1_screen]+phi[F2_screen]+phi[F3_screen]
  total_T4=phi[F4]+phi[F4_screen]    
  
  return(list(phi=phi,phidash=phidash,total_not_T4=total_not_T4,total_T4=total_T4))
}

plot_output_cases_averted_gen_pop=function(t1,X,XT,pcom,alpha,time_cut,
                                           treat_cov_mild,treat_cov_severe,
                                           screening_cov,screening_diagnosis_duration,
                                           ignore_screening){
  a=7:15
  chronic00=c(a,a+20,a+40,a+60,a+80,a+100,a+120,a+140,a+160)
  beforetrt=colSums(X[c(chronic00),])
  aftertrt= colSums(XT[c(chronic00,chronic00+180),])
  
  t=time_cut
  tend = time_cut+9
  # total number of treatments
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    dum=treat_comps_pv7_gen_pop(N,t1[i],pcom,alpha,time_cut,
                                treat_cov_mild,treat_cov_severe,
                                screening_cov,screening_diagnosis_duration,
                                ignore_screening)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tdum=2016+(time_cut:(tend-1))
  ydum=totaltestsF4[(time_cut+1):tend]+totaltests_notF4[(time_cut+1):tend]
  ydum=pmax(ydum,0.0001) # guard against no treats
  topy=beforetrt[(time_cut+1):tend]-aftertrt[(time_cut+1):tend]
  topy[topy<0.1]=0
  if (sum(ydum)<0.1){
    topy=rep(0,length(topy))
  }
  d=cumsum(topy)/cumsum(ydum)
  #print(paste0("The final is ",rev(d)[1]))
  df= data.frame(x=tdum,y=d)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("chronic cases averted") + ggtitle("Number of Chronic cases averted per DAA treatment")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(tdum[1],2030,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_treats_gen_pop=function(t1,XT,pcom,alpha,time_cut,
                                    treat_cov_mild,treat_cov_severe,
                                    screening_cov,screening_diagnosis_duration,
                                    ignore_screening){
  # XT is the output from the compartment model
  # t1 is the required times
  # This is for all intervention model
  #t1=1+t1; # starts at zero
  t=time_cut
  tend = time_cut+9
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    if (t1[i]==time_cut){
      df=45
    }
    dum=treat_comps_pv7_gen_pop(N,t1[i],pcom,alpha,time_cut,
                                treat_cov_mild,treat_cov_severe,
                                screening_cov,screening_diagnosis_duration,
                                ignore_screening)
    if (t1[i]==time_cut){
      df=45
    }
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tdum=2016+(time_cut:(tend-1))
  ydum=totaltestsF4[(time_cut+1):tend]+totaltests_notF4[(time_cut+1):tend]
  df= data.frame(x=tdum,y=ydum)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of DAA") + ggtitle("Number of DAA treatments per year")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(tdum[1],2030,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_incidence_new_chronic_cases_gen_pop=function(XT,delta,rateA_F0,time_cut,leny){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  A_comps_00=seq(6,180,20)
  T00=(1-delta)*(1-exp(-rateA_F0))*colSums(XT[A_comps_00,])
  # must also add in screened
  A_comps_00=seq(6,180,20)+180
  T00=T00+(1-delta)*(1-exp(-rateA_F0))*colSums(XT[A_comps_00,])
  t=time_cut
  if (leny==1){
    tend = time_cut+9
    tdum=2015+(time_cut:(tend))
    bt=tdum
  }else{
    tend=dim(XT)[2]
    tdum=2015+(time_cut:(tend))
    bt=c(2021,seq(2025,(2015+tend),5))
  }
  
  yinci_PWID=T00[time_cut:(tend)]
  df= data.frame(x=tdum,y=100*((yinci_PWID/yinci_PWID[1]) - 1))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Change %") + ggtitle("% change in new chronic infections compared to 2021")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = bt,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_mortality_gen_pop=function(XT,death_vec,time_cut,leny){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  # Now can have deaths in the failed treatment compartments
  testcompvec00=t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec00_S=180+t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec00=rbind(testcompvec00,testcompvec00_S)
  death_vec=c(death_vec,death_vec)
  t=time_cut
  if (leny==1){
    tend = time_cut+9
    tdum=2015+(time_cut:(tend))
    bt=tdum
  }else{
    tend=dim(XT)[2]
    tdum=2015+(time_cut:(tend))
    bt=c(2021,seq(2025,(2015+tend),5))
  }
  XTdeaths=rep(0,tend-time_cut+1)
  death_vec=1-exp(-death_vec)
  for (i in 1 : 8){
    Ytest=death_vec[i]*colSums(XT[testcompvec00[i,],]);
    XTdeaths=XTdeaths+Ytest[time_cut:tend]
  }
  df= data.frame(x=tdum,y=100*((XTdeaths/XTdeaths[1]) - 1))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Change %") + ggtitle("% change in liver related (DCC,HCC,LT) deaths compared to 2021")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = bt,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}



fitN_v2=function(mult,proby){
  # starting value for gen pop
  N=rep(0,180)
  # fit the general pop
  
  fit_year =1#67 # 2016, 1950+(0:80)[67]
  # This is from runmodel_v3(1), just taking first 180 (all former) for col 67
  X=readRDS("www/Jgenpop_notreat_v4.rds")
  #X=readRDS("www/starter_for_gen_pop.rds") 
  X=matrix(X,nrow = length(X),ncol=1)
  pop_in_gen_pop_with_hcv = proby*3500000
  # In general population 20 compartments for each age group
  # only S0, A, F0-F4,DCC,HCC and LT1 and LT2 have subjects
  A_comps_00=seq(6,180,20)
  S_comps_00=seq(1,180,20)
  F0_comps_00 = seq(7,180,20)
  F1_comps_00 = seq(8,180,20)
  F2_comps_00 = seq(9,180,20)
  F3_comps_00 = seq(10,180,20)
  F4_comps_00 = seq(11,180,20)
  DCC_comps_00 = seq(12,180,20)
  HCC_comps_00 = seq(13,180,20)
  LT1_comps_00 = seq(14,180,20)
  LT2_comps_00 = seq(15,180,20)
  chronic00=c(F0_comps_00,F1_comps_00,F2_comps_00,F3_comps_00,F4_comps_00,DCC_comps_00,HCC_comps_00,LT1_comps_00,LT2_comps_00)
  Total_in_former = sum(X[chronic00,fit_year])+sum(X[A_comps_00,fit_year]) # must have HCV
  #mult_in_S0_gen_pop = sum(X[S_comps_00,fit_year])/Total_in_former
  frac_in_A_gen_pop = sum(X[A_comps_00,fit_year])/Total_in_former
  frac_in_chronic_comps=c(sum(X[F0_comps_00,fit_year]),sum(X[F1_comps_00,fit_year]),sum(X[F2_comps_00,fit_year]),sum(X[F3_comps_00,fit_year]),sum(X[F4_comps_00,fit_year]),
                          sum(X[DCC_comps_00,fit_year]),sum(X[HCC_comps_00,fit_year]),sum(X[LT1_comps_00,fit_year]),sum(X[LT2_comps_00,fit_year]))/Total_in_former
  N=rep(0,180)
  age_weights = X[A_comps_00,fit_year]/sum(X[A_comps_00,fit_year])
  N[A_comps_00]=frac_in_A_gen_pop*pop_in_gen_pop_with_hcv*age_weights
  
  #age_weights = X[S_comps_00,fit_year]/sum(X[S_comps_00,fit_year])
  #N[S_comps_00]=mult_in_S0_gen_pop*pop_in_gen_pop_with_hcv*age_weights
  
  age_weights = X[F0_comps_00,fit_year]/sum(X[F0_comps_00,fit_year])
  N[F0_comps_00]=frac_in_chronic_comps[1]*pop_in_gen_pop_with_hcv*age_weights
  age_weights = X[F1_comps_00,fit_year]/sum(X[F1_comps_00,fit_year])
  N[F1_comps_00]=frac_in_chronic_comps[2]*pop_in_gen_pop_with_hcv*age_weights
  age_weights = X[F2_comps_00,fit_year]/sum(X[F2_comps_00,fit_year])
  N[F2_comps_00]=frac_in_chronic_comps[3]*pop_in_gen_pop_with_hcv*age_weights
  age_weights = X[F3_comps_00,fit_year]/sum(X[F3_comps_00,fit_year])
  N[F3_comps_00]=frac_in_chronic_comps[4]*pop_in_gen_pop_with_hcv*age_weights
  age_weights = X[F4_comps_00,fit_year]/sum(X[F4_comps_00,fit_year])
  N[F4_comps_00]=frac_in_chronic_comps[5]*pop_in_gen_pop_with_hcv*age_weights
  age_weights = X[DCC_comps_00,fit_year]/sum(X[DCC_comps_00,fit_year])
  N[DCC_comps_00]=frac_in_chronic_comps[6]*pop_in_gen_pop_with_hcv*age_weights
  age_weights = X[HCC_comps_00,fit_year]/sum(X[HCC_comps_00,fit_year])
  N[HCC_comps_00]=frac_in_chronic_comps[7]*pop_in_gen_pop_with_hcv*age_weights
  age_weights = X[LT1_comps_00,fit_year]/sum(X[LT1_comps_00,fit_year])
  N[LT1_comps_00]=frac_in_chronic_comps[8]*pop_in_gen_pop_with_hcv*age_weights
  age_weights = X[LT2_comps_00,fit_year]/sum(X[LT2_comps_00,fit_year])
  N[LT2_comps_00]=frac_in_chronic_comps[9]*pop_in_gen_pop_with_hcv*age_weights
  
  # only susceptible left
  age_weights = X[S_comps_00,fit_year]/sum(X[S_comps_00,fit_year])
  ratioof=sum(X[S_comps_00,1])/sum(X)
  N[S_comps_00]=ratioof*sum(N)*age_weights
  #N[S_comps_00]=mult_in_S0_gen_pop*pop_in_gen_pop_with_hcv*age_weights
  dumN=N
  # reduce the pops of first five ages in chronic and put in suscpetible
  N[F0_comps_00[1:5]]=N[F0_comps_00[1:5]]*mult
  N[F1_comps_00[1:5]]= N[F1_comps_00[1:5]]*mult
  N[F2_comps_00[1:5]]=N[F2_comps_00[1:5]]*mult
  N[F3_comps_00[1:5]]=N[F3_comps_00[1:5]]*mult
  N[F4_comps_00[1:5]]=N[F4_comps_00[1:5]]*mult
  N[DCC_comps_00[1:5]]=N[DCC_comps_00[1:5]]*mult
  N[HCC_comps_00[1:5]]=N[HCC_comps_00[1:5]]*mult
  N[LT1_comps_00[1:5]]=N[LT1_comps_00[1:5]]*mult
  N[LT2_comps_00[1:5]]=N[LT2_comps_00[1:5]]*mult
  addy =(1-mult)*(dumN[F0_comps_00[1:5]]+dumN[F1_comps_00[1:5]]+dumN[F2_comps_00[1:5]]+
                    dumN[F3_comps_00[1:5]]+dumN[F4_comps_00[1:5]]+dumN[DCC_comps_00[1:5]]+
                    dumN[HCC_comps_00[1:5]]+dumN[LT1_comps_00[1:5]]+dumN[LT2_comps_00[1:5]])
  N[S_comps_00[1:5]]=N[S_comps_00[1:5]]+addy
  return(N)
}

plot_output_chronic=function(t,X,leny){
  
  F0_comps_00 = seq(7,180,20)
  F0_comps_00=c(F0_comps_00,F0_comps_00+180)
  F1_comps_00 = seq(8,180,20)
  F1_comps_00=c(F1_comps_00,F1_comps_00+180)  
  F2_comps_00 = seq(9,180,20)
  F2_comps_00=c(F2_comps_00,F2_comps_00+180)
  F3_comps_00 = seq(10,180,20)
  F3_comps_00=c(F3_comps_00,F3_comps_00+180)
  F4_comps_00 = seq(11,180,20)
  F4_comps_00=c(F4_comps_00,F4_comps_00+180)
  DCC_comps_00 = seq(12,180,20)
  DCC_comps_00=c(DCC_comps_00,DCC_comps_00+180)
  HCC_comps_00 = seq(13,180,20)
  HCC_comps_00=c(HCC_comps_00,HCC_comps_00+180)
  LT1_comps_00 = seq(14,180,20)
  LT1_comps_00=c(LT1_comps_00,LT1_comps_00+180)
  LT2_comps_00 = seq(15,180,20)
  LT2_comps_00=c(LT2_comps_00,LT2_comps_00+180)
  lt2=colSums(X[LT2_comps_00,])
  lt1=colSums(X[LT1_comps_00,])
  lt=lt1+lt2;
  dc=colSums(X[DCC_comps_00,])
  hcc=colSums(X[HCC_comps_00,])
  f4=colSums(X[F4_comps_00,])
  f3=colSums(X[F3_comps_00,])
  f2=colSums(X[F2_comps_00,])
  f1=colSums(X[F1_comps_00,])
  f0=colSums(X[F0_comps_00,])
  tot = f0+f1+f2+f3+f4+hcc+dc+lt
  if (leny==2){
    df= data.frame(x=2016+(0:74),y=tot)
    bt=c(2016,seq(2020,2090,5))
  }else{
    df= data.frame(x=2016+(0:14),y=tot[1:15])
    bt=c(2016,seq(2020,2030,1))
  }
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of subjects") + ggtitle("# of chronically infected subjects in General population")+
    #scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+expand_limits(y=0)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = bt,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #a=ggplot_build(p1)
  #ylim=ceiling(max(a$data[[1]]$y)/100000)*100000
  p1=p1#+scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits =c(0,ylim))
  return(p1)
}

plot_output_screened_gen_pop=function(XT,screen_cov,time_cut,leny){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  # Now can have deaths in the failed treatment compartments
  t=time_cut
  if (leny==1){
    tend = time_cut+9
    tdum=2015+((time_cut+1):(tend))
    bt=tdum
  }else{
    tend=dim(XT)[2]
    tdum=2015+((time_cut+1):(tend))
    bt=c(2021,seq(2025,(2015+tend),5))
  }
  screen_cov=1-exp(-screen_cov) # convert to prob
  # These are the current compartments in the screened model
  F0_current = seq(187,360,20) 
  F1_current = seq(188,360,20) 
  F2_current = seq(189,360,20)  
  F3_current = seq(190,360,20) 
  F4_current = seq(191,360,20) 
  T0_current = F0_current + 9
  T1_current = F1_current + 9
  T2_current = F2_current + 9
  T3_current = F3_current + 9
  T4_current = F4_current + 9
  cn=c(F0_current,F1_current,F2_current,F3_current,F4_current,
       T0_current,T1_current,T2_current,T3_current,T4_current)
  # *** note need all the subjects in screened not just F0 to F4, hence 181:360
  XTscreened = (diff(colSums(XT[181:360,(time_cut-1):(tend-1)])))
  df= data.frame(x=tdum,y=XTscreened)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# screened") + ggtitle("Number F0-F4 subjects screened each year")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = bt,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_gen_total=function(XT,time_cut,leny){
  
  t=time_cut
  if (leny==1){
    tend = time_cut+9
    tdum=2015+((time_cut+1):(tend))
    bt=tdum
  }else{
    tend=dim(XT)[2]
    tdum=2015+((time_cut+1):(tend))
    bt=c(2021,seq(2025,(2015+tend),5))
  }
  XTtotal = colSums(XT[,(time_cut):(tend-1)])
  df= data.frame(x=tdum,y=XTtotal)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# screened") + ggtitle("Total gen pop each year")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = bt,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_prev_screened_gen_pop=function(XT,screen_cov,time_cut,leny){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  # Now can have deaths in the failed treatment compartments
  t=time_cut
  if (leny==1){
    tend = time_cut+9
    tdum=2015+((time_cut+1):(tend))
    bt=tdum
  }else{
    tend=dim(XT)[2]
    tdum=2015+((time_cut+1):(tend))
    bt=c(2021,seq(2025,(2015+tend),5))
  }
  
  XTscreened = colSums(XT[181:360,(time_cut):(tend-1)])
  df= data.frame(x=tdum,y=XTscreened)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of subjects") + ggtitle("Number of subjects enrolled in screening")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = bt,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_attack_rate_v_treat_all_gen_pop=function(t1,X,XT,pcom,alpha,time_cut,
                                              treat_cov_mild,treat_cov_severe,
                                              screening_cov,screening_diagnosis_duration,
                                              ignore_screening){
  all_chron=c(seq(7,180,20),seq(8,180,20),seq(9,180,20),seq(10,180,20),seq(11,180,20),seq(12,180,20),
              seq(13,180,20),seq(14,180,20),seq(15,180,20))
  all_chron_S=c(all_chron,all_chron+180)
  ya=colSums(X[all_chron,])
  yb=colSums(XT[all_chron_S,])
  y=ya-yb
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    dum=treat_comps_pv7_gen_pop(N,(t1[i]-1),pcom,alpha,time_cut,
                                treat_cov_mild,treat_cov_severe,
                                screening_cov,screening_diagnosis_duration,
                                ignore_screening)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = 15#dim(XT)[2]
  tdum=2016+t1[time_cut:tend]-1
  ydum=totaltestsF4[time_cut:tend]+totaltests_notF4[time_cut:tend]
  ydum=pmax(ydum,0.0001) # guard against no treats
  d=y[time_cut:tend]/cumsum(ydum)
  df= data.frame(x=tdum,y=d)
  # two scales in one diag (not always recommended, but I WANT that now)
  # some reading up: https://stackoverflow.com/questions/3099219/plot-with-2-y-axes-one-y-axis-on-the-left-and-another-y-axis-on-the-right/3101876#
  
  C=cumsum(c(y[time_cut:tend]))
  TT=cumsum(c(ydum))
  zz=rep(0,length(C))
  C[C<0.1]=zz[C<0.1]
  TT[TT<0.1]=zz[TT<0.1]
  a=rev(C)[1]
  b=rev(TT)[1]
  prod1 = 1
  prod2 = 1
  if (a >b){
    prod2=1#a/b
  }else{
    prod1=1#b/a
  }
  dt <- data.frame(when=tdum, numinter=C, prod=TT) # pure fiction
  p1=ggplot() + 
    geom_bar(mapping = aes(x = dt$when, y = dt$numinter), stat = "identity", fill = "red") +
    geom_line(mapping = aes(x = prod1*dt$when, y = dt$prod*prod2), size = 2, color = "blue") + 
    scale_x_continuous(name="Year",breaks = c(seq(tdum[1],2030,1)),expand=c(0,0))+
    scale_y_continuous(name = "Cumulative chronic cases averted", labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                       sec.axis = sec_axis(~./prod2, name = "Cumulative treatments",labels=function(x) format(x, big.mark = ",", scientific = FALSE))) +
    theme(
      axis.title.y = element_text(color = "red"),
      axis.text.y= element_text(color = "red"),
      axis.title.y.right = element_text(color = "blue"),
      axis.text.y.right=element_text(color="blue"))
  return(p1)
}


plot_attack_rate_gen_pop=function(times,X){
  all_chron=c(seq(7,180,20),seq(8,180,20),seq(9,180,20),seq(10,180,20),seq(11,180,20),seq(12,180,20),
              seq(13,180,20),seq(14,180,20),seq(15,180,20))
  all_chron_S=c(all_chron,all_chron+180)
  newtop = colSums(X[all_chron,])
  newbot = colSums(X[1:360,])
  y=100*newtop/newbot
  df=data.frame(times,y)
  p1=ggplot(data=subset(df,times>=5 & times<=14), aes(x=times+2016, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Percentage") + ggtitle("% of model with chronic HCV from 2021 to 2030")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),lim=c(0,70))+
    scale_x_continuous(breaks = c(seq(2021,2030,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)  
}

plot_areacomps_gen=function(times,X){
  
  F0_comps_00 = seq(7,180,20)
  F1_comps_00 = seq(8,180,20)
  F2_comps_00 = seq(9,180,20)
  F3_comps_00 = seq(10,180,20)
  F4_comps_00 = seq(11,180,20)
  DCC_comps_00 = seq(12,180,20)
  HCC_comps_00 = seq(13,180,20)
  LT1_comps_00 = seq(14,180,20)
  LT2_comps_00 = seq(15,180,20)
  
  coltimes = times+1
  lt2=colSums(X[LT2_comps_00,coltimes])
  lt1=colSums(X[LT1_comps_00,coltimes])
  lt=lt1+lt2;
  dc=colSums(X[DCC_comps_00,coltimes])
  hcc=colSums(X[HCC_comps_00,coltimes])
  f4=colSums(X[F4_comps_00,coltimes])
  f3=colSums(X[F3_comps_00,coltimes])
  f2=colSums(X[F2_comps_00,coltimes])
  f1=colSums(X[F1_comps_00,coltimes])
  f0=colSums(X[F0_comps_00,coltimes])
  lofT=length(times)
  y=c(f0,f1,f2,f3,f4,dc,hcc,lt)
  Stage=c(rep("F0",lofT),rep("F1",lofT),rep("F2",lofT),rep("F3",lofT),rep("F4",lofT),rep("DC",lofT),rep("HCC",lofT),rep("LT",lofT))
  t=rep(times,8)
  df=data.frame(y,t,Stage)
  df$Stage=factor(df$Stage,levels=c("F0","F1","F2","F3","F4","DC","HCC","LT"))
  p1=ggplot(data=df, aes(x=t+2016, y=y,fill=Stage)) + geom_area() +
    xlab("Year") + ylab("Population") + ggtitle("Number in chronic compartments, 2021-2030")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), lim = c(0, 500000),expand = c(0,0),breaks=seq(0,500000,50000))+
    scale_x_continuous(breaks = 2016+times,expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)  
}


plotstacked_v3_noscaling=function(X,L){
  times      = seq(0, 34, by = 1)
  # 2007 pop is 2248500
  # There are only 360 compartments LT2_comps_00
  # first 180 are gen pop no treat failure and LT2_comps_01
  # second 180 are treat failure
  
  scale10=1
  scale00=1
  lt2=scale00*colSums(X[L$LT2_comps_00,])+scale10*colSums(X[L$LT2_comps_01,]);
  lt1=scale00*colSums(X[L$LT1_comps_00,])+scale10*colSums(X[L$LT1_comps_01,]);
  lt=lt1+lt2;
  dc=scale00*colSums(X[L$DC_comps_00,])+scale10*colSums(X[L$DC_comps_01,]);
  hcc=scale00*colSums(X[L$HCC_comps_00,])+scale10*colSums(X[L$HCC_comps_01,]);
  f4=scale00*colSums(X[L$F4_comps_00,])+scale10*colSums(X[L$F4_comps_01,]);
  f3=scale00*colSums(X[L$F3_comps_00,])+scale10*colSums(X[L$F3_comps_01,]);
  f2=scale00*colSums(X[L$F2_comps_00,])+scale10*colSums(X[L$F2_comps_01,]);
  f1=scale00*colSums(X[L$F1_comps_00,])+scale10*colSums(X[L$F1_comps_01,]);
  f0=scale00*colSums(X[L$F0_comps_00,])+scale10*colSums(X[L$F0_comps_01,]);
  y=c(f0,f1,f2,f3,f4,dc,hcc,lt)
  nt=length(times)
  Stage=c(rep("F0",nt),rep("F1",nt),rep("F2",nt),rep("F3",nt),rep("F4",nt),rep("DC",nt),rep("HCC",nt),rep("LT",nt))
  t=rep(0:(nt-1),8)
  df=data.frame(y,t,Stage)
  maxy=max(pretty_breaks()(c(0, (f0+f1+f2+f3+f4+dc+hcc+lt))))
  vecy=pretty_breaks()(c(0, (f0+f1+f2+f3+f4+dc+hcc+lt)))
  df$Stage=factor(df$Stage,levels=c("F0","F1","F2","F3","F4","DC","HCC","LT"))
  p1=ggplot(data=subset(df,times>=5 & times<=24), aes(x=t+2016, y=y,fill=Stage)) + geom_area() +
    xlab("Year") + ylab("Population") + ggtitle(paste0("Number in chronic compartments"))+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2021,2040,1)),limits=c(2021,2040),expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

plotPWID_noscaled=function(XT,L,time_cut,leny){
  # 2007 pop is 2248500
  
  
  scale10=1
  scale00=1
  
  #scale10 = 1
  aftertrt= scale10*colSums(XT[c(L$chronic00,L$A_comps_00),])+scale00*colSums(XT[c(L$chronic01,L$A_comps_01),])
  tend = dim(XT)[2]-1
  # total number of treatments
  if (leny==1){
    tend = time_cut+19
    tdum=2015+(time_cut:(tend))
    bt=tdum
  }else{
    tend=dim(XT)[2]-1
    tdum=2015+(time_cut:(tend))
    bt=c(seq(2021,2040,1))
  } # starts at zero
  df=data.frame(tdum=tdum,aftertrt=aftertrt[time_cut:(tend)])
  maxy=max(pretty_breaks()(c(0, df$aftertrt)))
  vecy=pretty_breaks()(c(0, df$aftertrt))
  p2=ggplot(data=df, aes(x=tdum, y=aftertrt)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of subjects") + ggtitle(paste0("# of gen pop with HCV"))+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = bt,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))
}

plot_prev_v4_noscaling=function(X,L,time_cut){
  times      = seq(0, 34, by = 1)
  tend=dim(X)[2]-1
  tdum=2015+(time_cut:(tend))
  y=100*colSums(X[c(L$chronic00,L$chronic01),])/colSums(X[1:180,])
  df=data.frame(times,y)
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=subset(df,times>=6 & times<=24), aes(x=times+2016, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Percentage") + ggtitle(paste0("% of gen pop with chronic HCV"))+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2021,2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p1)  
}

plotincidence_v2_change_noscaling=function(XT,L,time_cut,delta,acuterate,curr_mort_pwid,curr_mort_former){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  
  T00=as.vector((1-curr_mort_former)%*%XT[L$A_comps_00,])
  T01=as.vector((1-curr_mort_former)%*%XT[L$A_comps_01,])
  
  # integrate
  t=time_cut # since starts at 1950
  tend = 25
  yinci_PWID=T00[t:tend]+T01[t:tend]
  df= data.frame(x=2015+(t:tend),y=100*((yinci_PWID/yinci_PWID[1] - 1)))
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Change %") + ggtitle("% incidence change compared to 2021")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(-10,10))+
    scale_x_continuous(breaks = c(seq(2021,2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))
  return(p1)
}

plotincidence_v2_noscaling=function(XT,L,time_cut,delta,acuterate,curr_mort_pwid,curr_mort_former){
  
  
  # T00=colSums(XT[L$A_comps_00,]);
  # T01=colSums(XT[L$A_comps_01,]);
  
  T00=as.vector((1-curr_mort_former)%*%XT[L$A_comps_00,])
  T01=as.vector((1-curr_mort_former)%*%XT[L$A_comps_01,])
  
  # integrate
  t=time_cut # since starts at 1950
  tend = 25
  yinci_PWID=(1-delta)*(T00[t:tend]+T01[t:tend])*acuterate
  df= data.frame(x=2015+(t:tend),y=yinci_PWID)
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of subjects") + ggtitle("# of incident chronic cases")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq((2015+time_cut),2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))
  return(p1)
}

plot_output_chronic_all_genpop=function(t1,L,XT,time_cut,leny){
  aftertrt= colSums(XT[L$chronic00,])+colSums(XT[L$chronic01,])
  time_cut=time_cut-1 # since starts at 1950
  tend = dim(XT)[2]-1
  # total number of treatments
  if (leny==1){
    tend = time_cut+18
    tdum=2016+(time_cut:(tend))
    bt=tdum
  }else{
    tend=dim(XT)[2]-1
    tdum=1950+(time_cut:(tend))
    bt=c(2021,seq(2025,(1950+tend),5))
  } # starts at zero
  
  df=data.frame(tdum=tdum,aftertrt=aftertrt[(1+time_cut):(1+tend)])
  maxy=max(pretty_breaks()(c(0, df$aftertrt)))
  vecy=pretty_breaks()(c(0, df$aftertrt))
  p1=ggplot(data=df, aes(x=tdum, y=aftertrt)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of subjects") + ggtitle("# of chronically infected subjects")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = bt,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_incidence_new_chronic_cases_v2_all_genpop=function(XT,delta,rateA_F0,time_cut,
                                                               A_comps_00,A_comps_01){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  pA_F0=1-exp(-rateA_F0)
  T00=(1-delta)*pA_F0*colSums(XT[A_comps_00,]);
  T01=(1-delta)*pA_F0*colSums(XT[A_comps_01,]);
  
  # integrate
  t=time_cut-2 # since starts at 1950
  tend = dim(XT)[2]-11
  yinci_PWID=T00[t:tend]+T01[t:tend]
  
  df= data.frame(x=2016+(t:tend),y=100*((yinci_PWID/yinci_PWID[1]) - 1))
  maxy=max(pretty_breaks()(c(0, df$y)))
  miny=min(-10,min(pretty_breaks()(c(0, df$y))))
  vecy=pretty_breaks()(c(miny, df$y))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Change %") + ggtitle("% change in new chronic infections compared to 2021")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(miny,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2016+t,2016+tend,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_treats_v2_all_genpop=function(t1,XT,pcom,alpha,time_ofint_inODEunits,
                                          treat_cov_mild,treat_cov_severe,
                                          screening_cov,screening_diagnosis_duration,
                                          ignore_screening){
  # XT is the output from the compartment model
  # t1 is the required times
  # This is for all intervention model
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    t=t1[i]
    if (t>=(time_ofint_inODEunits-1)) { # year 2022 and onwards
      enroll_S=screening_cov
      if  (ignore_screening==1){
        enroll_S=0
      }
    }else{
      enroll_S=0
    }
    dum=treat_comps_pv7_gen_pop(N,t,pcom,alpha,time_ofint_inODEunits,
                                treat_cov_mild,treat_cov_severe,
                                enroll_S,screening_diagnosis_duration,
                                ignore_screening)
    
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = dim(XT)[2]-10
  tdum=2016+t1[time_ofint_inODEunits:tend]-1
  ydum=totaltestsF4[time_ofint_inODEunits:tend]+totaltests_notF4[time_ofint_inODEunits:tend]
  df= data.frame(x=tdum,y=ydum)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of DAA") + ggtitle("Total number of DAA treatments")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(tdum[1],2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
  
  
}

plot_output_mortality_v2_all_genpop =function(XT,death_vec,time_cut){
  # death_vec=c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  
  death_vals00=rep(0,4)
  death_vals01=rep(0,4)
  time_cut=time_cut-1
  time_end=dim(XT)[2]-10
  XTdeaths=rep(0,time_end-time_cut+1)
  # Now can have deaths in the failed treatment compartments
  testcompvec00=t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec01_failed=t(matrix(c(seq(192,360,20),seq(193,360,20),seq(194,360,20),seq(195,360,20)),nrow=9,ncol=4))  
  
  
  death_vec=1-exp(-death_vec)
  for (i in 1 : 4){
    Ytest=death_vec[i]*colSums(XT[testcompvec00[i,],]);
    Y1test=death_vec[i]*colSums(XT[testcompvec01_failed[i,],]);
    
    
    
    XTdeaths=XTdeaths+Ytest[time_cut:time_end]+Y1test[time_cut:time_end]
  }
  #print(XTdeaths)
  df= data.frame(x=-1+2016+(time_cut:time_end),y=100*((XTdeaths/XTdeaths[1]) - 1))
  maxy=max(pretty_breaks()(c(0, df$y)))
  miny=min(0,min(pretty_breaks()(c(0, df$y))))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Change %") + ggtitle("% change in liver related mortality (DCC,HCC,LT) compared to 2021")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(miny,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2016+time_cut-1,2016+time_end-1,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_mortality_v2_all_num_genpop =function(XT,death_vec,time_cut){
  # death_vec=c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  
  death_vals00=rep(0,4)
  death_vals01=rep(0,4)
  time_cut=time_cut-1 + 1
  time_end=dim(XT)[2]-10
  XTdeaths=rep(0,time_end-time_cut+1)
  # Now can have deaths in the failed treatment compartments
  testcompvec00=t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec01_failed=t(matrix(c(seq(192,360,20),seq(193,360,20),seq(194,360,20),seq(195,360,20)),nrow=9,ncol=4))  
  
  
  death_vec=1-exp(-death_vec)
  for (i in 1 : 4){
    Ytest=death_vec[i]*colSums(XT[testcompvec00[i,],]);
    Y1test=death_vec[i]*colSums(XT[testcompvec01_failed[i,],]);
    
    
    
    XTdeaths=XTdeaths+Ytest[time_cut:time_end]+Y1test[time_cut:time_end]
  }
  #print(XTdeaths)
  df= data.frame(x=-1+2016+(time_cut:time_end),y=XTdeaths)
  maxy=max(pretty_breaks()(c(0, df$y)))
  miny=min(0,min(pretty_breaks()(c(0, df$y))))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of deaths") + ggtitle("# of liver related deaths (DCC,HCC,LT)")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(miny,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2016+time_cut-1,2016+time_end-1,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_cases_averted_all_genpop=function(t1,L,X,XT,pcom,alpha,time_cut,
                                              treat_cov_mild,treat_cov_severe,
                                              screening_cov,screening_diagnosis_duration,
                                              ignore_screening){
  beforetrt=colSums(X[L$chronic00,])+colSums(X[L$chronic01,])
  aftertrt= colSums(XT[L$chronic00,])+colSums(XT[L$chronic01,])
  
  t=time_cut-2 # since starts at 1950
  tend = dim(XT)[2]-1
  # total number of treatments
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    t=t1[i]
    if (t>=(time_cut-1)) { # year 2022 and onwards
      enroll_S=screening_cov
      if  (ignore_screening==1){
        enroll_S=0
      }
    }else{
      enroll_S=0
    }
    dum=treat_comps_pv7_gen_pop(N,t,pcom,alpha,time_cut,
                                treat_cov_mild,treat_cov_severe,
                                enroll_S,screening_diagnosis_duration,
                                ignore_screening)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = dim(XT)[2]
  tdum=2016+t1[time_cut:tend]-1
  ydum=totaltestsF4[time_cut:tend]+totaltests_notF4[time_cut:tend]
  ydum=pmax(ydum,0.0001) # guard against no treats
  topy=beforetrt[time_cut:tend]-aftertrt[time_cut:tend]
  topy[topy<0.1]=0
  if (sum(ydum)<0.1){
    topy=rep(0,length(topy))
  }
  d=cumsum(topy)/cumsum(ydum)
  print(paste0("The final is ",rev(d)[1]))
  df= data.frame(x=tdum,y=d)
  p1=ggplot(data=subset(df,x<=2040), aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("chronic cases averted") + ggtitle("Number of Chronic cases averted by DAA treatment")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(tdum[1],2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_attack_rate_v_treat_all_genpop=function(t1,L,X,XT,pcom,alpha,time_cut,
                                             treat_cov_mild,treat_cov_severe,
                                             screening_cov,screening_diagnosis_duration,
                                             ignore_screening){
  ya=colSums(X[L$chronic01,])+colSums(X[L$chronic00,])
  yb=colSums(XT[L$chronic01,])+colSums(XT[L$chronic00,])
  y=ya-yb
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    t=t1[i]
    if (t>=(time_cut-1)) { # year 2022 and onwards
      enroll_S=screening_cov
      if  (ignore_screening==1){
        enroll_S=0
      }
    }else{
      enroll_S=0
    }
    dum=treat_comps_pv7_gen_pop(N,t,pcom,alpha,time_cut,
                                treat_cov_mild,treat_cov_severe,
                                enroll_S,screening_diagnosis_duration,
                                ignore_screening)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = dim(XT)[2]
  tdum=2016+t1[time_cut:tend]-1
  ydum=totaltestsF4[time_cut:tend]+totaltests_notF4[time_cut:tend]
  ydum=pmax(ydum,0.0001) # guard against no treats
  d=y[time_cut:tend]/cumsum(ydum)
  df= data.frame(x=tdum,y=d)
  
  # two scales in one diag (not always recommended, but I WANT that now)
  # some reading up: https://stackoverflow.com/questions/3099219/plot-with-2-y-axes-one-y-axis-on-the-left-and-another-y-axis-on-the-right/3101876#
  
  C=cumsum(c(y[time_cut:tend]))
  TT=cumsum(c(ydum))
  zz=rep(0,length(C))
  C[C<0.1]=zz[C<0.1]
  TT[TT<0.1]=zz[TT<0.1]
  a=rev(C)[1]
  b=rev(TT)[1]
  prod1 = 1
  prod2 = 1
  if (a >b){
    prod2=1#a/b
  }else{
    prod1=1#b/a
  }
  dt <- data.frame(when=tdum, numinter=C, prod=TT) # pure fiction
  dt=subset(dt,when<=2040)
  p1=ggplot() + 
    geom_bar(mapping = aes(x = dt$when, y = dt$numinter), stat = "identity", fill = "red") +
    geom_line(mapping = aes(x = prod1*dt$when, y = dt$prod*prod2), size = 2, color = "blue") + 
    scale_x_continuous(name="Year",breaks = c(seq(tdum[1],2040,1)),expand=c(0,0))+
    scale_y_continuous(name = "Cumulative chronic cases averted", labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                       sec.axis = sec_axis(~./prod2, name = "Cumulative treatments",labels=function(x) format(x, big.mark = ",", scientific = FALSE))) +
    theme(
      axis.title.y = element_text(color = "red"),
      axis.text.y= element_text(color = "red"),
      axis.title.y.right = element_text(color = "blue"),
      axis.text.y.right=element_text(color="blue"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_screened_count_genpop=function(t1,XT,time_ofint_inODEunits){
  # XT is the output from the compartment model
  # t1 is the required times
  # This is for all intervention model
  
  F0_current_screen = seq(187,340,20)
  F1_current_screen = seq(188,340,20)
  F2_current_screen = seq(189,340,20)
  F3_current_screen = seq(190,340,20)
  F4_current_screen = seq(191,340,20)
  T0_current_screen = seq(187,340,20) + 9
  T1_current_screen = seq(188,340,20) + 9
  T2_current_screen = seq(189,340,20) + 9
  T3_current_screen = seq(190,340,20) + 9
  T4_current_screen = seq(191,340,20) + 9
  cn=c(F0_current_screen,F1_current_screen,F2_current_screen,F3_current_screen,F4_current_screen,
       T0_current_screen,T1_current_screen,T2_current_screen,T3_current_screen,T4_current_screen)
  # *** note need all the subjects that have been moved not just F0-F4
  # for nep = 721:1080, for OST 1081:1440, for both 1441:1800
  total_screen =c(0,diff(colSums(XT[181:360,])))
  
  y1=total_screen
  t1=2016+(0:34)
  df=data.frame(t1,y1)
  
  p=ggplot(data=subset(df,t1>=2022 & t1<=2040), aes(x=t1, y=y1)) + geom_line(color="red",size=2)+
    xlab("Year") + ylab("Number of people") + ggtitle("Number of subjects at stage F0-F4 screened per year from 2022 to 2030")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(2022,2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p)  
}


