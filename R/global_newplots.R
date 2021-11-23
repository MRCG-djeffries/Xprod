global_newplots=function(Xin,L,timein,scale00,scale10,scale00_prison,scale10_prison,
                         time_cut,Xbase_gp,Xbase_pwid,Xbase_pris,com_alpha,L_PWID,L_PRIS,L_GP,ignore_flows,df,
                         Lgenpop,Lpwid,Lprison,time_vals){
  # X is the 1: 3960 compartments
  # timein is the time to plot, for example 2016:2040
  # L is the compartment data
  # scales for PWID scale00,scale10
  # scales for prsion scale00_prison,scale10_prison
  # print("here")
  tcols = timein - 2015 + 1
  nt=length(timein)

  # stage plots
  #GEN POP
  Xgenpop=Xin[1:360,tcols]
  Cgenpop=getallpop_all_v2_pwid_for_prison(Xgenpop,L,rep(1,9),rep(1,9),nt,1,1)
  p5_gp=plot_areacomps_v3_pwid_for_prison(timein,Cgenpop,1)
  #PWID
  Xpwid=Xin[361:2160,tcols] # restrict the time for these output
  Cpwid=getallpop_all_v2_pwid_for_prison(Xpwid,L,rep(1,9),rep(1,9),nt,scale00,scale10)
  p5_pwid=plot_areacomps_v3_pwid_for_prison(timein,Cpwid,2)
  
  #PRISON
  Xprison=Xin[2161:3960,tcols] # restrict the time for these output
  Cprison=getallpop_all_v2_pwid_for_prison(Xprison,L,rep(1,9),rep(1,9),nt,scale00_prison,scale10_prison)
  p5_pi=plot_areacomps_v3_pwid_for_prison(timein,Cprison,3)
  
  #xbase must also have tcols
  Xbase_gp= Xbase_gp[,tcols]
  Xbase_pwid=Xbase_pwid[,tcols]
  Xbase_pris=Xbase_pris[,tcols]
  
  # now plot all combinations
  D=mget(c("Cgenpop","Cprison","Cpwid"))
  p5_gp_pwid=plot_areacomps_v3_pwid_for_prison(timein,( Map(funky,D[[1]],D[[3]],NA)),4)
  p5_pi_pwid=plot_areacomps_v3_pwid_for_prison(timein,(Map(funky,D[[2]],D[[3]],NA)),5)
  p5_gp_pi=plot_areacomps_v3_pwid_for_prison(timein,(Map(funky,D[[1]],D[[2]],NA)),6)
  p5_gp_pwid_pi=plot_areacomps_v3_pwid_for_prison(timein,(Map(funky,D[[1]],D[[2]],D[[3]])),7)
  
  # total treatments
  tot_treats_gp=plot_output_treats_v2_all_PWID_prison_gp(1,timein,L,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  tot_treats_pwid=plot_output_treats_v2_all_PWID_prison_gp(2,timein,L,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  tot_treats_pi=plot_output_treats_v2_all_PWID_prison_gp(3,timein,L,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  tot_treats_gp_pwid=plot_output_treats_v2_all_PWID_prison_gp(4,timein,L,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  tot_treats_pi_pwid=plot_output_treats_v2_all_PWID_prison_gp(5,timein,L,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  tot_treats_gp_pi=plot_output_treats_v2_all_PWID_prison_gp(6,timein,L,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  tot_treats_gp_pwid_pi=plot_output_treats_v2_all_PWID_prison_gp(7,timein,L,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  
  # cum treats # edit all the screening
  pcum_gp = plot_attack_rate_v_treat_all_PWID_prison_gp_v2(1,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  pcum_pwid = plot_attack_rate_v_treat_all_PWID_prison_gp_v2(2,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  pcum_pi = plot_attack_rate_v_treat_all_PWID_prison_gp_v2(3,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  pcum_gp_pwid = plot_attack_rate_v_treat_all_PWID_prison_gp_v2(4,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  pcum_pi_pwid = plot_attack_rate_v_treat_all_PWID_prison_gp_v2(5,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  pcum_gp_pi = plot_attack_rate_v_treat_all_PWID_prison_gp_v2(6,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  pcum_gp_pwid_pi = plot_attack_rate_v_treat_all_PWID_prison_gp_v2(7,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  
  # case averted per treat
  casepert_gp = plot_output_cases_averted_all_PWID_prison_gp(1,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  casepert_pwid = plot_output_cases_averted_all_PWID_prison_gp(2,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  casepert_pi = plot_output_cases_averted_all_PWID_prison_gp(3,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  casepert_gp_pwid = plot_output_cases_averted_all_PWID_prison_gp(4,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  casepert_pi_pwid = plot_output_cases_averted_all_PWID_prison_gp(5,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  casepert_gp_pi = plot_output_cases_averted_all_PWID_prison_gp(6,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  casepert_gp_pwid_pi = plot_output_cases_averted_all_PWID_prison_gp(7,timein,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS,L_GP)
  
  # number in prevention/screening
  prevc_gp = plot_output_nep_ost_v3_all_count_PWID_prison_gp(1,timein,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,time_cut)
  prevc_pwid = plot_output_nep_ost_v3_all_count_PWID_prison_gp(2,timein,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,time_cut)
  prevc_pi = plot_output_nep_ost_v3_all_count_PWID_prison_gp(3,timein,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,time_cut)
  prevc_gp_pwid = plot_output_nep_ost_v3_all_count_PWID_prison_gp(4,timein,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,time_cut)
  prevc_pi_pwid = plot_output_nep_ost_v3_all_count_PWID_prison_gp(5,timein,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,time_cut)
  prevc_gp_pi = plot_output_nep_ost_v3_all_count_PWID_prison_gp(6,timein,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,time_cut)
  prevc_gp_pwid_pi = plot_output_nep_ost_v3_all_count_PWID_prison_gp(7,timein,Xgenpop,Xpwid,Xprison,scale00,scale10,scale00_prison,scale10_prison,time_cut)
  
  # mortality and incidence plots, need all cols here - nums
  Xgenpop_allcols=Xin[1:360,]
  Xpwid_allcols=Xin[361:2160,] 
  Xprison_allcols=Xin[2161:3960,] 
  
  # R0
  rt_gp=plot_Rt_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,1,1,scale00,scale10,scale00_prison,scale10_prison)  
  rt_pwid=plot_Rt_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,2,1,scale00,scale10,scale00_prison,scale10_prison)  
  rt_pi=plot_Rt_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,3,1,scale00,scale10,scale00_prison,scale10_prison)  
  rt_gp_pwid=plot_Rt_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,4,1,scale00,scale10,scale00_prison,scale10_prison)  
  rt_pi_pwid=plot_Rt_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,5,1,scale00,scale10,scale00_prison,scale10_prison)  
  rt_gp_pi=plot_Rt_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,6,1,scale00,scale10,scale00_prison,scale10_prison)  
  rt_gp_pwid_pi=plot_Rt_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,7,1,scale00,scale10,scale00_prison,scale10_prison)  
  
  # mortality
  p3_gp=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,1,scale00,scale10,scale00_prison,scale10_prison,1,ignore_flows,
                                                    com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)$p1
  p3_pwid=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,2,scale00,scale10,scale00_prison,scale10_prison,1,ignore_flows,
                                                      com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)$p1
  p3_pi=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,3,scale00,scale10,scale00_prison,scale10_prison,1,ignore_flows,
                                                    com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)$p1
  p3_gp_pwid=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,4,scale00,scale10,scale00_prison,scale10_prison,1,ignore_flows,
                                                         com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)$p1
  p3_pi_pwid=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,5,scale00,scale10,scale00_prison,scale10_prison,1,ignore_flows,
                                                         com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)$p1
  p3_gp_pi=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,6,scale00,scale10,scale00_prison,scale10_prison,1,ignore_flows,
                                                       com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)$p1
  p3_gp_pwid_pi=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,7,scale00,scale10,scale00_prison,scale10_prison,1,ignore_flows,
                                                            com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)$p1
  
  dum=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,1,scale00,scale10,scale00_prison,scale10_prison,2,ignore_flows,
                                                  com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)
  p3_gp_num=dum$p1
  p3_gp_num_val=dum$morty
  dum=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,2,scale00,scale10,scale00_prison,scale10_prison,2,ignore_flows,
                                                  com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)
  p3_pwid_num=dum$p1
  p3_pwid_num_val=dum$morty
  dum=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,3,scale00,scale10,scale00_prison,scale10_prison,2,ignore_flows,
                                                  com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)
  p3_pi_num=dum$p1
  p3_pi_num_val=dum$morty
  dum=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,4,scale00,scale10,scale00_prison,scale10_prison,2,ignore_flows,
                                                  com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)
  p3_gp_pwid_num=dum$p1
  p3_gp_pwid_num_val=dum$morty
  dum=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,5,scale00,scale10,scale00_prison,scale10_prison,2,ignore_flows,
                                                  com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)
  p3_pi_pwid_num=dum$p1
  p3_pi_pwid_num_val=dum$morty
  dum=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,6,scale00,scale10,scale00_prison,scale10_prison,2,ignore_flows,
                                                  com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)
  p3_gp_pi_num=dum$p1
  p3_gp_pi_num_val=dum$morty
  dum=plot_output_mortality_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,7,scale00,scale10,scale00_prison,scale10_prison,2,ignore_flows,
                                                  com_alpha$pcom,com_alpha$alpha_current,time_cut,L_PWID,L_PRIS)    
  p3_gp_pwid_pi_num=dum$p1
  p3_gp_pwid_pi_num_val=dum$morty
  # incidence % change
  p1_gp=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,1,scale00,scale10,scale00_prison,scale10_prison,1)$p1
  p1_pwid=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,2,scale00,scale10,scale00_prison,scale10_prison,1)$p1
  p1_pi=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,3,scale00,scale10,scale00_prison,scale10_prison,1)$p1
  p1_gp_pwid=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,4,scale00,scale10,scale00_prison,scale10_prison,1)$p1
  p1_pi_pwid=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,5,scale00,scale10,scale00_prison,scale10_prison,1)$p1
  p1_gp_pi=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,6,scale00,scale10,scale00_prison,scale10_prison,1)$p1
  p1_gp_pwid_pi=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,7,scale00,scale10,scale00_prison,scale10_prison,1)$p1
  
  # incidence # change
  dum=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,1,scale00,scale10,scale00_prison,scale10_prison,2)
  p1_gp_num=dum$p1
  p1_gp_num_val=dum$inci
  dum=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,2,scale00,scale10,scale00_prison,scale10_prison,2)
  p1_pwid_num=dum$p1
  p1_pwid_num_val=dum$inci
  dum=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,3,scale00,scale10,scale00_prison,scale10_prison,2)
  p1_pi_num=dum$p1
  p1_pi_num_val=dum$inci
  dum=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,4,scale00,scale10,scale00_prison,scale10_prison,2)
  p1_gp_pwid_num=dum$p1
  p1_gp_pwid_num_val=dum$inci
  dum=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,5,scale00,scale10,scale00_prison,scale10_prison,2)
  p1_pi_pwid_num=dum$p1
  p1_pi_pwid_num_val=dum$inci
  dum=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,6,scale00,scale10,scale00_prison,scale10_prison,2)    
  p1_gp_pi_num=dum$p1
  p1_gp_pi_num_val=dum$inci
  dum=plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,7,scale00,scale10,scale00_prison,scale10_prison,2)  
  p1_gp_pwid_pi_num=dum$p1
  p1_gp_pwid_pi_num_val=dum$inci 

  
  # both plots of inci and deaths
  both_gp=plot_cases_deaths_PWID_prison_gp(timein,p1_gp_num_val,p3_gp_num_val,1)
  both_pwid=plot_cases_deaths_PWID_prison_gp(timein,p1_pwid_num_val,p3_pwid_num_val,2)
  both_pi=plot_cases_deaths_PWID_prison_gp(timein,p1_pi_num_val,p3_pi_num_val,3)
  both_gp_pwid=plot_cases_deaths_PWID_prison_gp(timein,p1_gp_pwid_num_val,p3_gp_pwid_num_val,4)
  both_pi_pwid=plot_cases_deaths_PWID_prison_gp(timein,p1_pi_pwid_num_val,p3_pi_pwid_num_val,5)
  both_gp_pi=plot_cases_deaths_PWID_prison_gp(timein,p1_gp_pi_num_val,p3_gp_pi_num_val,6)
  both_gp_pwid_pi=plot_cases_deaths_PWID_prison_gp(timein,p1_gp_pwid_pi_num_val,p3_gp_pwid_pi_num_val,7)
  
  
  # point prev
  pp_gp=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,1,1,scale00,scale10,scale00_prison,scale10_prison)
  pp_pwid=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,2,1,scale00,scale10,scale00_prison,scale10_prison)  
  pp_pi=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,3,1,scale00,scale10,scale00_prison,scale10_prison)
  pp_gp_pwid=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,4,1,scale00,scale10,scale00_prison,scale10_prison)
  pp_pi_pwid=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,5,1,scale00,scale10,scale00_prison,scale10_prison)
  pp_gp_pi=  plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,6,1,scale00,scale10,scale00_prison,scale10_prison)
  pp_gp_pwid_pi=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,7,1,scale00,scale10,scale00_prison,scale10_prison)
  
  # total number chronic
  cn_gp=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,1,2,scale00,scale10,scale00_prison,scale10_prison)
  cn_pwid=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,2,2,scale00,scale10,scale00_prison,scale10_prison)  
  cn_pi=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,3,2,scale00,scale10,scale00_prison,scale10_prison)
  cn_gp_pwid=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,4,2,scale00,scale10,scale00_prison,scale10_prison)
  cn_pi_pwid=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,5,2,scale00,scale10,scale00_prison,scale10_prison)
  cn_gp_pi=  plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,6,2,scale00,scale10,scale00_prison,scale10_prison)
  cn_gp_pwid_pi=plot_point_prev_PWID_prison_gp(Xgenpop_allcols,Xpwid_allcols,Xprison_allcols,timein,7,2,scale00,scale10,scale00_prison,scale10_prison)

  
  Xgenpop_allcols=Xin[1:360,]
  Xpwid_allcols=Xin[361:2160,]
  Xprison_allcols=Xin[2161:3960,]
  scale00_pwid=scale00
  scale10_pwid=scale10
  # For the ICER output
  tcols=1+(timein-2015)
  # note from figures as
  # the treat plots always go from 2022
  if (time_vals==0){
    bity=rep(0,(tot_treats_gp$data$x[1]-2016))
  } else {
    bity =NULL
  }
  Ltreatgenp=cost_qaly_run_with_treats_genpop(Xgenpop_allcols[,tcols],1,1,c(bity,tot_treats_gp$data$y),df$DAA_duration_gen_three_GP,df$discount_rate)
  Ltreatpwid=cost_qaly_run_with_treats(Xpwid_allcols[,tcols],scale00_pwid,scale10_pwid,c(bity,tot_treats_pwid$data$y),df$DAA_duration_three_PWID,df$discount_rate)
  Ltreatpris=cost_qaly_run_with_treats(Xprison_allcols[,tcols],scale00_prison,scale10_prison,c(bity,tot_treats_pi$data$y),df$DAA_duration_three_PRIS,df$discount_rate)

  # 
    icer_gp=plot_output_icer_PWID_prison_gp(1,Lgenpop,Lpwid,Lprison,Ltreatgenp,Ltreatpwid,Ltreatpris,df)
    icer_pwid=plot_output_icer_PWID_prison_gp(2,Lgenpop,Lpwid,Lprison,Ltreatgenp,Ltreatpwid,Ltreatpris,df)
    icer_pi=plot_output_icer_PWID_prison_gp(3,Lgenpop,Lpwid,Lprison,Ltreatgenp,Ltreatpwid,Ltreatpris,df)
    icer_gp_pwid=plot_output_icer_PWID_prison_gp(4,Lgenpop,Lpwid,Lprison,Ltreatgenp,Ltreatpwid,Ltreatpris,df)
    icer_pi_pwid=plot_output_icer_PWID_prison_gp(5,Lgenpop,Lpwid,Lprison,Ltreatgenp,Ltreatpwid,Ltreatpris,df)
    icer_gp_pi=plot_output_icer_PWID_prison_gp(6,Lgenpop,Lpwid,Lprison,Ltreatgenp,Ltreatpwid,Ltreatpris,df)
    icer_gp_pwid_pi=plot_output_icer_PWID_prison_gp(7,Lgenpop,Lpwid,Lprison,Ltreatgenp,Ltreatpwid,Ltreatpris,df)


  # order is stages, mort %, mort #, inci %,inci #
  return(list(stages_gp=p5_gp,stages_pwid=p5_pwid,stages_pi=p5_pi,stages_gp_pwid=p5_gp_pwid,stages_pi_pwid=p5_pi_pwid,stages_gp_pi=p5_gp_pi,stages_gp_pwid_pi=p5_gp_pwid_pi,
              mort_gp=p3_gp,mort_pwid=p3_pwid,mort_pi=p3_pi,mort_gp_pwid=p3_gp_pwid,mort_pi_pwid=p3_pi_pwid,mort_gp_pi=p3_gp_pi,mort_gp_pwid_pi=p3_gp_pwid_pi,
              mort_gp_num=p3_gp_num,mort_pwid_num=p3_pwid_num,mort_pi_num=p3_pi_num,mort_gp_pwid_num=p3_gp_pwid_num,mort_pi_pwid_num=p3_pi_pwid_num,
              mort_gp_pi_num=p3_gp_pi_num,mort_gp_pwid_pi_num=p3_gp_pwid_pi_num,
              inci_gp=p1_gp,inci_pwid=p1_pwid,inci_pi=p1_pi,inci_gp_pwid=p1_gp_pwid,inci_pi_pwid=p1_pi_pwid,inci_gp_pi=p1_gp_pi,inci_gp_pwid_pi=p1_gp_pwid_pi,
              inci_gp_num=p1_gp_num,inci_pwid_num=p1_pwid_num,inci_pi_num=p1_pi_num,inci_gp_pwid_num=p1_gp_pwid_num,inci_pi_pwid_num=p1_pi_pwid_num,
              inci_gp_pi_num=p1_gp_pi_num,inci_gp_pwid_pi_num=p1_gp_pwid_pi_num,
              both_gp=both_gp,both_pwid=both_pwid,both_pi=both_pi,both_gp_pwid=both_gp_pwid,both_pi_pwid=both_pi_pwid,both_gp_pi=both_gp_pi,both_gp_pwid_pi=both_gp_pwid_pi,
              pp_gp=pp_gp,pp_pwid=pp_pwid,pp_pi=pp_pi,pp_gp_pwid=pp_gp_pwid,pp_pi_pwid=pp_pi_pwid,pp_gp_pi=pp_gp_pi,pp_gp_pwid_pi=pp_gp_pwid_pi,
              cn_gp=cn_gp,cn_pwid=cn_pwid,cn_pi=cn_pi,cn_gp_pwid=cn_gp_pwid,cn_pi_pwid=cn_pi_pwid,cn_gp_pi=cn_gp_pi,cn_gp_pwid_pi=cn_gp_pwid_pi,
              rt_gp=rt_gp,rt_pwid=rt_pwid,rt_pi=rt_pi,rt_gp_pwid=rt_gp_pwid,rt_pi_pwid=rt_pi_pwid,rt_gp_pi=rt_gp_pi,rt_gp_pwid_pi=rt_gp_pwid_pi,
              pcum_gp=pcum_gp,pcum_pwid=pcum_pwid,pcum_pi=pcum_pi,pcum_gp_pwid=pcum_gp_pwid,pcum_pi_pwid=pcum_pi_pwid,pcum_gp_pi=pcum_gp_pi,pcum_gp_pwid_pi=pcum_gp_pwid_pi,
              tot_treats_gp=tot_treats_gp,tot_treats_pwid=tot_treats_pwid,tot_treats_pi=tot_treats_pi,tot_treats_gp_pwid=tot_treats_gp_pwid,tot_treats_pi_pwid=tot_treats_pi_pwid,tot_treats_gp_pi=tot_treats_gp_pi,tot_treats_gp_pwid_pi=tot_treats_gp_pwid_pi,
              casepert_gp=casepert_gp,casepert_pwid=casepert_pwid,casepert_pi=casepert_pi,casepert_gp_pwid=casepert_gp_pwid,casepert_pi_pwid=casepert_pi_pwid,casepert_gp_pi=casepert_gp_pi,casepert_gp_pwid_pi=casepert_gp_pwid_pi,
              prevc_gp=prevc_gp,prevc_pwid=prevc_pwid,prevc_pi=prevc_pi,prevc_gp_pwid=prevc_gp_pwid,prevc_pi_pwid=prevc_pi_pwid,prevc_gp_pi=prevc_gp_pi,prevc_gp_pwid_pi=prevc_gp_pwid_pi,
              icer_gp=icer_gp,icer_pwid=icer_pwid,icer_pi=icer_pi,icer_gp_pwid=icer_gp_pwid,icer_pi_pwid=icer_pi_pwid,icer_gp_pi=icer_gp_pi,icer_gp_pwid_pi=icer_gp_pwid_pi
  ))
}

getallpop_all_v2_pwid_for_prison=function(X,L,age_weights_current,age_weights_former,nt,age_scale00,age_scale10){
  #X has 720 rows
  #L is the list of all the compartment index

  if (dim(X)[1]==1800){
    Aformer = matrix(rep(age_weights_former,nt),nrow=9,ncol=nt)
    Apwid = matrix(rep(age_weights_current,nt),nrow=9,ncol=nt)
  with(L,{
    lt2=age_scale00*colSums(Aformer*X[LT2_comps_00,])+age_scale10*colSums(Apwid*X[LT2_comps_10,])+
      age_scale00*colSums(Aformer*X[LT2_comps_01,])+age_scale10*colSums(Apwid*X[LT2_comps_11,])+
      age_scale10*(colSums(Apwid*X[360+LT2_comps_11,])+colSums(Apwid*X[720+LT2_comps_11,])+colSums(Apwid*X[1080+LT2_comps_11,]))+
      age_scale10*(colSums(Apwid*X[360+LT2_comps_10,])+colSums(Apwid*X[720+LT2_comps_10,])+colSums(Apwid*X[1080+LT2_comps_10,]))
    
    lt1=age_scale00*colSums(Aformer*X[LT1_comps_00,])+age_scale10*colSums(Apwid*X[LT1_comps_10,])+
      age_scale00*colSums(Aformer*X[LT1_comps_01,])+age_scale10*colSums(Apwid*X[LT1_comps_11,])+
      age_scale10*(colSums(Apwid*X[360+LT1_comps_11,])+colSums(Apwid*X[720+LT1_comps_11,])+colSums(Apwid*X[1080+LT1_comps_11,]))+
      age_scale10*(colSums(Apwid*X[360+LT1_comps_10,])+colSums(Apwid*X[720+LT1_comps_10,])+colSums(Apwid*X[1080+LT1_comps_10,]))
    
    lt=lt1+lt2
    dc=age_scale00*colSums(Aformer*X[DC_comps_00,])+age_scale10*colSums(Apwid*X[DC_comps_10,])+
      age_scale00*colSums(Aformer*X[DC_comps_01,])+age_scale10*colSums(Apwid*X[DC_comps_11,])+
      age_scale10*(colSums(Apwid*X[360+DC_comps_11,])+colSums(Apwid*X[720+DC_comps_11,])+colSums(Apwid*X[1080+DC_comps_11,]))+
      age_scale10*(colSums(Apwid*X[360+DC_comps_10,])+colSums(Apwid*X[720+DC_comps_10,])+colSums(Apwid*X[1080+DC_comps_10,]))
    
    hcc=age_scale00*colSums(Aformer*X[HCC_comps_00,])+age_scale10*colSums(Apwid*X[HCC_comps_10,])+
      age_scale00*colSums(Aformer*X[HCC_comps_01,])+age_scale10*colSums(Apwid*X[HCC_comps_11,])+
      age_scale10*(colSums(Apwid*X[360+HCC_comps_11,])+colSums(Apwid*X[720+HCC_comps_11,])+colSums(Apwid*X[1080+HCC_comps_11,]))+
      age_scale10*(colSums(Apwid*X[360+HCC_comps_10,])+colSums(Apwid*X[720+HCC_comps_10,])+colSums(Apwid*X[1080+HCC_comps_10,]))
    
    
    f4=age_scale00*colSums(Aformer*X[F4_comps_00,])+age_scale10*colSums(Apwid*X[F4_comps_10,])+
      age_scale00*colSums(Aformer*X[F4_comps_01,])+age_scale10*colSums(Apwid*X[F4_comps_11,])+
      age_scale10*(colSums(Apwid*X[360+F4_comps_11,])+colSums(Apwid*X[720+F4_comps_11,])+colSums(Apwid*X[1080+F4_comps_11,]))+
      age_scale10*(colSums(Apwid*X[360+F4_comps_10,])+colSums(Apwid*X[720+F4_comps_10,])+colSums(Apwid*X[1080+F4_comps_10,]))
    
    
    f3=age_scale00*colSums(Aformer*X[F3_comps_00,])+age_scale10*colSums(Apwid*X[F3_comps_10,])+
      age_scale00*colSums(Aformer*X[F3_comps_01,])+age_scale10*colSums(Apwid*X[F3_comps_11,])+
      age_scale10*(colSums(Apwid*X[360+F3_comps_11,])+colSums(Apwid*X[720+F3_comps_11,])+colSums(Apwid*X[1080+F3_comps_11,]))+
      age_scale10*(colSums(Apwid*X[360+F3_comps_10,])+colSums(Apwid*X[720+F3_comps_10,])+colSums(Apwid*X[1080+F3_comps_10,]))
    
    f2=age_scale00*colSums(Aformer*X[F2_comps_00,])+age_scale10*colSums(Apwid*X[F2_comps_10,])+
      age_scale00*colSums(Aformer*X[F2_comps_01,])+age_scale10*colSums(Apwid*X[F2_comps_11,])+
      age_scale10*(colSums(Apwid*X[360+F2_comps_11,])+colSums(Apwid*X[720+F2_comps_11,])+colSums(Apwid*X[1080+F2_comps_11,]))+
      age_scale10*(colSums(Apwid*X[360+F2_comps_10,])+colSums(Apwid*X[720+F2_comps_10,])+colSums(Apwid*X[1080+F2_comps_10,]))
    
    
    f1=age_scale00*colSums(Aformer*X[F1_comps_00,])+age_scale10*colSums(Apwid*X[F1_comps_10,])+
      age_scale00*colSums(Aformer*X[F1_comps_01,])+age_scale10*colSums(Apwid*X[F1_comps_11,])+
      age_scale10*(colSums(Apwid*X[360+F1_comps_11,])+colSums(Apwid*X[720+F1_comps_11,])+colSums(Apwid*X[1080+F1_comps_11,]))+
      age_scale10*(colSums(Apwid*X[360+F1_comps_10,])+colSums(Apwid*X[720+F1_comps_10,])+colSums(Apwid*X[1080+F1_comps_10,]))
    
    
    f0=age_scale00*colSums(Aformer*X[F0_comps_00,])+age_scale10*colSums(Apwid*X[F0_comps_10,])+
      age_scale00*colSums(Aformer*X[F0_comps_01,])+age_scale10*colSums(Apwid*X[F0_comps_11,])+
      age_scale10*(colSums(Apwid*X[360+F0_comps_11,])+colSums(Apwid*X[720+F0_comps_11,])+colSums(Apwid*X[1080+F0_comps_11,]))+
      age_scale10*(colSums(Apwid*X[360+F0_comps_10,])+colSums(Apwid*X[720+F0_comps_10,])+colSums(Apwid*X[1080+F0_comps_10,]))
    
    
    return(list(lt=lt,dc=dc,hcc=hcc,f0=f0,f1=f1,f2=f2,f3=f3,f4=f4))
  })
  }else{
    # gen pop
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
    return(list(lt=lt,dc=dc,hcc=hcc,f0=f0,f1=f1,f2=f2,f3=f3,f4=f4))
  }
  
}


plot_areacomps_v3_pwid_for_prison=function(times,C,type_of_plot){
  nt=length(times)
  if (type_of_plot==1){
    titly = "Gen pop model - Number in chronic compartments"
  }else if (type_of_plot==2){
    titly = "PWID model - Number in chronic compartments"
  } else if (type_of_plot==3){
    titly = "Prison model - Number in chronic compartments"
  } else if (type_of_plot==4){
    titly = "Gen pop & PWID model - Number in chronic compartments"
  } else if (type_of_plot==5){
    titly = "Prison & PWID model - Number in chronic compartments"
  } else if (type_of_plot==6){
    titly = "Gen pop & prison model - Number in chronic compartments"
  } else if (type_of_plot==7){
    titly = "Gen pop, prison & PWID model - Number in chronic compartments"
  }
  y=c(C$f0,C$f1,C$f2,C$f3,C$f4,C$dc,C$hcc,C$lt)
  Stage=c(rep("F0",nt),rep("F1",nt),rep("F2",nt),rep("F3",nt),rep("F4",nt),rep("DC",nt),rep("HCC",nt),rep("LT",nt))
  t=rep(times,8)
  df=data.frame(y,t,Stage)
  maxy=max(pretty_breaks()(c(0, (C$f0+C$f1+C$f2+C$f3+C$f4+C$dc+C$hcc+C$lt))))
  vecy=pretty_breaks()(c(0, (C$f0+C$f1+C$f2+C$f3+C$f4+C$dc+C$hcc+C$lt)))
  df$Stage=factor(df$Stage,levels=c("F0","F1","F2","F3","F4","DC","HCC","LT"))
  p1=ggplot(data=df, aes(x=t, y=y,fill=Stage)) + geom_area() +
    xlab("Year") + ylab("Population") + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = times[1]:times[nt],expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)  
}

funky = function(a,b,c){ 
  # used for Map function to add dataframes
  if (length(c)==1) { # no warning this way is.na(c) gives warnings
    z=a+b
  }else{
    z=a+b+c
  }
  }


calc_deaths_for_plot=function(timein,death_vec,XT,scale10,scale00){
  
  time_cut=min(timein)-2015+1
  time_end=max(timein)-2015+1
  XTdeaths=rep(0,time_end-time_cut+1)
  if (dim(XT)[1]==1800){
      # prison or PWID
      # Now can have deaths in the failed treatment compartments
      testcompvec00=t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
      testcompvec10=t(matrix(360+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
      testcompvec01_failed=t(matrix(c(seq(192,360,20),seq(193,360,20),seq(194,360,20),seq(195,360,20)),nrow=9,ncol=4))
      testcompvec11_failed=t(matrix(c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
      
      testcompvec10_nep=t(matrix(720+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
      testcompvec11_failed_nep=t(matrix(360+c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
      
      testcompvec10_ost=t(matrix(1080+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
      testcompvec11_failed_ost=t(matrix(720+c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
      
      testcompvec10_nep_ost=t(matrix(1440+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
      testcompvec11_failed_nep_ost=t(matrix(1080+c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
      
      Xbot_deaths=0
      tval = 1 # This is 2015
      multy = 0.5 # ref Bruno
      for (i in 1 : 4){
        Xtest=death_vec[i]*scale10*colSums(XT[testcompvec10[i,],])
        Ytest=death_vec[i]*scale00*colSums(XT[testcompvec00[i,],])
        X1test=death_vec[i]*scale10*colSums(XT[testcompvec11_failed[i,],])
        Y1test=death_vec[i]*scale00*colSums(XT[testcompvec01_failed[i,],])
        
        X1test_nep = death_vec[i]*scale10*colSums(XT[testcompvec10_nep[i,],])
        Y1test_nep = death_vec[i]*scale10*colSums(XT[testcompvec11_failed_nep[i,],])
        
        X1test_ost = death_vec[i]*scale10*colSums(XT[testcompvec10_ost[i,],])
        Y1test_ost = death_vec[i]*scale10*colSums(XT[testcompvec11_failed_ost[i,],])
        
        X1test_nep_ost = death_vec[i]*scale10*colSums(XT[testcompvec10_nep_ost[i,],])
        Y1test_nep_ost = death_vec[i]*scale10*colSums(XT[testcompvec11_failed_nep_ost[i,],])
        
        XTdeaths=XTdeaths+Xtest[time_cut:time_end]+Ytest[time_cut:time_end]+
          X1test[time_cut:time_end]+Y1test[time_cut:time_end]+
          X1test_nep[time_cut:time_end]+Y1test_nep[time_cut:time_end]+
          X1test_ost[time_cut:time_end]+Y1test_ost[time_cut:time_end]+
          X1test_nep_ost[time_cut:time_end]+Y1test_nep_ost[time_cut:time_end]
        Xbot_deaths = Xbot_deaths + Xtest[tval]+Ytest[tval]+
          X1test[tval]+Y1test[tval]+
          X1test_nep[tval]+Y1test_nep[tval]+
          X1test_ost[tval]+Y1test_ost[tval]+
          X1test_nep_ost[tval]+Y1test_nep_ost[tval]
      }
  }else{
    # GP
    testcompvec00=t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
    testcompvec00_screened=t(matrix(c(seq(192,360,20),seq(193,360,20),seq(194,360,20),seq(195,360,20)),nrow=9,ncol=4))
    Xbot_deaths=0
    tval = 1 # This is 2015
    multy = 0.5 # ref Bruno
    for (i in 1 : 4){
      Xtest=death_vec[i]*colSums(XT[testcompvec00_screened[i,],])
      Ytest=death_vec[i]*colSums(XT[testcompvec00[i,],])
      XTdeaths=XTdeaths+Xtest[time_cut:time_end]+Ytest[time_cut:time_end]
      Xbot_deaths = Xbot_deaths + Xtest[tval]+Ytest[tval]
    }
    # don't actually have good starting value for gen pop deaths
    m=XTdeaths[3]-XTdeaths[2]
    ic = XTdeaths[3]-3*m
    Xbot_deaths=m+ic
    #Xbot_deaths = XTdeaths[1]*(1-(XTdeaths[2]-XTdeaths[1])/XTdeaths[1])
  }
  return(list(XTdeaths=XTdeaths,Xbot_deaths=Xbot_deaths))
}


calc_deaths_for_plot_extra=function(timein,death_vec,XT,scale10,scale00,
                                    pcom,alpha,time_cut,
                                    DAAtreat,SCreening,
                                    enroll_nep,enroll_ost,enroll_nep_ost){
  
  time_cut=min(timein)-2015+1
  time_end=max(timein)-2015+1
  XTdeaths=rep(0,time_end-time_cut+1)
  if (dim(XT)[1]==1800){
    t1=2015:rev(timein)[1]
    # prison or PWID
    # Now can have deaths in the failed treatment compartments
    testcompvec00=t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
    testcompvec10=t(matrix(360+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
    testcompvec01_failed=t(matrix(c(seq(192,360,20),seq(193,360,20),seq(194,360,20),seq(195,360,20)),nrow=9,ncol=4))
    testcompvec11_failed=t(matrix(c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
    
    testcompvec10_nep=t(matrix(720+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
    testcompvec11_failed_nep=t(matrix(360+c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
    
    testcompvec10_ost=t(matrix(1080+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
    testcompvec11_failed_ost=t(matrix(720+c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
    
    testcompvec10_nep_ost=t(matrix(1440+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
    testcompvec11_failed_nep_ost=t(matrix(1080+c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
    
    
    # need to add back in deaths for DC/HCC and LT
    addbackindeaths00=matrix(data = 0, nrow = 4, ncol=ncol(XT)) # rows are deaths in DC/HCC/LT1/LT2
    addbackindeaths01=matrix(data = 0, nrow = 4, ncol=ncol(XT))
    addbackindeaths10=matrix(data = 0, nrow = 4, ncol=ncol(XT))
    addbackindeaths11=matrix(data = 0, nrow = 4, ncol=ncol(XT))
    addbackindeaths10_nep=matrix(data = 0, nrow = 4, ncol=ncol(XT))
    addbackindeaths11_nep=matrix(data = 0, nrow = 4, ncol=ncol(XT))
    addbackindeaths10_ost=matrix(data = 0, nrow = 4, ncol=ncol(XT))
    addbackindeaths11_ost=matrix(data = 0, nrow = 4, ncol=ncol(XT))
    addbackindeaths10_nep_ost=matrix(data = 0, nrow = 4, ncol=ncol(XT))
    addbackindeaths11_nep_ost=matrix(data = 0, nrow = 4, ncol=ncol(XT))
    for (i in 1 : ncol(XT)){
      N=XT[,i]
      dum=treat_comps_pv7(N,scale00,scale10,t1[i],pcom,alpha,time_cut,
                          DAAtreat,SCreening,
                          enroll_nep,enroll_ost,enroll_nep_ost)
      if (i > time_cut){
        addbackindeaths00[,i]=dum$direct_deaths[1,]
        addbackindeaths01[,i]=dum$direct_deaths[2,]
        addbackindeaths10[,i]=dum$direct_deaths[3,]
        addbackindeaths11[,i]=dum$direct_deaths[4,]
        
        addbackindeaths10_nep[,i]=dum$direct_deaths_nep[1,]
        addbackindeaths10_ost[,i]=dum$direct_deaths_ost[1,]
        addbackindeaths10_nep_ost[,i]=dum$direct_deaths_nep_ost[1,]
        
        addbackindeaths10_nep[,i]=dum$direct_deaths_nep[2,]
        addbackindeaths10_ost[,i]=dum$direct_deaths_ost[2,]
        addbackindeaths10_nep_ost[,i]=dum$direct_deaths_nep_ost[2,]
      }
    }
    
    Xbot_deaths=0
    tval = 1 # This is 2015
    multy = 0.5 # ref Bruno
    for (i in 1 : 4){
      Xtest=death_vec[i]*scale10*colSums(XT[testcompvec10[i,],])
      Xtest=Xtest + multy*death_vec[i]*addbackindeaths10[i,]
      Ytest=death_vec[i]*scale00*colSums(XT[testcompvec00[i,],])
      Ytest=Ytest + multy*death_vec[i]*addbackindeaths00[i,]
      X1test=death_vec[i]*scale10*colSums(XT[testcompvec11_failed[i,],])
      X1test=X1test + multy*death_vec[i]*addbackindeaths11[i,]
      Y1test=death_vec[i]*scale00*colSums(XT[testcompvec01_failed[i,],])
      Y1test=Y1test + multy*death_vec[i]*addbackindeaths01[i,]
      
      X1test_nep = death_vec[i]*scale10*colSums(XT[testcompvec10_nep[i,],])
      X1test_nep = X1test_nep+multy*death_vec[i]*addbackindeaths10_nep[i,]
      Y1test_nep = death_vec[i]*scale10*colSums(XT[testcompvec11_failed_nep[i,],])
      Y1test_nep = Y1test_nep+multy*death_vec[i]*addbackindeaths11_nep[i,]      
      
      X1test_ost = death_vec[i]*scale10*colSums(XT[testcompvec10_ost[i,],])
      X1test_ost = X1test_ost+multy*death_vec[i]*addbackindeaths10_ost[i,]
      Y1test_ost = death_vec[i]*scale10*colSums(XT[testcompvec11_failed_ost[i,],])
      Y1test_ost = Y1test_ost+multy*death_vec[i]*addbackindeaths11_ost[i,]
      
      X1test_nep_ost = death_vec[i]*scale10*colSums(XT[testcompvec10_nep_ost[i,],])
      X1test_nep_ost = X1test_nep_ost+multy*death_vec[i]*addbackindeaths10_nep_ost[i,]
      Y1test_nep_ost = death_vec[i]*scale10*colSums(XT[testcompvec11_failed_nep_ost[i,],])
      Y1test_nep_ost = Y1test_nep_ost+multy*death_vec[i]*addbackindeaths11_nep_ost[i,]
      
      XTdeaths=XTdeaths+Xtest[time_cut:time_end]+Ytest[time_cut:time_end]+
        X1test[time_cut:time_end]+Y1test[time_cut:time_end]+
        X1test_nep[time_cut:time_end]+Y1test_nep[time_cut:time_end]+
        X1test_ost[time_cut:time_end]+Y1test_ost[time_cut:time_end]+
        X1test_nep_ost[time_cut:time_end]+Y1test_nep_ost[time_cut:time_end]
      Xbot_deaths = Xbot_deaths + Xtest[tval]+Ytest[tval]+
        X1test[tval]+Y1test[tval]+
        X1test_nep[tval]+Y1test_nep[tval]+
        X1test_ost[tval]+Y1test_ost[tval]+
        X1test_nep_ost[tval]+Y1test_nep_ost[tval]
    }
  }else{
    # GP
    testcompvec00=t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
    testcompvec00_screened=t(matrix(c(seq(192,360,20),seq(193,360,20),seq(194,360,20),seq(195,360,20)),nrow=9,ncol=4))
    Xbot_deaths=0
    tval = 1 # This is 2015
    multy = 0.5 # ref Bruno
    for (i in 1 : 4){
      Xtest=death_vec[i]*colSums(XT[testcompvec00_screened[i,],])
      Ytest=death_vec[i]*colSums(XT[testcompvec00[i,],])
      XTdeaths=XTdeaths+Xtest[time_cut:time_end]+Ytest[time_cut:time_end]
      Xbot_deaths = Xbot_deaths + Xtest[tval]+Ytest[tval]
    }
    # don't actually have good starting value for gen pop deaths
    m=XTdeaths[3]-XTdeaths[2]
    ic = XTdeaths[3]-3*m
    Xbot_deaths=m+ic
    #Xbot_deaths = XTdeaths[1]*(1-(XTdeaths[2]-XTdeaths[1])/XTdeaths[1])
  }
  return(list(XTdeaths=XTdeaths,Xbot_deaths=Xbot_deaths))
}

plot_output_mortality_v2_all_PWID_prison_gp =function(XT_gp,XT_pwid,XT_prison,timein,type_of_plot,scale00,scale10,prison_scale00,prison_scale10,typey,ignore_flows,
                                                      pcom,alpha,time_cut,L_PWID,L_PRIS)
{

  # add the SVR deaths
  # XT are the compartments
  # death_vec ar ethe mortality rates
  # scale00 and scale 10 are the scaling factors
  time_cut=min(timein)-2015+1
  time_end=max(timein)-2015+1
  scenario_number=122
  dum=readRDS(paste0("www/scenario_",scenario_number,"_param_PWID.rds"))
  param_vals_base=dum[1:916]
  
  death_vec=param_vals_base[913:916] # This is already a rate -log(1-0.138) etc
  #death_vec=1-exp(-death_vec)
  prison_P=incarceration_parameters_withif(ignore_flows)
  prison_death_vec=death_vec*prison_P$liver_related_mort_rr_increase
  
  DAAtreat_pwid = L_PWID$DAAtreat
  SCreening_pwid = L_PWID$SCreening
  NSPOST_pwid = L_PWID$NSPOST
  enroll_nep_pwid=NSPOST_pwid$NSPcov
  enroll_ost_pwid=NSPOST_pwid$OSTcov
  enroll_nep_ost_pwid=NSPOST_pwid$NSPOSTcov
  
  DAAtreat_pris = L_PRIS$DAAtreat
  SCreening_pris = L_PRIS$SCreening
  NSPOST_pris = L_PRIS$NSPOST
  enroll_nep_pris=NSPOST_pris$NSPcov
  enroll_ost_pris=NSPOST_pris$OSTcov
  enroll_nep_ost_pris=NSPOST_pris$NSPOSTcov
  
  
  if (type_of_plot==1){
    L=calc_deaths_for_plot(timein,death_vec,XT_gp,1,1)
    XTdeaths=L$XTdeaths
    Xbot_deaths=L$Xbot_deaths
    if (typey==1){
      titly = "Gen pop model - % change in liver related mortality (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Gen pop model - Number of liver related deaths (DCC,HCC,LT)"  
    }
  }else if (type_of_plot==2){
    L=calc_deaths_for_plot_extra(timein,death_vec,XT_pwid,scale10,scale00,
                                 pcom,alpha,time_cut,
                                 DAAtreat_pwid,SCreening_pwid,
                                 enroll_nep_pwid,enroll_ost_pwid,enroll_nep_ost_pwid
                                 )
    # L=calc_deaths_for_plot(timein,death_vec,XT_pwid,scale10,scale00)
    XTdeaths=L$XTdeaths
    Xbot_deaths=L$Xbot_deaths
    if (typey==1){
      titly = "PWID model - % change in liver related mortality (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "PWID model - Number of liver related deaths (DCC,HCC,LT)"
    }
  } else if (type_of_plot==3){
    L=calc_deaths_for_plot_extra(timein,death_vec,XT_prison,prison_scale10,prison_scale00,
                                 pcom,alpha,time_cut,
                                 DAAtreat_pris,SCreening_pris,
                                 enroll_nep_pris,enroll_ost_pris,enroll_nep_ost_pris
    )
    # L=calc_deaths_for_plot(timein,prison_death_vec,XT_prison,prison_scale10,prison_scale00)
    XTdeaths=L$XTdeaths
    Xbot_deaths=L$Xbot_deaths
    if (typey==1){
      titly = "Prison model - % change in liver related mortality (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Prison model - Number of liver related deaths (DCC,HCC,LT)"
    }
  } else if (type_of_plot==4){
    L=calc_deaths_for_plot(timein,death_vec,XT_gp,1,1)
    XTdeaths=L$XTdeaths
    Xbot_deaths=L$Xbot_deaths
    L=calc_deaths_for_plot_extra(timein,death_vec,XT_pwid,scale10,scale00,
                                 pcom,alpha,time_cut,
                                 DAAtreat_pwid,SCreening_pwid,
                                 enroll_nep_pwid,enroll_ost_pwid,enroll_nep_ost_pwid
    )
    # L=calc_deaths_for_plot(timein,death_vec,XT_pwid,scale10,scale00)
    XTdeaths=XTdeaths+L$XTdeaths
    Xbot_deaths=Xbot_deaths+L$Xbot_deaths
    if (typey==1){
      titly = "Gen pop & PWID model - % change in liver related mortality (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Gen pop & PWID model - Number of liver related deaths (DCC,HCC,LT)"
    }
  } else if (type_of_plot==5){
    L=calc_deaths_for_plot_extra(timein,death_vec,XT_prison,prison_scale10,prison_scale00,
                                 pcom,alpha,time_cut,
                                 DAAtreat_pris,SCreening_pris,
                                 enroll_nep_pris,enroll_ost_pris,enroll_nep_ost_pris
    )
    # L=calc_deaths_for_plot(timein,prison_death_vec,XT_prison,prison_scale10,prison_scale00)
    XTdeaths=L$XTdeaths
    Xbot_deaths=L$Xbot_deaths
    L=calc_deaths_for_plot(timein,death_vec,XT_pwid,scale10,scale00)
    XTdeaths=XTdeaths+L$XTdeaths
    Xbot_deaths=Xbot_deaths+L$Xbot_deaths    
    if (typey==1){
      titly = "Prison & PWID model - % change in liver related mortality (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Prison & PWID model - Number of liver related deaths (DCC,HCC,LT)"
    }
  } else if (type_of_plot==6){
    L=calc_deaths_for_plot(timein,death_vec,XT_gp,1,1)
    XTdeaths=L$XTdeaths
    Xbot_deaths=L$Xbot_deaths   
    L=calc_deaths_for_plot_extra(timein,death_vec,XT_prison,prison_scale10,prison_scale00,
                                 pcom,alpha,time_cut,
                                 DAAtreat_pris,SCreening_pris,
                                 enroll_nep_pris,enroll_ost_pris,enroll_nep_ost_pris
    )
    # L=calc_deaths_for_plot(timein,prison_death_vec,XT_prison,prison_scale10,prison_scale00)
    XTdeaths=XTdeaths+L$XTdeaths
    Xbot_deaths=Xbot_deaths+L$Xbot_deaths
    if (typey==1){
      titly = "Gen pop & prison model - % change in liver related mortality (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Gen pop & prison model - Number of liver related deaths (DCC,HCC,LT)"
    }
  } else if (type_of_plot==7){
    L=calc_deaths_for_plot(timein,death_vec,XT_gp,1,1)
    XTdeaths=L$XTdeaths
    Xbot_deaths=L$Xbot_deaths   
    L=calc_deaths_for_plot_extra(timein,death_vec,XT_prison,prison_scale10,prison_scale00,
                                 pcom,alpha,time_cut,
                                 DAAtreat_pris,SCreening_pris,
                                 enroll_nep_pris,enroll_ost_pris,enroll_nep_ost_pris
    )
    # L=calc_deaths_for_plot(timein,prison_death_vec,XT_prison,prison_scale10,prison_scale00)
    XTdeaths=XTdeaths+L$XTdeaths
    Xbot_deaths=Xbot_deaths+L$Xbot_deaths
    L=calc_deaths_for_plot_extra(timein,death_vec,XT_pwid,scale10,scale00,
                                 pcom,alpha,time_cut,
                                 DAAtreat_pwid,SCreening_pwid,
                                 enroll_nep_pwid,enroll_ost_pwid,enroll_nep_ost_pwid
    )    
    # L=calc_deaths_for_plot(timein,death_vec,XT_pwid,scale10,scale00)
    XTdeaths=XTdeaths+L$XTdeaths
    Xbot_deaths=Xbot_deaths+L$Xbot_deaths       
    if (typey==1){
      titly = "Gen pop, prison & PWID model - % change in liver related mortality (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Gen pop, prison & PWID model - Number of liver related deaths (DCC,HCC,LT)"
    }
  }
  
  if (typey==1){
    df= data.frame(x=timein,y=100*((XTdeaths/Xbot_deaths) - 1)) # XTdeaths[1]
    #titly="% change in liver related mortality (DCC,HCC,LT) compared to 2015"
    ylaby="Change %"
    ywholim=-65
    minylim= -65*105.5/100
  }else{
    df= data.frame(x=timein,y=XTdeaths)
    #titly="Number of liver related deaths (DCC,HCC,LT)"
    ylaby="# deaths"
    ywholim = 0.35*Xbot_deaths
    minylim = 0.95*ywholim
  }
  
  maxy=max(pretty_breaks()(c(0, df$y)))
  miny=min(min(pretty_breaks()(c(0, df$y))),minylim)
  vecy=pretty_breaks()(c(miny, df$y))
  maxy = max(vecy)
  miny=min(vecy)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab(ylaby) + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(miny,maxy),breaks=vecy)+
    scale_x_continuous(breaks = timein,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_hline(yintercept = ywholim,color="blue",size=2) + 
    annotate("text", min(timein)+time_cut+5, ywholim, vjust = -1, label = "WHO mortality target")
  return(list(p1=p1,morty=XTdeaths))
}



calc_chronic_cases_for_plot=function(timein,curr_mort_former,curr_mort_pwid,delta,rateA_F0,XT,scale10,scale00){
  
  time_cut=min(timein)-2015+1
  time_end=max(timein)-2015+1
  tval=1 # 2015
  XTdeaths=rep(0,time_end-time_cut+1)
  A_comps_00 = seq(6,180,20)
  A_comps_01 = A_comps_00 + 180
  A_comps_10 = A_comps_01 + 180
  A_comps_11 = A_comps_10 + 180

  
  if (dim(XT)[1]==1800){
    # prison or PWID, only acute compartments
    
    T00=(1-delta)*rateA_F0*scale00*colSums((1-curr_mort_former)%*%XT[A_comps_00,]);
    T01=(1-delta)*rateA_F0*scale00*colSums((1-curr_mort_former)%*%XT[A_comps_01,]);
    T11=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_11,]);
    T10=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_10,]);
    
    T11_nep=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_11+360,]);
    T10_nep=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_10+360,]);
    
    T11_ost=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_11+720,]);
    T10_ost=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_10+720,]);
    
    T11_nep_ost=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_11+1080,]);
    T10_nep_ost=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_10+1080,]);
 
    tval=3
    yinci_bot3 = T00[tval]+T01[tval]+T10[tval]+T11[tval]+
                     T10_nep[tval]+T11_nep[tval]+
                     T10_ost[tval]+T11_ost[tval]+
                     T10_nep_ost[tval]+T11_nep_ost[tval]
    tval=2
    yinci_bot2 = T00[tval]+T01[tval]+T10[tval]+T11[tval]+
      T10_nep[tval]+T11_nep[tval]+
      T10_ost[tval]+T11_ost[tval]+
      T10_nep_ost[tval]+T11_nep_ost[tval]
    yinci_bot=2*yinci_bot2 - yinci_bot3 # interpolate Y3-Y2=Y2-y1, so y1 = 2*Y2-Y3
    yinci=T00[time_cut:time_end]+T01[time_cut:time_end]+T10[time_cut:time_end]+T11[time_cut:time_end]+
               T10_nep[time_cut:time_end]+T11_nep[time_cut:time_end]+
               T10_ost[time_cut:time_end]+T11_ost[time_cut:time_end]+
               T10_nep_ost[time_cut:time_end]+T11_nep_ost[time_cut:time_end]
  } else{
    T00=(1-delta)*rateA_F0*as.vector((1-curr_mort_former)%*%XT[A_comps_00,])
    T01=(1-delta)*rateA_F0*as.vector((1-curr_mort_former)%*%XT[A_comps_01,])
    yinci_bot = T00[tval+3]+T01[tval+3] # allow run in
    yinci=T00[time_cut:time_end]+T01[time_cut:time_end]
  }

  return(list(XTnewchronic=yinci,Xbot_newchronic=yinci_bot))
}

plot_output_incidence_new_chronic_cases_v2_all_PWID_prison_gp=function(XT_gp,XT_pwid,XT_prison,timein,type_of_plot,scale00,scale10,prison_scale00,prison_scale10,typey){

  # XT_gp,XT_pwid, XT_prison are the output from the compartment model

  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters frm the base line fit
  # same for prison values fo rprison pop
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  
  time_cut=min(timein)-2015+1
  time_end=max(timein)-2015+1
  scenario_number=122
  dum=readRDS(paste0("www/scenario_",scenario_number,"_param_PWID.rds"))
  param_vals_base=dum[1:916]
  curr_mort_pwid=param_vals_base[895:903]
  curr_mort_former=param_vals_base[904:912]
  # the same for all populations at present 22 August 2021
  rA_F0= -param_vals_base[106]
  delta=param_vals_base[6]/rA_F0
  
  
  if (type_of_plot==1){
    L=calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_gp,1,1)
    XTnewchronic=L$XTnewchronic
    Xbot_newchronic=L$Xbot_newchronic
    if (typey==1){
      titly = "Gen pop model - % change in new chronic cases (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Gen pop model - Number of new chronic cases (DCC,HCC,LT)"  
    }
  }else if (type_of_plot==2){
    L=calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_pwid,scale10,scale00) 
    XTnewchronic=L$XTnewchronic
    Xbot_newchronic=L$Xbot_newchronic
    if (typey==1){
      titly = "PWID model - % change in new chronic cases (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "PWID model - Number of new chronic cases (DCC,HCC,LT)"
    }
  } else if (type_of_plot==3){
    L=calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_prison,prison_scale10,prison_scale00) 
    XTnewchronic=L$XTnewchronic
    Xbot_newchronic=L$Xbot_newchronic
    if (typey==1){
      titly = "Prison model - % change in new chronic cases (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Prison model - Number of new chronic cases (DCC,HCC,LT)"
    }
  } else if (type_of_plot==4){
    L=calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_gp,1,1)
    XTnewchronic=L$XTnewchronic
    Xbot_newchronic=L$Xbot_newchronic
    L=calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_pwid,scale10,scale00) 
    XTnewchronic=XTnewchronic+L$XTnewchronic
    Xbot_newchronic=Xbot_newchronic+L$Xbot_newchronic
    if (typey==1){
      titly = "Gen pop & PWID model - % change in new chronic cases (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Gen pop & PWID model - Number of new chronic cases (DCC,HCC,LT)"
    }
  } else if (type_of_plot==5){
    L= calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_prison,prison_scale10,prison_scale00)
    XTnewchronic=L$XTnewchronic
    Xbot_newchronic=L$Xbot_newchronic
    L=calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_pwid,scale10,scale00) 
    XTnewchronic=XTnewchronic+L$XTnewchronic
    Xbot_newchronic=Xbot_newchronic+L$Xbot_newchronic    
    if (typey==1){
      titly = "Prison & PWID model - % change in new chronic cases (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Prison & PWID model - Number of new chronic cases (DCC,HCC,LT)"
    }
  } else if (type_of_plot==6){
    L=calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_gp,1,1)
    XTnewchronic=L$XTnewchronic
    Xbot_newchronic=L$Xbot_newchronic   
    L= calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_prison,prison_scale10,prison_scale00)
    XTnewchronic=XTnewchronic+L$XTnewchronic
    Xbot_newchronic=Xbot_newchronic+L$Xbot_newchronic
    if (typey==1){
      titly = "Gen pop & prison model - % change in new chronic cases (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Gen pop & prison model - Number of new chronic cases (DCC,HCC,LT)"
    }
  } else if (type_of_plot==7){
    L=calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_gp,1,1)
    XTnewchronic=L$XTnewchronic
    Xbot_newchronic=L$Xbot_newchronic   
    L=calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_prison,prison_scale10,prison_scale00)
    XTnewchronic=XTnewchronic+L$XTnewchronic
    Xbot_newchronic=Xbot_newchronic+L$Xbot_newchronic
    L=calc_chronic_cases_for_plot(timein,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_pwid,scale10,scale00)
    XTnewchronic=XTnewchronic+L$XTnewchronic
    Xbot_newchronic=Xbot_newchronic+L$Xbot_newchronic       
    if (typey==1){
      titly = "Gen pop, prison & PWID model - % change in new chronic cases (DCC,HCC,LT) compared to 2015"
    }else{
      titly = "Gen pop, prison & PWID model - Number of new chronic cases (DCC,HCC,LT)"
    }
  }
  
  
  if (typey==1){
    df= data.frame(x=timein,y=100*((XTnewchronic/Xbot_newchronic) - 1)) # XTdeaths[1]
    #titly="% change in liver related mortality (DCC,HCC,LT) compared to 2015"
    ylaby="Change %"
    ywholim=-80
    minylim= -80*105.5/100
  }else{
    df= data.frame(x=timein,y=XTnewchronic)
    #titly="Number of liver related deaths (DCC,HCC,LT)"
    ylaby="# of subjects"
    ywholim = 0.2*Xbot_newchronic
    minylim = ywholim * 0.95
  }

  
  maxy=max(pretty_breaks()(c(0, df$y)))
  miny=min(min(pretty_breaks()(c(0, df$y))),minylim)
  vecy=pretty_breaks()(c(miny, df$y))
  maxy = max(vecy)
  miny=min(vecy)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab(ylaby) + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(miny,maxy),breaks=vecy)+
    scale_x_continuous(breaks = timein,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    geom_hline(yintercept = ywholim,color="blue",size=2) + 
    annotate("text", min(timein)+time_cut+5, ywholim, vjust = -1, label = "WHO new infections target")
  return(list(p1=p1,inci=XTnewchronic))
}


# new type of plot
plot_cases_deaths_PWID_prison_gp=function(timein,inci_data,mort_data,numplot){
  # Note these country names come from vaccine data
  
  if (numplot==1){
    titly=paste0("GP Incident cases and deaths","        ")
  }else if(numplot==2){
    titly=paste0("PWID Incident cases and deaths","        ")
  }else if(numplot==3){
    titly=paste0("Prison Incident cases and deaths","        ")
  }else if(numplot==4){
    titly=paste0("Gen pop & PWID Incident cases and deaths","        ")
  }else if(numplot==5){
    titly=paste0("Prison & PWID Incident cases and deaths","        ")
  }else if(numplot==6){
    titly=paste0("Gen pop & prison Incident cases and deaths","        ")
  }else if(numplot==7){
    titly=paste0("Gen pop, PWID & prison Incident cases and deaths","        ")
  }
  cases=inci_data
  deaths=mort_data
  typeof = c(rep("HCV Incidence",length(cases)),rep("HCV Deaths",length(deaths)))
  Y=c(cases,-deaths)
  D=c(timein,timein)
  dat = data.frame(typeof=typeof,Y=Y,D=D)
  dat1 <- subset(dat,Y >= 0)
  dat2 <- subset(dat,Y < 0)
  dum1=pretty(c(0,dat1$Y))
  maxof_pos=max(dum1)
  dum2=pretty(c(0,dat2$Y))
  minof_neg=min(dum2)
  prettybig=comma(c(abs(dum2),dum1))
  prettysmall=c(dum2/abs(minof_neg),dum1/maxof_pos)
  dat1$Y=dat1$Y/maxof_pos
  d1=dat1$Y
  dat2$Y=dat2$Y/abs(minof_neg)
  d2=dat2$Y
  xx=min(dat1$D)
  
  p1=ggplot() +
    geom_bar(data = dat1, aes(x=D, y=Y, fill=typeof),stat = "identity",position ="identity",width = 0.5) +
    geom_bar(data = dat2, aes(x=D, y=Y, fill=typeof),stat = "identity",position ="identity",width = 0.5) +
    scale_fill_manual(titly, values = c("HCV Incidence" = "blue", "HCV Deaths" = "red"))+
    scale_y_continuous(breaks=prettysmall,label=prettybig)+
    scale_x_continuous(breaks = unique(D),expand=c(0,0))+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "top",legend.justification='left')+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(p1)
}

plot_point_prev_PWID_prison_gp=function(XT_gp,XT_pwid,XT_prison,timein,numplot,typeof,scale00,scale10,scale00_prison,scale10_prison){
  
  L=coeffsof_v5()
  time_cut=min(timein)-2015+1
  time_end=max(timein)-2015+1
  
  if (numplot==1){
    titly="Point prev:% of gen pop with chronic HCV"
    XT=XT_gp
  }else if(numplot==2){
    titly="Point prev:% of PWID with chronic HCV"
    XT=rbind(scale00*XT_pwid[1:360,],scale10*XT_pwid[361:1800,])
  }else if(numplot==3){
    titly="Point prev:% of prison with chronic HCV"
    XT=rbind(scale00_prison*XT_prison[1:360,],scale10_prison*XT_prison[361:1800,])
  }else if(numplot==4){
    titly="Point prev:% of gen pop & PWID with chronic HCV"
    dum=rbind(XT_gp,matrix(0,nrow=dim(XT_pwid)[1]-dim(XT_gp)[1],ncol=dim(XT_gp)[2]))
    XT=dum+rbind(scale00*XT_pwid[1:360,],scale10*XT_pwid[361:1800,])
  }else if(numplot==5){
    titly="Point prev:% of prison & PWID with chronic HCV"
    XT=rbind(scale00_prison*XT_prison[1:360,],scale10_prison*XT_prison[361:1800,])+
       rbind(scale00*XT_pwid[1:360,],scale10*XT_pwid[361:1800,])
  }else if(numplot==6){
    titly="Point prev:% of gen pop & prison with chronic HCV"
    dum=rbind(XT_gp,matrix(0,nrow=dim(XT_pwid)[1]-dim(XT_gp)[1],ncol=dim(XT_gp)[2]))
    XT=dum+rbind(scale00_prison*XT_prison[1:360,],scale10_prison*XT_prison[361:1800,])
  }else if(numplot==7){
    titly="Point prev:% of gen pop, PWID & prison with chronic HCV"
    dum=rbind(XT_gp,matrix(0,nrow=dim(XT_pwid)[1]-dim(XT_gp)[1],ncol=dim(XT_gp)[2]))
    XT=dum+rbind(scale00*XT_pwid[1:360,],scale10*XT_pwid[361:1800,])+
           rbind(scale00_prison*XT_prison[1:360,],scale10_prison*XT_prison[361:1800,])
  }
  laby="Percentage"
  if (typeof==2){
    titly=gsub("Point prev:%","Number",titly)
    laby="Number of subjects"
  }
  

  if (dim(XT)[1]==1800){
    # prison or PWID, only acute compartments
    
    chron_pwid = c(L$chronic00,L$chronic01,L$chronic10,L$chronic11)
    chron_pwid_nep =c((L$chronic10+360),(L$chronic11+360))
    chron_pwid_ost =c((L$chronic10+720),(L$chronic11+720))
    chron_pwid_nep_ost =c((L$chronic10+1080),(L$chronic11+1080))
    T1=XT[chron_pwid,time_cut:time_end]
    T2=XT[chron_pwid_nep,time_cut:time_end]
    T3=XT[chron_pwid_ost,time_cut:time_end]
    T4=XT[chron_pwid_nep_ost,time_cut:time_end]
    boty = XT[,time_cut:time_end]
    if (typeof==2){
        y=(colSums(T1) + colSums(T2) + colSums(T3) + colSums(T3))
      }else{
        y=100*(colSums(T1) + colSums(T2) + colSums(T3) + colSums(T3))/colSums(boty)
    }
  } else{
    if (typeof==2){
        y=colSums(XT[c(L$chronic00,L$chronic01),time_cut:time_end])
    }else{
        y=100*colSums(XT[c(L$chronic00,L$chronic01),time_cut:time_end])/colSums(XT[,time_cut:time_end])
    }
  }
  times = time_cut:time_end
  df=data.frame(times,y)
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=df, aes(x=timein, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Percentage") + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = timein,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))

  return(p1)  
}

plot_Rt_PWID_prison_gp=function(XT_gp,XT_pwid,XT_prison,timein,numplot,typeof,scale00,scale10,scale00_prison,scale10_prison){
  L=coeffsof_v5()
  time_cut=min(timein)-2015+1
  time_end=max(timein)-2015+1
  scenario_number=122
  dum=readRDS(paste0("www/scenario_",scenario_number,"_param_PWID.rds"))
  param_vals_base=dum[1:916]
  curr_mort_pwid=param_vals_base[895:903]
  curr_mort_former=param_vals_base[904:912]
  # the same for all populations at present 22 August 2021
  rA_F0= -param_vals_base[106]
  delta=param_vals_base[6]/rA_F0
  
  
  if (numplot==1){
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_gp,1,1)
    XTnewchronic=L$XTnewchronic
    titly="Instantaneous R0 estimate gen pop"
  }else if(numplot==2){
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_pwid,scale10,scale00) 
    XTnewchronic=L$XTnewchronic
    titly="Instantaneous R0 estimate PWID"
  }else if(numplot==3){
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_prison,scale10_prison,scale00_prison) 
    titly="Instantaneous R0 estimate prison"
    XTnewchronic=L$XTnewchronic
  }else if(numplot==4){
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_gp,1,1)
    XTnewchronic=L$XTnewchronic
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_pwid,scale10,scale00) 
    XTnewchronic=XTnewchronic+L$XTnewchronic
    titly="Instantaneous R0 estimate gen pop & PWID"
  }else if(numplot==5){
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_prison,scale10_prison,scale00_prison) 
    XTnewchronic=L$XTnewchronic
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_pwid,scale10,scale00) 
    XTnewchronic=XTnewchronic+L$XTnewchronic
    titly="Instantaneous R0 estimate prison & PWID"
  }else if(numplot==6){
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_gp,1,1)
    XTnewchronic=L$XTnewchronic
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_prison,scale10_prison,scale00_prison) 
    XTnewchronic=XTnewchronic+L$XTnewchronic
    titly="Instantaneous R0 estimate gen pop & prison"
  }else if(numplot==7){
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_gp,1,1)
    XTnewchronic=L$XTnewchronic
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_pwid,scale10,scale00) 
    XTnewchronic=XTnewchronic+L$XTnewchronic
    L=calc_chronic_cases_for_plot(2015:2040,curr_mort_former,curr_mort_pwid,delta,rA_F0,XT_prison,scale10_prison,scale00_prison) 
    XTnewchronic=XTnewchronic+L$XTnewchronic
    titly="Instantaneous R0 estimate gen pop, PWID & prison"
  }
  laby="Rate"
  X=2015:2040
  I = XTnewchronic
  ntsart=8
  
 
  meansi=7
  stdsi=2
  Rt=rep(0,(length(I)-ntsart+1))
  Rx=Rt
  for ( i in ntsart:length(I)){
    boty = 0
    for ( j in 1 :(i-1)){
      boty = boty +I[i-j]*dgamma(j,4,1)#dnorm(j,mean=meansi,sd=stdsi)
    }
    Rt[i-ntsart+1] = I[i]/boty
    Rx[i-ntsart+1] = X[i]
  }
  #plot(Rx,Rt,type="p",pch=19)
  fity=turbotrend(Rx,Rt) # taken off TurboNorm::
  #lines(fity$xtrend,fity$ytrend)
  
  times = time_cut:time_end # note this assumes 2022 to 2040
  df=data.frame(x=Rx,y=Rt)
  df2=data.frame(fityx=fity$xtrend,fityy=fity$ytrend)
  if (numplot==1){
    df=data.frame(x=Rx,y=rep(0.1,(length(I)-ntsart+1))) 
    df2=data.frame(fityx=Rx,fityy=rep(0.1,(length(I)-ntsart+1)))
  }
  
  miny= min(pretty_breaks()(c(min(df$y), df$y)))
  maxy=max(pretty_breaks()(c(0, df$y)))
  miny=min(pretty_breaks()(c(min(df$y),df$y)))
  
  vecy=pretty_breaks()(c(miny, maxy))
  p1=ggplot() + geom_line(data=df2, aes(x=fityx, y=fityy),color="red",size=2) +
    geom_point(data=df, aes(x=x, y=y),color="black",size=1)+
    xlab("Year") + ylab("Rt") + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(miny,maxy),breaks=vecy)+
    scale_x_continuous(breaks = timein,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))
  #saveRDS(df,"~/HCV_vV/R0/testrt.rds")
  return(p1)  
}

rn_of_three_plots=function(pg_genpop,pg_pwid,pg_prison){
  # pg=ggplot_build() output
  z=c(pg_genpop$data[[1]][,1],pg_pwid$data[[1]][,1],pg_prison$data[[1]][,1])
  miny=min(pretty_breaks()(c(0,z)))
  maxy=max(pretty_breaks()(c(0,z)))
  return(c(miny,maxy))
}



plot_attack_rate_v_treat_all_PWID_prison_gp_v2=function(numplot,t1,L,Xbase_gp,Xbase_pwid,Xbase_pris,
                                                     Xgenpop,Xpwid,Xprison,
                                                     scale00_pwid,scale10_pwid,scale00_prison,scale10_prison,
                                                     pcom,alpha,time_cut,L_PWID,L_PRIS,L_GP){
  
  DAAtreat_pwid = L_PWID$DAAtreat
  SCreening_pwid = L_PWID$SCreening
  NSPOST_pwid = L_PWID$NSPOST
  DAAtreat_pris = L_PRIS$DAAtreat
  SCreening_pris = L_PRIS$SCreening
  NSPOST_pris = L_PRIS$NSPOST 
  DAAtreat_gp = L_GP$DAAtreat
  SCreening_gp = L_GP$SCreening
  DAAtreat_gp = L_GP$DAAtreat
  SCreening_gp = L_GP$SCreening

  timecut=which(t1==time_cut)

  if (numplot==1){
    titly="Cumulative with chronic HCV averted and treats for gen pop"
    X=Xbase_gp
    XT=Xgenpop
    scale00=1
    scale10=1
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test_gp(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
  }else if(numplot==2){
    titly="Cumulative with chronic HCV averted and treats for PWID"
    X=Xbase_pwid
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
  }else if(numplot==3){
    titly="Cumulative with chronic HCV averted and treats for prison"
    X=Xbase_pris
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
  }else if(numplot==4){
    titly="Cumulative with chronic HCV averted and treats for gen pop & PWID"
    X=Xbase_gp
    XT=Xgenpop
    scale00=1
    scale10=1
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test_gp(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
    X=Xbase_pwid
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    y=y + get_y(X,XT,L,scale10,scale00)  
    ydum=ydum+get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
  }else if(numplot==5){
    titly="Cumulative with chronic HCV averted and treats for prison & PWID"
    X=Xbase_pris
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
    X=Xbase_pwid
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    y=y + get_y(X,XT,L,scale10,scale00)  
    ydum=ydum + get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
  }else if(numplot==6){
    titly="Cumulative with chronic HCV averted and treats for gen pop & prison"
    X=Xbase_gp
    XT=Xgenpop
    scale00=1
    scale10=1
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test_gp(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
    X=Xbase_pris
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    y=y+get_y(X,XT,L,scale10,scale00)
    ydum=ydum+get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
  }else if(numplot==7){
    titly="Cumulative with chronic HCV averted and treats for gen pop, PWID & prison"
    X=Xbase_gp
    XT=Xgenpop
    scale00=1
    scale10=1
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test_gp(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
    X=Xbase_pwid
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    y=y + get_y(X,XT,L,scale10,scale00)  
    ydum=ydum + get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
    X=Xbase_pris
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    y=y+get_y(X,XT,L,scale10,scale00)
    ydum=ydum + get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
  }

  

  tend=which(t1==rev(t1)[1])
  tdum=t1[timecut:tend]
  ydum=pmax(ydum,0.0001) # guard against no treats
  # two scales in one diag (not always recommended, but I WANT that now)
  # some reading up: https://stackoverflow.com/questions/3099219/plot-with-2-y-axes-one-y-axis-on-the-left-and-another-y-axis-on-the-right/3101876#
  
  C=cumsum(c(y[timecut:tend]))
  TT=cumsum(c(ydum[timecut:tend]))
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
    scale_x_continuous(name="Year",breaks = tdum,expand=c(0,0))+
    scale_y_continuous(name = "Cumulative chronic cases averted", labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                       sec.axis = sec_axis(~./prod2, name = "Cumulative treatments",labels=function(x) format(x, big.mark = ",", scientific = FALSE))) +
    ggtitle(titly)+
    theme(
      axis.title.y = element_text(color = "red"),
      axis.text.y= element_text(color = "red"),
      axis.title.y.right = element_text(color = "blue"),
      axis.text.y.right=element_text(color="blue"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}
get_tot_test=function(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat,SCreening,NSPOST){
enroll_nep=NSPOST$NSPcov
enroll_ost=NSPOST$OSTcov
enroll_nep_ost=NSPOST$NSPOSTcov
totaltestsF4=rep(0,length(t1))
totaltests_notF4=rep(0,length(t1))
for (i in 1 : length(t1)){
  N=XT[,i]
  dum=treat_comps_pv7_PWID(N,scale00,scale10,t1[i],pcom,alpha,time_cut,
                           DAAtreat,SCreening,
                           enroll_nep,enroll_ost,enroll_nep_ost)
  totaltestsF4[i]=sum(dum$total_T4)
  totaltests_notF4[i]=sum(dum$total_not_T4)
}
return(totaltestsF4+totaltests_notF4)
}

get_tot_test_gp=function(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat,SCreening,enroll_nep,enroll_ost,enroll_nep_ost){
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  
  for (i in 1 : length(t1)){
    N=XT[,i]
   dum=treat_comps_pv7_gen_pop(N,t1[i],pcom,alpha,time_cut,
                                     DAAtreat$treat_cov_mild,DAAtreat$treat_cov_severe,
                                     SCreening$screening_cov,SCreening$screening_diagnosis_duration,
                                     SCreening$ignore_screening)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  return(totaltestsF4+totaltests_notF4)
}

get_y=function(X,XT,L,scale10,scale00){
  if (dim(XT)[1]==1800){
    a=colSums(X[L$chronic00,])+colSums(X[L$chronic10,])
    b=colSums(XT[L$chronic00,])+colSums(XT[L$chronic10,])+
      colSums(XT[360+L$chronic10,])+colSums(XT[720+L$chronic10,])+colSums(XT[1080+L$chronic10,])
    jequalzero=abs(b-a)/a<0.001
    # actual numbers
    ya=scale10*colSums(X[L$chronic10,])+scale00*colSums(X[L$chronic00,])
    yb=scale10*colSums(XT[L$chronic10,])+scale00*colSums(XT[L$chronic00,])+
      scale10*(colSums(XT[360+L$chronic10,])+colSums(XT[720+L$chronic10,])+colSums(XT[1080+L$chronic10,]))
    y=ya-yb
    y[jequalzero]=0
  }else{
    # gp so only 360 compartments
    # 1 to 180 are no screening = L$chronic00
    # 181 to 360 are screened = L$chronic01  
    a=colSums(X[L$chronic00,])+colSums(X[L$chronic01,])
    b=colSums(XT[L$chronic00,])+colSums(XT[L$chronic01,])
    jequalzero=abs(b-a)/a<0.001
    # actual numbers - same as no scale for genpop
    ya=colSums(X[L$chronic00,])+colSums(X[L$chronic01,])
    yb=colSums(XT[L$chronic00,])+colSums(XT[L$chronic01,])
    y=ya-yb
    y[jequalzero]=0
  }
  return(y)
}

plot_output_treats_v2_all_PWID_prison_gp=function(numplot,t1,L,Xgenpop,Xpwid,Xprison,
                                                  scale00_pwid,scale10_pwid,scale00_prison,scale10_prison,
                                                  pcom,alpha,time_cut,L_PWID,L_PRIS,L_GP){
  # XT is the output from the compartment model
  # t1 is the required times
  # This is for all intervention model
  DAAtreat_pwid = L_PWID$DAAtreat
  SCreening_pwid = L_PWID$SCreening
  NSPOST_pwid = L_PWID$NSPOST
  DAAtreat_pris = L_PRIS$DAAtreat
  SCreening_pris = L_PRIS$SCreening
  NSPOST_pris = L_PRIS$NSPOST 
  DAAtreat_gp = L_GP$DAAtreat
  SCreening_gp = L_GP$SCreening
  DAAtreat_gp = L_GP$DAAtreat
  SCreening_gp = L_GP$SCreening
  
  timecut=which(t1==time_cut)
  if (numplot==1){
    titly="per 1,000 susceptible and HCV gen pop"
    XT=Xgenpop
    scale00=1
    scale10=1
    dum=get_tot_test_gp_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
    ydum=dum$Y
    totP=dum$X    
  }else if(numplot==2){
    titly="per 1,000 PWID (pwid pop)"
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    dum=get_tot_test_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
    ydum=dum$Y
    totP=dum$X
  } else if(numplot==3){
    titly="per 1,000 PWID (prison)"
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    dum=get_tot_test_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
    ydum=dum$Y
    totP=dum$X
  }else if(numplot==4){
    titly="per 1,000 susceptible and HCV gen pop & PWID (pwid pop)"
    XT=Xgenpop
    scale00=1
    scale10=1
    dum=get_tot_test_gp_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
    ydum=dum$Y
    totP=dum$X
    
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    dum2=get_tot_test_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
    ydum = ydum + dum2$Y
    totP = totP + dum2$X
  }else if(numplot==5){
    titly="per 1,000 PWID (pwid and prison pop)"
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    dum=get_tot_test_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
    ydum=dum$Y
    totP=dum$X 
    
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    dum2=get_tot_test_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
    ydum = ydum + dum2$Y
    totP = totP + dum2$X
  }else if(numplot==6){
    titly="per 1,000 susceptible and HCV gen pop & PWID (prison)"
    XT=Xgenpop
    scale00=1
    scale10=1
    dum=get_tot_test_gp_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
    ydum=dum$Y
    totP=dum$X
    
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    dum2=get_tot_test_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
    ydum = ydum + dum2$Y
    totP = totP + dum2$X
    
  }else if(numplot==7){
    titly="per 1,000 susceptible and HCV gen pop & PWID (pwid and prison pop)"
    XT=Xgenpop
    scale00=1
    scale10=1
    dum=get_tot_test_gp_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
    ydum=dum$Y
    totP=dum$X
    
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    dum2=get_tot_test_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)

    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    dum3=get_tot_test_tot_treats(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
    ydum = ydum + dum2$Y + dum3$Y
    totP = totP + dum2$X + dum3$X
  }
  tend=which(t1==rev(t1)[1])
  tdum=t1[timecut:tend]
  avy = round(1000*sum(ydum[timecut:tend])/sum(totP[timecut:tend]))
  ydum = ydum[timecut:tend]
  df= data.frame(x=tdum,y=ydum)
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of DAA") + ggtitle(paste0("Total number of DAA treatments - average ",avy," ",titly))+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = tdum,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

get_tot_test_tot_treats=function(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat,SCreening,NSPOST){
  enroll_nep=NSPOST$NSPcov
  enroll_ost=NSPOST$OSTcov
  enroll_nep_ost=NSPOST$NSPOSTcov
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  totP=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    totP[i]= scale10*(sum(N[361:720])+sum(N[721:1080])+sum(N[1081:1440])+sum(N[1441:1800]))
    dum=treat_comps_pv7_PWID(N,scale00,scale10,t1[i],pcom,alpha,time_cut,
                             DAAtreat,SCreening,
                             enroll_nep,enroll_ost,enroll_nep_ost)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  return(list(X=totP,Y=totaltestsF4+totaltests_notF4))
}

get_tot_test_gp_tot_treats=function(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat,SCreening,enroll_nep,enroll_ost,enroll_nep_ost){
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  totP=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    totP=sum(N)
    dum=treat_comps_pv7_gen_pop(N,t1[i],pcom,alpha,time_cut,
                                DAAtreat$treat_cov_mild,DAAtreat$treat_cov_severe,
                                SCreening$screening_cov,SCreening$screening_diagnosis_duration,
                                SCreening$ignore_screening)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  return(list(X=totP,Y=totaltestsF4+totaltests_notF4))
}
# t1,L,X,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat,SCreening,
# enroll_nep,enroll_ost,enroll_nep_ost
plot_output_cases_averted_all_PWID_prison_gp=function(numplot,t1,L,Xbase_gp,Xbase_pwid,Xbase_pris,Xgenpop,Xpwid,Xprison,
                                                      scale00_pwid,scale10_pwid,scale00_prison,scale10_prison,
                                                      pcom,alpha,time_cut,L_PWID,L_PRIS,L_GP){
  
  DAAtreat_pwid = L_PWID$DAAtreat
  SCreening_pwid = L_PWID$SCreening
  NSPOST_pwid = L_PWID$NSPOST
  DAAtreat_pris = L_PRIS$DAAtreat
  SCreening_pris = L_PRIS$SCreening
  NSPOST_pris = L_PRIS$NSPOST 
  DAAtreat_gp = L_GP$DAAtreat
  SCreening_gp = L_GP$SCreening
  DAAtreat_gp = L_GP$DAAtreat
  SCreening_gp = L_GP$SCreening
  
  timecut=which(t1==time_cut)
  tend=which(t1==rev(t1)[1])
  tdum=t1[timecut:tend]
  if (numplot==1){
    titly="Cumulative number of chronic cases averted per DAA treatment for gen pop"
    X=Xbase_gp
    XT=Xgenpop
    scale00=1
    scale10=1
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test_gp(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
  }else if(numplot==2){
    titly="Cumulative number of chronic cases averted per DAA treatment for PWID"
    X=Xbase_pwid
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
  }else if(numplot==3){
    titly="Cumulative number of chronic cases averted per DAA treatment for prison"
    X=Xbase_pris
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
  }else if(numplot==4){
    titly="Cumulative number of chronic cases averted per DAA treatment for gen pop & PWID"
    X=Xbase_gp
    XT=Xgenpop
    scale00=1
    scale10=1
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test_gp(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
    X=Xbase_pwid
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    y=y + get_y(X,XT,L,scale10,scale00)  
    ydum=ydum+get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
  }else if(numplot==5){
    titly="Cumulative number of chronic cases averted per DAA treatment for prison & PWID"
    X=Xbase_pris
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
    X=Xbase_pwid
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    y=y + get_y(X,XT,L,scale10,scale00)  
    ydum=ydum + get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
  }else if(numplot==6){
    titly="Cumulative number of chronic cases averted per DAA treatment for gen pop & prison"
    X=Xbase_gp
    XT=Xgenpop
    scale00=1
    scale10=1
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test_gp(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
    X=Xbase_pris
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    y=y+get_y(X,XT,L,scale10,scale00)
    ydum=ydum+get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
  }else if(numplot==7){
    titly="Cumulative number of chronic cases averted per DAA treatment for gen pop, PWID & prison"
    X=Xbase_gp
    XT=Xgenpop
    scale00=1
    scale10=1
    y=get_y(X,XT,L,scale10,scale00)
    ydum=get_tot_test_gp(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_gp,SCreening_gp)
    X=Xbase_pwid
    XT=Xpwid
    scale00=scale00_pwid
    scale10=scale10_pwid
    y=y + get_y(X,XT,L,scale10,scale00)  
    ydum=ydum + get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pwid,SCreening_pwid,NSPOST_pwid)
    X=Xbase_pris
    XT=Xprison
    scale00=scale00_prison
    scale10=scale10_prison
    y=y+get_y(X,XT,L,scale10,scale00)
    ydum=ydum + get_tot_test(numplot,t1,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat_pris,SCreening_pris,NSPOST_pris)
  }
  ydum=pmax(ydum,0.0001) # guard against no treats
  ydum=ydum[timecut:tend]
  topy=y[timecut:tend]
  topy[topy<0.1]=0
  if (sum(ydum)<0.1){
    topy=rep(0,length(topy))
  }
  d=cumsum(topy)/cumsum(ydum)
  #print(paste0("The final is ",rev(d)[1]))
  df= data.frame(x=tdum,y=d)
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("chronic cases averted") + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(tdum[1],2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_nep_ost_v3_all_count_PWID_prison_gp=function(numplot,t1,Xgenpop,Xpwid,Xprison,scale00_pwid,scale10_pwid,scale00_prison,scale10_prison,time_cut){
  # XT is the output from the compartment model
  # t1 is the required times
  # This is for all intervention model
  
  timecut=which(t1==time_cut)
  tend=which(t1==rev(t1)[1])
  tdum=t1[timecut:tend]
  rngy=timecut:tend
  if (numplot==1){
    titly="Chronic subjects enrolled in screening for gen pop"
    XT=Xgenpop
    y=get_prev_tots_gp(XT,rngy)
    p=plot_one(data.frame(t1=tdum,y0=y),titly)
  }else if(numplot==2){
    titly="Chronic subjects enrolled in prevention for PWID"
    XT=Xpwid
    y=get_prev_tots(scale10_pwid,XT,rngy)
    p=plot_three(data.frame(t1=tdum,y1=y[[1]],y2=y[[2]],y3=y[[3]]),titly)
  }else if(numplot==3){
    titly="Chronic subjects enrolled in prevention for prison"
    XT=Xprison
    y=get_prev_tots(scale10_prison,XT,rngy)
    p=plot_three(data.frame(t1=tdum,y1=y[[1]],y2=y[[2]],y3=y[[3]]),titly)    
  }else if(numplot==4){
    titly="Chronic subjects enrolled in screening/prevention for gen pop & PWID"
    XT=Xgenpop
    ygp=get_prev_tots_gp(XT,rngy)
    XT=Xpwid
    y=c(get_prev_tots(scale10_pwid,XT,rngy))
    p=plot_four(data.frame(t1=tdum,y0=ygp,y1=y[[1]],y2=y[[2]],y3=y[[3]]),titly)    
  }else if(numplot==5){
    titly="Chronic subjects enrolled in prevention for prison & PWID"
    XT=Xprison
    y_prison=get_prev_tots(scale10_prison,XT,rngy)
    XT=Xpwid
    y_pwid=get_prev_tots(scale10_pwid,XT,rngy)
    y=addto(y_pwid,y_prison)
    p=plot_three(data.frame(t1=tdum,y1=y[[1]],y2=y[[2]],y3=y[[3]]),titly)  
  }else if(numplot==6){
    titly="Chronic subjects enrolled in screening/prevention for gen pop & prison"
    XT=Xgenpop
    ygp=get_prev_tots_gp(XT,rngy)
    XT=Xprison
    y=c(get_prev_tots(scale10_prison,XT,rngy))
    p=plot_four(data.frame(t1=tdum,y0=ygp,y1=y[[1]],y2=y[[2]],y3=y[[3]]),titly)  
  }else if(numplot==7){
    titly="Chronic subjects enrolled in screening/prevention for gen pop, PWID & prison"
    XT=Xpwid
    y_pwid=get_prev_tots(scale10_pwid,XT,rngy)
    XT=Xprison
    y_prison=get_prev_tots(scale10_prison,XT,rngy)
    y=addto(y_pwid,y_prison)
    XT=Xgenpop
    ygp=get_prev_tots_gp(XT,rngy)
    p=plot_four(data.frame(t1=tdum,y0=ygp,y1=y[[1]],y2=y[[2]],y3=y[[3]]),titly)    
  }
return(p)
}
addto=function(A,B){
  y=data.frame(y1=A[[1]]+B[[1]],y2=A[[2]]+B[[2]],y3=A[[3]]+B[[3]])
  return(y)
}

get_prev_tots=function(scale10,XT,rngy){
  # note X cols start on col before timecut
  # These are used for PWID and prison pop
  F0_current_nep = seq(367,540,20) + 360
  F1_current_nep = seq(368,540,20) + 360
  F2_current_nep = seq(369,540,20) + 360 
  F3_current_nep = seq(370,540,20) + 360
  F4_current_nep = seq(371,540,20) + 360
  HC_current_nep = seq(372,540,20) + 360 
  DC_current_nep = seq(373,540,20) + 360
  L1_current_nep = seq(374,540,20) + 360
  L2_current_nep = seq(375,540,20) + 360
  nep=c(F0_current_nep,F1_current_nep,F2_current_nep,F3_current_nep,F4_current_nep,HC_current_nep,DC_current_nep,L1_current_nep,L2_current_nep)
  
  F0_current_ost = seq(367,540,20) + 720
  F1_current_ost = seq(368,540,20) + 720
  F2_current_ost = seq(369,540,20) + 720 
  F3_current_ost = seq(370,540,20) + 720
  F4_current_ost = seq(371,540,20) + 720
  HC_current_ost = seq(372,540,20) + 720 
  DC_current_ost = seq(373,540,20) + 720
  L1_current_ost = seq(374,540,20) + 720
  L2_current_ost = seq(375,540,20) + 720
  ost=c(F0_current_ost,F1_current_ost,F2_current_ost,F3_current_ost,F4_current_ost,HC_current_ost,DC_current_ost,L1_current_ost,L2_current_ost)
  
  F0_current_nep_ost = seq(367,540,20) + 1080
  F1_current_nep_ost = seq(368,540,20) + 1080
  F2_current_nep_ost = seq(369,540,20) + 1080 
  F3_current_nep_ost = seq(370,540,20) + 1080
  F4_current_nep_ost = seq(371,540,20) + 1080
  HC_current_nep_ost = seq(372,540,20) + 1080 
  DC_current_nep_ost = seq(373,540,20) + 1080
  L1_current_nep_ost = seq(374,540,20) + 1080
  L2_current_nep_ost = seq(375,540,20) + 1080
  nep_ost=c(F0_current_nep_ost,F1_current_nep_ost,F2_current_nep_ost,F3_current_nep_ost,F4_current_nep_ost,HC_current_nep_ost,DC_current_nep_ost,L1_current_nep_ost,L2_current_nep_ost)
  
  # *** note need all the subjects that have been moved not just F0-F4
  # for nep = 721:1080, for OST 1081:1440, for both 1441:1800
  total_nep = scale10*c(0,diff(colSums(XT[nep,])))
  total_ost = scale10*c(0,diff(colSums(XT[ost,])))
  total_nep_ost = scale10*colSums(XT[nep_ost,])
  y1=total_nep[rngy]
  y2=total_ost[rngy]
  y3=total_nep_ost[rngy]
  return(list(y1=y1,y2=y2,y3=y3))
}

get_prev_tots_gp=function(XT,rngy){
  # note X cols start on col before timecut
  # These are used for the gp screened
  F0_screened = seq(367,540,20) - 360 + 180
  F1_screened = seq(368,540,20) - 360 + 180
  F2_screened = seq(369,540,20) - 360 + 180 
  F3_screened = seq(370,540,20) - 360 + 180
  F4_screened = seq(371,540,20) - 360 + 180
  HC_screened = seq(372,540,20) - 360 + 180 
  DC_screened = seq(373,540,20) - 360 + 180
  L1_screened = seq(374,540,20) - 360 + 180
  L2_screened = seq(375,540,20) - 360 + 180
  screened=c(F0_screened,F1_screened,F2_screened,F3_screened,F4_screened,HC_screened,DC_screened,L1_screened,L2_screened)
  total_screened = colSums(XT[screened,])
  
  return(total_screened[rngy])
}

plot_four=function(df,titly){
t1=df$t1
p=ggplot(data=df, aes(x=t1)) + geom_line(aes(y=y0,colour="screen"),size=2) +
  geom_line(aes(y=y1,colour="nep"),size=2)+
  geom_line(aes(y=y2,colour="ost"),size=2)+ geom_line(aes(y=y3,colour="nep-ost"),size=2)+
  scale_colour_manual("", 
                      breaks = c("screen","nep", "ost","nep-ost"),
                      values = c("cyan","blue", "green","red")) +
  xlab("Year") + ylab("Number of HCV chronic") + ggtitle(titly)+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
  scale_x_continuous(breaks = t1,expand=c(0,0))+
  theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
return(p)  
}

plot_three=function(df,titly){
  t1=df$t1
  p=ggplot(data=df, aes(x=t1)) + geom_line(aes(y=y1,colour="nep"),size=2)+
    geom_line(aes(y=y2,colour="ost"),size=2)+ geom_line(aes(y=y3,colour="nep-ost"),size=2)+
    scale_colour_manual("", 
                        breaks = c("nep", "ost","nep-ost"),
                        values = c("blue", "green","red")) +
    xlab("Year") + ylab("Number of HCV chronic") + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = t1,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p)  
}

plot_one=function(df,titly){
  t1=df$t1
  p=ggplot(data=df, aes(x=t1)) + geom_line(aes(y=y0),colour="cyan",size=2) +
    # scale_colour_manual("", 
    #                     breaks = c("screened"),
    #                     values = c("cyan")) +
    xlab("Year") + ylab("Number of HCV chronic") + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = t1,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p)  
}


 plot_output_icer_PWID_prison_gp=function(numplot,
                                          Lgenpop,Lpwid,Lprison,
                                          Lgenpop_T,Lpwid_T,Lprison_T,df){
#   Lgenpop,Lpwid,Lprison baseline COST,QALYS and stage_vals
#   After treats   

   if (numplot==1){
    
     v=c(df$fixed_coverage_three_GP)
     g=c("gen pop")
     dum=title_footnote(v,g)
     trtmessage_1<<-dum$trtmessage
     footnote_1<<-dum$footnote
     p=output_ICER_text_v2_three (Lgenpop$COST,Lgenpop$QALY,Lgenpop_T$COST,Lgenpop_T$QALY,
                                 Lgenpop$stage_vals,Lgenpop_T$stage_vals,v,g)
    }else if(numplot==2){
     v=c(df$fixed_coverage_three_PWID)
     g=c("pwid")
     dum=title_footnote(v,g)
     trtmessage_2<<-dum$trtmessage
     footnote_2<<-dum$footnote
     p=output_ICER_text_v2_three (Lpwid$COST,Lpwid$QALY,Lpwid_T$COST,Lpwid_T$QALY,
                                Lpwid$stage_vals,Lpwid_T$stage_vals,v,g)
   }else if(numplot==3){
     v=c(df$fixed_coverage_three_PRIS)
     g=c("prison")
     dum=title_footnote(v,g)
     trtmessage_3<<-dum$trtmessage
     footnote_3<<-dum$footnote
     p=output_ICER_text_v2_three (Lprison$COST,Lprison$QALY,Lprison_T$COST,Lprison_T$QALY,
                                Lprison$stage_vals,Lprison_T$stage_vals,v,g)
   }else if(numplot==4){
     v=c(df$fixed_coverage_three_GP,df$fixed_coverage_three_PWID)
     g=c("gen pop","pwid")
     dum=title_footnote(v,g)
     trtmessage_4<<-dum$trtmessage
     footnote_4<<-dum$footnote
     p=output_ICER_text_v2_three (Lgenpop$COST+Lpwid$COST,Lgenpop$QALY+Lpwid$QALY,
                                Lgenpop_T$COST+Lpwid_T$COST,Lgenpop_T$QALY+Lpwid_T$QALY,
                                cbind.data.frame(STAGE=Lpwid$stage_vals[,1],Lpwid$stage_vals[,2:3]+Lgenpop$stage_vals[,2:3],stringsAsFactors = FALSE),
                                cbind.data.frame(STAGE=Lpwid_T$stage_vals[,1],Lpwid_T$stage_vals[,2:3]+Lgenpop_T$stage_vals[,2:3],stringsAsFactors = FALSE),
                                v,g)
   }else if(numplot==5){
     v=c(df$fixed_coverage_three_PRIS,df$fixed_coverage_three_PWID)
     g=c("prison","pwid")
     dum=title_footnote(v,g)
     trtmessage_5<<-dum$trtmessage
     footnote_5<<-dum$footnote
     p=output_ICER_text_v2_three (Lprison$COST+Lpwid$COST,Lprison$QALY+Lpwid$QALY,
                                Lprison_T$COST+Lpwid_T$COST,Lprison_T$QALY+Lpwid_T$QALY,
                                cbind.data.frame(STAGE=Lpwid$stage_vals[,1],Lpwid$stage_vals[,2:3]+Lprison$stage_vals[,2:3],stringsAsFactors = FALSE),
                                cbind.data.frame(STAGE=Lpwid_T$stage_vals[,1],Lpwid_T$stage_vals[,2:3]+Lprison_T$stage_vals[,2:3],stringsAsFactors = FALSE),
                                v,g)
   }else if(numplot==6){
     v=c(df$fixed_coverage_three_GP,df$fixed_coverage_three_PRIS)
     g=c("genpop","prison")
     dum=title_footnote(v,g)
     trtmessage_6<<-dum$trtmessage
     footnote_6<<-dum$footnote
     p=output_ICER_text_v2_three (Lprison$COST+Lgenpop$COST,Lgenpop$QALY+Lprison$QALY,
                                Lprison_T$COST+Lgenpop_T$COST,Lgenpop_T$QALY+Lprison_T$QALY,
                                cbind.data.frame(STAGE=Lprison$stage_vals[,1],Lprison$stage_vals[,2:3]+Lgenpop$stage_vals[,2:3],stringsAsFactors = FALSE),
                                cbind.data.frame(STAGE=Lprison_T$stage_vals[,1],Lprison_T$stage_vals[,2:3]+Lgenpop$stage_vals[,2:3],stringsAsFactors = FALSE),
                                v,g)
  }else if(numplot==7){
    v=c(df$fixed_coverage_three_GP,df$fixed_coverage_three_PWID,df$fixed_coverage_three_PRIS)
    g=c("gen pop","pwid","prison")
    dum=title_footnote(v,g)
    trtmessage_7<<-dum$trtmessage
    footnote_7<<-dum$footnote
    p=output_ICER_text_v2_three (Lgenpop$COST+Lpwid$COST+Lprison$COST,Lgenpop$QALY+Lpwid$QALY+Lprison$QALY,
                               Lgenpop_T$COST+Lpwid_T$COST+Lprison_T$COST,Lgenpop_T$QALY+Lpwid_T$QALY+Lprison_T$QALY,
                               cbind.data.frame(STAGE=Lprison$stage_vals[,1],Lprison$stage_vals[,2:3]+Lgenpop$stage_vals[,2:3]+Lpwid$stage_vals[,2:3],stringsAsFactors = FALSE),
                               cbind.data.frame(STAGE=Lprison_T$stage_vals[,1],Lprison_T$stage_vals[,2:3]+Lgenpop_T$stage_vals[,2:3]+Lpwid_T$stage_vals[,2:3],stringsAsFactors = FALSE),
                               v,g)
  }

   return(p)  

 }

# icer calc
output_ICER_text_v2_three = function(base_cost,base_qaly,trt_cost,trt_qaly,
                               base_stage_vals,trt_stage_vals,v,g){
  library(flextable)
  library(purrr)
  library(DT)
  library(jsonlite)
  
  ICER = (sum(trt_cost) - sum(base_cost))/(sum(trt_qaly)-sum(base_qaly))
  captiony="$ per QALY gained\n"
  # dum=title_footnote(v,g)
  # trtmessage=dum$trtmessage
  # footnote=dum$footnote
  # if (daa_only==0){
  #   trtmessage = "DAA treatment"
  #   footnote=""
  # }else{
  #   trtmessage = "DAA treatment with prevention"
  #   footnote="Prevention costs not included"
  # }
  costs = c(sum(base_cost),sum(trt_cost))
  qalys = c(sum(base_qaly),sum(trt_qaly))
  icers = c(NA,round(ICER))
  df = data.frame(costs,qalys,icers)
  df$costs <- format(round(df$costs), big.mark=',', scientific=FALSE) 
  df$qalys <- format(round(df$qalys), big.mark=',', scientific=FALSE) 
  df$icers <- format(round(df$icers), big.mark=',', scientific=FALSE) 
  
  base_stage_vals$COST <- format(round(base_stage_vals$COST), big.mark=',', scientific=FALSE) 
  base_stage_vals$QALYs <- format(round(base_stage_vals$QALYs), big.mark=',', scientific=FALSE) 
  
  trt_stage_vals$COST <- format(round(trt_stage_vals$COST), big.mark=',', scientific=FALSE) 
  trt_stage_vals$QALYs <- format(round(trt_stage_vals$QALYs), big.mark=',', scientific=FALSE) 
  df[] <- lapply(df, as.character)
  df$icers[(gsub(" ", "", df$icers, fixed = TRUE))=="NA"]=""
  df$icers[(gsub(" ", "", df$icers, fixed = TRUE))=="NaN"]="-"
  df$icers[(gsub(" ", "", df$icers, fixed = TRUE))=="Inf"]="-"
  df$icers[(gsub(" ", "", df$icers, fixed = TRUE))=="0"]="-"
  #print(df)
  #df[is.na(df)]="" 
  # is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
  # df[is.nan.data.frame(df)]=""
  # print(df)
  rownames(df)=c("baseline","intervention")
  names(df)=c("Total Cost($)", "Total Qalys", "ICER($)" )
  #df <- sapply(df, as.character) 
  
  subdats <- lapply(list(base_stage_vals, trt_stage_vals), purrr::transpose)
  Dat <- cbind(" " = "expand", df, details = I(subdats))
  # set_flextable_defaults(theme_fun = theme_vanilla,
  #                        padding.top = 12,
  #                        background.color = "#EFEFEF")
  # df <- format(df,big.mark = ",")
  # df$`Total Cost($)`=as.character(df$`Total Cost($)`)
  # df$`Total Qalys`=as.character(df$`Total Qalys`)
  # df$`ICER($)`=as.character(df$`ICER($)`)
  # df[1,3]=""
  # if (df[2,3]=="NaN"){
  #   df[2,3]="-"
  # }
  # outab = flextable(df)
  # #set_flextable_defaults(big.mark = ",",padding.top = 12)
  # outab=flextable::colformat_num(x=outab, na_str="",big.mark = ",")
  # outab = set_caption(outab, caption = captiony)
  # outab = add_footer_lines(outab, footnote)
  # outab = color(outab,i=2,j=3, color = "red")
  # outab = bold(outab , i=2,j=3, bold = TRUE)
  # outab = align_text_col(outab , align = "right")
  # outab = width(outab, width = 3)
  # #set_table_properties(outab, width = 1, layout = "autofit")
  # return(outab)
  return(Dat)
}

title_footnote=function(v,g){
  # v are 0 or 1 daa or prev
  # g are the groups gen pop etc
  n = length(v)
  trtmessage ="DAA treatment for "
  trt_extra = ""
  footnote = ""
  ifoot=0
  for ( i in 1 : n){
    trt_extra = paste0(trt_extra,g[i],",")
      # if (v[i]==0){
      #   trt_extra = paste0(trt_extra,g[i],"(DAA only),")
      # }else{
      #   trt_extra = paste0(trt_extra,g[i],"(with prev),")
      #   ifoot=1
      # }
  }
  footnote="Prevention costs not included"
  # if (ifoot==1){
  #   footnote="Prevention costs not included"
  # }
  trtmessage=paste0(trtmessage,trt_extra)
  trtmessage=substr(trtmessage,1,nchar(trtmessage)-1)
  return(list(trtmessage=trtmessage,footnote=footnote))
}

