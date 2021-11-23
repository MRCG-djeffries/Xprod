testnewstartPWID_v4=function(discount_rate,ignore_flows=0,time_vals=0,plotout=0){
  # This tests a 2016 start for PWID model
  # Start is now 2015
  # get starting pops for prison and gen pop

  scenario_number=122
  s=runmodel_v4(scenario_number)
  
  # 2015 is col 66
  Xstartin2016_genpop=cbind(s$X[1:360,66],s$X[361:720,66]) #**** note actually prison start
  z8=run_PWID_with_pop(Xstartin2016_genpop,s,ignore_flows) # ,stages=p5,stages_prison=p5_prison
  Xgenpop=z8$X$X[1:360,]
  Xpwid=z8$X$X[361:2160,]
  Xprison=z8$X$X[2161:3960,]
  scale00_prison=z8$s0_prison
  scale10_prison=z8$s1_prison
  scale00_pwid=z8$s0_pwid
  scale10_pwid=z8$s1_pwid
  if (time_vals==0){
    timesoffigure=2016:2040
  }else{
    timesoffigure=2022:2040
  }
  L=coeffsof_v5()
  #align title to left theme(plot.title.position = 'plot')
  if (plotout==1){
    Xin=z8$X$X
    timein=timesoffigure
    tcols = timein - 2015 + 1
    nt=length(timein)
    #GEN POP
    Xgenpop=Xin[1:360,tcols]
    Cgenpop=getallpop_all_v2_pwid_for_prison(Xgenpop,L,rep(1,9),rep(1,9),nt,1,1)
    p5_gp=plot_areacomps_v3_pwid_for_prison(timein,Cgenpop,1)
    #PWID
    Xpwid=Xin[361:2160,tcols] # restrict the time for these output
    Cpwid=getallpop_all_v2_pwid_for_prison(Xpwid,L,rep(1,9),rep(1,9),nt,scale00_pwid,scale10_pwid)
    p5_pwid=plot_areacomps_v3_pwid_for_prison(timein,Cpwid,2)
    #PRISON
    Xprison=Xin[2161:3960,tcols] # restrict the time for these output
    Cprison=getallpop_all_v2_pwid_for_prison(Xprison,L,rep(1,9),rep(1,9),nt,scale00_prison,scale10_prison)
    p5_pi=plot_areacomps_v3_pwid_for_prison(timein,Cprison,3)
    return(list(stage_G=p5_gp,stage_P=p5_pwid,stage_J=p5_pi))
  # dum=global_newplots(z8$X$X,L,timesoffigure,scale00_pwid,scale10_pwid,scale00_prison,scale10_prison,ignore_flows)
  # q=list(G=dum$stages_gp,P=dum$stages_pwid,J=dum$stages_pi,GP=dum$stages_gp_pwid,JP=dum$stages_pi_pwid,GJ=dum$stages_gp_pi,GPJ=dum$stages_gp_pwid_pi,
  #        G1=dum$mort_gp,P1=dum$mort_pwid,J1=dum$mort_pi,GP1=dum$mort_gp_pwid,JP1=dum$mort_pi_pwid,GJ1=dum$mort_gp_pi,GPJ1=dum$mort_gp_pwid_pi,
  #        G2=dum$mort_gp_num,P2=dum$mort_pwid_num,J2=dum$mort_pi_num,GP2=dum$mort_gp_pwid_num,JP2=dum$mort_pi_pwid_num,
  #        GJ2=dum$mort_gp_pi_num,GPJ2=dum$mort_gp_pwid_pi_num,
  #        G3=dum$inci_gp,P3=dum$inci_pwid,J3=dum$inci_pi,GP3=dum$inci_gp_pwid,JP3=dum$inci_pi_pwid,GJ3=dum$inci_gp_pi,GPJ3=dum$inci_gp_pwid_pi,
  #        G4=dum$inci_gp_num,P4=dum$inci_pwid_num,J4=dum$inci_pi_num,GP4=dum$inci_gp_pwid_num,JP4=dum$inci_pi_pwid_num,
  #        GJ4=dum$inci_gp_pi_num,GPJ4=dum$inci_gp_pwid_pi_num,
  #        G5=dum$both_gp,P5=dum$both_pwid,J5=dum$both_pi,GP5=dum$both_gp_pwid,JP5=dum$both_pi_pwid,GJ5=dum$both_gp_pi,GPJ5=dum$both_gp_pwid_pi,
  #        G6=dum$pp_gp,P6=dum$pp_pwid,J6=dum$pp_pi,GP6=dum$pp_gp_pwid,JP6=dum$pp_pi_pwid,GJ6=dum$pp_gp_pi,GPJ6=dum$pp_gp_pwid_pi,
  #        G7=dum$cn_gp,P7=dum$cn_pwid,J7=dum$cn_pi,GP7=dum$cn_gp_pwid,JP7=dum$cn_pi_pwid,GJ7=dum$cn_gp_pi,GPJ7=dum$cn_gp_pwid_pi,
  #        G8=dum$rt_gp,P8=dum$rt_pwid,J8=dum$rt_pi,GP8=dum$rt_gp_pwid,JP8=dum$rt_pi_pwid,GJ8=dum$rt_gp_pi,GPJ8=dum$rt_gp_pwid_pi
  #        )
  # 
  # stage_pr=plot_grid(q$G,q$P+theme(legend.position = "none"),q$J+theme(legend.position = "none"),nrow=1,ncol=3)
  # mort_pr=plot_grid(q$G1+ ggtitle("Gen pop, liver mort(% change), ref:2015")+ theme(plot.title = element_text(size=9)),
  #                   q$P1+ ggtitle("PWID, liver mort(% change), ref:2015")+theme(plot.title = element_text(size=9)),
  #                   q$J1+ ggtitle("prison, liver mort(% change), ref:2015")+theme(plot.title = element_text(size=9)),nrow=1,ncol=3)
  # mort_pr_num=plot_grid(q$G2+ ggtitle("Gen pop, liver mort(# change), ref:2015")+ theme(plot.title = element_text(size=9)),
  #                   q$P2+ ggtitle("PWID, liver mort(# change), ref:2015")+theme(plot.title = element_text(size=9)),
  #                   q$J2+ ggtitle("prison, liver mort(# change), ref:2015")+theme(plot.title = element_text(size=9)),nrow=1,ncol=3)
  # inci_pr=plot_grid(q$G3+ ggtitle("Gen pop, new chronic(% change), ref:2015")+ theme(plot.title = element_text(size=9)),
  #                       q$P3+ ggtitle("PWID, new chronic(% change), ref:2015")+theme(plot.title = element_text(size=9)),
  #                       q$J3+ ggtitle("prison, new chronic(% change), ref:2015")+theme(plot.title = element_text(size=9)),nrow=1,ncol=3)
  # inci_pr_num=plot_grid(q$G4+ ggtitle("Gen pop, new chronic(# change), ref:2015")+ theme(plot.title = element_text(size=9)),
  #                   q$P4+ ggtitle("PWID, new chronic(# change), ref:2015")+theme(plot.title = element_text(size=9)),
  #                   q$J4+ ggtitle("prison, new chronic(# change), ref:2015")+theme(plot.title = element_text(size=9)),nrow=1,ncol=3)
  # both_pr=plot_grid(q$G5+scale_fill_manual("Gen pop ", values = c("HCV Incidence" = "blue", "HCV Deaths" = "red")),
  #                   q$P5+scale_fill_manual("PWID ", values = c("HCV Incidence" = "blue", "HCV Deaths" = "red")),
  #                   q$J5+scale_fill_manual("Prison ", values = c("HCV Incidence" = "blue", "HCV Deaths" = "red")),nrow=1,ncol=3)
  # pp_pr=plot_grid(q$G6+ggtitle("Gen pop point prev"),q$P6+ggtitle("PWID point prev"),q$J6+ggtitle("prison point prev"),nrow=1,ncol=3)
  # cn_pr=plot_grid(q$G7+ggtitle("Gen pop # chronic"),q$P7+ggtitle("PWID # chronic"),q$J7+ggtitle("prison # chronic"),nrow=1,ncol=3)
  # M=rn_of_three_plots(ggplot_build(q$G8),ggplot_build(q$P8),ggplot_build(q$J8))
  # rt_pr=plot_grid(q$G8+scale_y_continuous(expand=c(0,0),limits=c(M[1],M[2])),
  #                 q$P8+scale_y_continuous(expand=c(0,0),limits=c(M[1],M[2])),
  #                 q$J8+scale_y_continuous(expand=c(0,0),limits=c(M[1],M[2])),nrow=1,ncol=3)
  # 
  # # calculate the costs and qalys for the three populations
  # tcols=1+(timesoffigure-2015)
  # 
  # Lbasegenp=cost_qaly_run_genpop(Xgenpop[,tcols],1,1)
  # Lbasepwid=cost_qaly_run(Xpwid[,tcols],scale00_pwid,scale10_pwid)
  # Lbasepris=cost_qaly_run(Xprison[,tcols],scale00_prison,scale10_prison)
  # return(list(stage_G=q$G,stage_P=q$P,stage_J=q$J,stage_GP=q$GP,stage_JP=q$JP,stage_GJ=q$GJ,stage_GPJ=q$GPJ,stage_threeplots=stage_pr,
  #             mort_G=q$G1,mort_P=q$P1,mort_J=q$J1,mort_GP=q$GP1,mort_JP=q$JP1,mort_GJ=q$GJ1,mort_GPJ=q$GPJ1,mort_threeplots=mort_pr,
  #             mort_G_num=q$G2,mort_P_num=q$P2,mort_J_num=q$J2,mort_GP_num=q$GP2,mort_JP_num=q$JP2,mort_GJ_num=q$GJ2,
  #             mort_GPJ_num=q$GPJ2,mort_threeplots_num=mort_pr_num,
  #             inci_G=q$G3,inci_P=q$P3,inci_J=q$J3,inci_GP=q$GP3,inci_JP=q$JP3,inci_GJ=q$GJ3,inci_GPJ=q$GPJ3,inci_threeplots=inci_pr,
  #             inci_G_num=q$G4,inci_P_num=q$P4,inci_J_num=q$J4,inci_GP_num=q$GP4,inci_JP_num=q$JP4,inci_GJ_num=q$GJ4,
  #             inci_GPJ_num=q$GPJ4,inci_threeplots_num=inci_pr_num,
  #             both_G=q$G5,both_P=q$P5,both_J=q$J5,both_GP=q$GP5,both_JP=q$JP5,both_GJ=q$GJ5,both_GPJ=q$GPJ5,
  #             both_threeplots=both_pr,
  #             point_prev_G=q$G6,point_prev_P=q$P6,point_prev_J=q$J6,point_prev_GP=q$GP6,point_prev_JP=q$JP6,
  #             point_prev_GJ=q$GJ6,point_prev_GPJ=q$GPJ6,point_prev_threeplots=pp_pr,
  #             chronic_number_G=q$G7,chronic_number_P=q$P7,chronic_number_J=q$J7,chronic_number_GP=q$GP7,chronic_number_JP=q$JP7,
  #             chronic_number_GJ=q$GJ7,chronic_number_GPJ=q$GPJ7,chronic_number_threeplots=cn_pr,
  #             R0_number_G=q$G8,R0_number_P=q$P8,R0_number_J=q$J8,R0_number_GP=q$GP8,R0_number_JP=q$JP8,
  #             R0_number_GJ=q$GJ8,R0_number_GPJ=q$GPJ8,R0_number_threeplots=rt_pr,
  #             Lbasegenp=Lbasegenp,Lbasepwid=Lbasepwid,Lbasepris=Lbasepris
  # ))
  } else{
    tcols=1+(timesoffigure-2015)
    Lbasegenp=cost_qaly_run_genpop(Xgenpop[,tcols],1,1,discount_rate)
    Lbasepwid=cost_qaly_run(Xpwid[,tcols],scale00_pwid,scale10_pwid,discount_rate)
    Lbasepris=cost_qaly_run(Xprison[,tcols],scale00_prison,scale10_prison,discount_rate)
    
    return(list( s=s,Xgenpop=Xgenpop,Xpwid=Xpwid,Xprison=Xprison,Lbasegenp=Lbasegenp,Lbasepwid=Lbasepwid,Lbasepris=Lbasepris))
  }
}

run_PWID_with_pop = function(startX,s,ignore_flows){
  # get the gen pop
  version_num = 8
  x=readRDS(paste0("www/genpop_fit_raw_v",version_num,".rds"))
  param_vals_gen_pop=param_setup_gen_pop(x,0)
  treat_cov_mild_gen_pop=0
  treat_cov_severe_gen_pop=0
  screening_cov_gen_pop=0
  screening_diagnosis_duration_gen_pop=1
  ignore_screening_gen_pop=0
  time_ofint_inODEunits_gen_pop=7
  pcom_gen_pop=0
  alpha_gen_pop=0
  treatduration_gen_pop=52/12
  
  
  
  # pop  starts at 2016
  TreatmentStartTime=73  
  Pformermild=-log(1-0.0)
  Pformeradva=-log(1-0.0)
  Pcurrenmild=-log(1-0.0)
  Pcurrenadva=-log(1-0.0)
  Text_FixedyCov="No"
  NSPcov=0
  OSTcov=0
  NSP_OST_cov=0
  Ditimedur=1
  scenario_number=122
  treatduration=52/8
  mortvec=rep(0,26)
  incivec=rep(0,26)
  L= TreatInputVariables_v3(TreatmentStartTime,Pformermild,Pformeradva,Pcurrenmild,Pcurrenadva,
                            Text_FixedyCov,
                            NSPcov,OSTcov,NSP_OST_cov,
                            Ditimedur)
  DAAtreat=L$DAAtreat
  SCreening=L$SCreening  
  NSPOST=L$NSPOST  
  TreatmentStartTime=L$TreatmentStartTime
  #s=runmodel_v4(scenario_number)
  
  # for prison population
  dum=get_prison_scaling_new2(startX)
  scale00_prison=dum$scale00_prison
  scale10_prison=dum$scale10_prison
  startX_prison=dum$startX_prison
  # startX_prison=rep(0,720)
  # prison_pop_with_HCV=(20/100)*3.5*10^6
  # percent_prison_pop_with_HCV_PWID = 0.5
  # percent_prison_pop_with_HCV_rest = (1-percent_prison_pop_with_HCV_PWID)
  # 
  # susceptible_former = c(seq(1,180,20),seq(2,180,20),seq(3,180,20),seq(4,180,20),seq(5,180,20))
  # susceptible_former=c(susceptible_former,susceptible_former+180)
  # susceptible_current =susceptible_former+180
  # former_HCV = setdiff(1:360,susceptible_former)
  # current_HCV = setdiff(361:720,susceptible_current)
  # all_HCV=c(former_HCV,current_HCV)
  # startX_prison[1:360]=startX[1:360]*percent_prison_pop_with_HCV_rest*sum(startX[all_HCV])/sum(startX[former_HCV])
  # startX_prison[361:720]=startX[361:720]*percent_prison_pop_with_HCV_PWID*sum(startX[all_HCV])/sum(startX[current_HCV])
  # # startX is the staring for PWID in non-incarcerated
  # # Assume distribution is the same for prison pop
  # startX_prison = startX_prison*(sum(startX)/sum(startX_prison))
  # 
  # scale00_prison = percent_prison_pop_with_HCV_rest*prison_pop_with_HCV/sum(startX_prison[former_HCV])
  # scale10_prison = percent_prison_pop_with_HCV_PWID*prison_pop_with_HCV/sum(startX_prison[current_HCV])
  # tot_prison = scale00_prison*sum(startX_prison[susceptible_former])+
  #   scale00_prison*sum(startX_prison[former_HCV])+
  #   scale10_prison*sum(startX_prison[susceptible_current])+
  #   scale10_prison*sum(startX_prison[current_HCV])
  # # Assume prison pop of 2,173,800
  # add_to_suscpetible = 2173800 - tot_prison
  # add_to_susceptible_former =add_to_suscpetible* (sum(startX_prison[susceptible_former])/sum(startX_prison[c(susceptible_former,susceptible_current)]))
  # add_to_susceptible_current =   add_to_suscpetible-add_to_susceptible_former
  # startX_prison[susceptible_former]=startX_prison[susceptible_former]+(add_to_susceptible_former *(startX_prison[susceptible_former]/sum(startX_prison[susceptible_former])))/scale00_prison
  # startX_prison[susceptible_current]=startX_prison[susceptible_current]+(add_to_susceptible_current*(startX_prison[susceptible_current]/sum(startX_prison[susceptible_current])))/scale10_prison
  # check that totals add up 
  X=runmodel_nep_v2_with_treat_long_v3_pwid_for_prison(startX,s$scale00,s$scale10,TreatmentStartTime,DAAtreat,SCreening,NSPOST,s,scenario_number,
                                                       treatduration,mortvec,incivec,
                                                       param_vals_gen_pop,
                                                       treat_cov_mild_gen_pop,
                                                       treat_cov_severe_gen_pop,
                                                       screening_cov_gen_pop,
                                                       screening_diagnosis_duration_gen_pop,
                                                       ignore_screening_gen_pop,
                                                       time_ofint_inODEunits_gen_pop,
                                                       pcom_gen_pop,
                                                       alpha_gen_pop,
                                                       treatduration_gen_pop,
                                                       startX_prison,
                                                       scale00_prison,
                                                       scale10_prison,
                                                       TreatmentStartTime,
                                                       DAAtreat,
                                                       SCreening,
                                                       NSPOST,
                                                       scenario_number,
                                                       treatduration,
                                                       mortvec,
                                                       incivec,
                                                       ignore_flows)
  # This occurs because have to start at 2015 and not 2016
  # Starts at 2015 because this is WHO comparison year
  # rescale PWID after fitting all three together
  pop2016=2248500*sum(s$X[361:720,67])/sum(s$X[361:720,58])
  L=coeffsof_v5()
  Xpwid=X$X[361:2160,]
  perof=sum(Xpwid[L$chronic10,2])/sum(Xpwid[361:540,2]) # col 2 is 2016
  scale10_point = perof*pop2016
  TT=readRDS("www/scenario_values.rds")
  prop_HCV_are_PWID = TT$X[scenario_number]/100
  scale00_point = prop_HCV_are_PWID*3.5*10^6-scale10_point # 
  new_PWID_scale10=scale10_point/sum(Xpwid[L$chronic10,2]); 
  new_PWID_scale00=scale00_point/sum(Xpwid[L$chronic00,2]);
  # recalibrate the genpop
  pop_in_gen_pop_with_hcv = 0.2*3500000
  Xgenpop=X$X[1:360,]
  scalefactor = pop_in_gen_pop_with_hcv/sum(Xgenpop[L$chronic00,2]) # no screened
  X$X[1:360,]=X$X[1:360,]*scalefactor

  # rescale prsion after fitting all three together
  #dum = get_prison_scaling3(X$X[2161:3960,2])
  new_prison_scale10=scale10_prison
  new_prison_scale00=scale00_prison
  
  
  
  return(list(X=X,s0_prison = new_prison_scale00,s1_prison=new_prison_scale10,s0_pwid=new_PWID_scale00,s1_pwid=new_PWID_scale10)) # last two slots are now COST a
  
}



runmodel_nep_v2_with_treat_long_v3_pwid_for_prison=function(startX,scale00,scale10,TreatmentStartTime,
                                                            DAAtreat,SCreening,NSPOST,s,
                                                            scenario_number,
                                                            treatduration,
                                                            mortvec,incivec,
                                                            param_vals_gen_pop,
                                                            treat_cov_mild_gen_pop,
                                                            treat_cov_severe_gen_pop,
                                                            screening_cov_gen_pop,
                                                            screening_diagnosis_duration_gen_pop,
                                                            ignore_screening_gen_pop,
                                                            time_ofint_inODEunits_gen_pop,
                                                            pcom_gen_pop,
                                                            alpha_gen_pop,
                                                            treatduration_gen_pop,
                                                            startX_prison,scale00_prison,scale10_prison,TreatmentStartTime_prison,
                                                            DAAtreat_prison,SCreening_prison,NSPOST_prison, # note no s
                                                            scenario_number_prison,
                                                            treatduration_prison,
                                                            mortvec_prison,incivec_prison,ignore_flows,
                                                            pop_per="not used",
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
  # set up the new structure
  # old was  param_vals=param_setup_daa_v2(rin,treatduration) # length 916
  dum=readRDS(paste0("www/scenario_",scenario_number,"_param_PWID.rds"))
  #print(paste0("the scenario number is ",scenario_number))
  param_vals_base=dum[1:916]
  # add in the treatduration
  trt_ind = c(16,37,58,79,100,316,337,358,379,400) # 16,1 17,2 18,3 19,4 20,5 16,16 17,17 18,18 19,19 20,20
  param_vals_base[trt_ind[1:5]]=treatduration # ***************** second five are - the treatduiration
  param_vals_base[trt_ind[6:10]]=-treatduration
  param_vals_base[(trt_ind[1:5]+400)]=treatduration
  param_vals_base[(trt_ind[6:10]+400)]=-treatduration
  pop_per_growth=0 # not used now,left in as zero
  param_vals=c(param_vals_base,nep_rel_risk_red,NSPOST$NSPcov,nepdrop,idu_nep_cess, # nsp
               ost_rel_risk_red,NSPOST$OSTcov,ostdrop,idu_ost_cess,  # ost
               nep_ost_rel_risk_red,NSPOST$NSP_OSTcov,nep_ostdrop,idu_nep_ost_cess, # both
               scale00,scale10,pcom,alpha,TreatmentStartTime-1, # for model
               DAAtreat$Pcurrenmild,DAAtreat$Pcurrenadva, # note no former - need to subtract 2 from index
               SCreening$Ditimedur,SCreening$FixedyCov,pop_per_growth,
               #rin[4],rin[5],rin[6]) # note these three model terms no added here
               dum[918],dum[919],dum[920],
               dum[921],dum[922],
               DAAtreat$Pformermild,DAAtreat$Pformeradva) # These are the new relapse parameters
  # for now just changed the scales
  param_vals_prison=c(param_vals_base,nep_rel_risk_red,NSPOST$NSPcov,nepdrop,idu_nep_cess, # nsp
                      ost_rel_risk_red,NSPOST$OSTcov,ostdrop,idu_ost_cess,  # ost
                      nep_ost_rel_risk_red,NSPOST$NSP_OSTcov,nep_ostdrop,idu_nep_ost_cess, # both
                      scale00_prison,scale10_prison,pcom,alpha,TreatmentStartTime-1, # for model
                      DAAtreat$Pcurrenmild,DAAtreat$Pcurrenadva, # note no former - need to subtract 2 from index
                      SCreening$Ditimedur,SCreening$FixedyCov,pop_per_growth,
                      #rin[4],rin[5],rin[6]) # note these three model terms no added here
                      dum[918],dum[919],dum[920],
                      dum[921],dum[922],
                      DAAtreat$Pformermild,DAAtreat$Pformeradva, 
                      ignore_flows)# These are the new relapse parameters
  
  
  Nint=fitN_v4(1,0.2,1400)
  
  parameters_gen_pop = c(param_vals_gen_pop,treat_cov_mild_gen_pop,treat_cov_severe_gen_pop,
                         screening_cov_gen_pop,screening_diagnosis_duration_gen_pop,
                         ignore_screening_gen_pop,
                         pcom_gen_pop,alpha_gen_pop,(time_ofint_inODEunits_gen_pop-1),treatduration_gen_pop)
  #test_for_leak(parameters,Nint)
  
  dum=asode_nep_ost_daa_long_v3_pwid_for_prison(param_vals,L,startX,
                                                parameters_gen_pop,Nint,
                                                param_vals_prison,startX_prison) # this is the full version with DAA and nep and OST
  #print(NSPOST)
  #Xpwid=dum$X[361:2160,1:26] # restrict the time for these output
  #C=getallpop_all_pwid_for_prison(Xpwid,L,rep(1,9),rep(1,9),26,scale00,scale10)
  #p5=plot_areacomps_v2_pwid_for_prison(0:25,C,1)
  
  #Xpwid_prison=dum$X[2161:3960,1:26] # restrict the time for these output
  #C=getallpop_all_pwid_for_prison(Xpwid_prison,L,rep(1,9),rep(1,9),26,scale00_prison,scale10_prison)
  #p5_prison=plot_areacomps_v2_pwid_for_prison(0:25,C,2)
  X=dum$X[,1:26] # 2016 to 2040
  return(list(X=X))
}

asode_nep_ost_daa_long_v3_pwid_for_prison=function(param_vals,L,startX,
                                                   parameters_gen_pop,Nint,
                                                   param_vals_prison,startX_prison){
  library(deSolve)
  
  
  init       = c(c(Nint,rep(0,180)),c(startX,rep(0,3*360)),c(startX_prison,rep(0,3*360)))
  parameters = list(G=parameters_gen_pop,P=param_vals,J=param_vals_prison)
  times      = seq(66, 91, by = 1)
  lt=length(times)
  X = ode(y=init, times=times, func=MODEL_nep_ost_daa_long_v3_pwid_for_prison, parms=parameters,method="ode45")
  X=t(X[,2:(2*1800+360+1)])
  a=NULL
  #a=plot_allcomps_v2nep(0:80,X,L$chronic10)
  return(list(a=a,X=X))
}




MODEL_nep_ost_daa_long_v3_pwid_for_prison <- function(time, state, parametersL) {
  
  # Remember genpop isnot scaled and PWID and prison are scaled differently
  # Changed now uses the new trend
 
  prison_params = parametersL$J
  scale00_prison=prison_params[929] # scale00 and scale10 are the former and current scaling factors
  scale10_prison=prison_params[930]
  ignore_flows=rev(prison_params)[1] 
  prison_P=incarceration_parameters_withif(ignore_flows)
  prison_comps=getprison_comps(state) # these are required out of order  
  # genpop
  parameters=parametersL$G
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
  N=state[1:360]
  # percentage PWID infected
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(0,160,20),nrow=9,ncol=10)
  top00_index = matrix(t(dum),nrow=90,ncol=1)
  dum=matrix(rep(Irows,each=9),nrow=9)+matrix(seq(180,340,20),nrow=9,ncol=10)
  top00_index_S = matrix(t(dum),nrow=90,ncol=1)
  top = sum(N[top00_index]) + sum(N[top00_index_S])# 40 is j = 0, 60 is j = 1 and i = 1 for both
  bot = sum(N[1:length(N)]) # 40 is j = 0, 60 is j = 1 and i = 1 for both
  I = 0.1*top/bot;
  
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
  Mdash_genpop=Mdash
  mort_former_genpop = mort_former
  death_vec_genpop=death_vec
  
  X00_genpop = matrix(N[1:(20*9)],20,9)
  X00_S_genpop = matrix(N[181:(180+20*9)],20,9)
  
  # with deaths set as zero - this should be closed
  # age movement also set as zero
  # there are treatment so no failed treatment
  # so only D10 and D00
  # only for the youngest age group since no transition
  #print(sum(prison_P$percent_return_to_genpop*prison_P$av_sentence_duration*prison_comps$X00_prison*scale00_prison))
  #print(sum(X00_genpop))
  d00_genpop=Mdash_genpop%*%X00_genpop-mort_former_genpop*X00_genpop+t(age_matrix%*%t(X00_genpop))-enroll_S*X00_genpop -
    prison_P$incarceration_prob_genp*X00_genpop +
    prison_P$percent_return_to_genpop*prison_P$av_sentence_duration*prison_comps$X00_prison*scale00_prison +
    prison_P$percent_return_to_genpop*prison_P$av_sentence_duration*prison_comps$X01_prison*scale10_prison 
  d00_genpop[1,]=d00_genpop[1,]-c(200,200,200,200,200,100,100,100,100) # remove fixed number each year from susceptible
  d00_genpop[6,]=d00_genpop[6,]+c(200,200,200,200,200,100,100,100,100) # add into acute
  
  
  d00_S_genpop=enroll_S*X00_genpop+Mdash_genpop%*%X00_S_genpop-mort_former*X00_S_genpop+t(age_matrix%*%t(X00_S_genpop)) -
    prison_P$incarceration_prob_genp*X00_S_genpop
  
  d00_genpop[1,1] =d00_genpop[1,1] + sum(mort_former_genpop*X00_genpop) +      
    death_vec_genpop[1]*sum(X00_genpop[12,])  + death_vec_genpop[2]*sum(X00_genpop[13,])  + 
    death_vec_genpop[3]*sum(X00_genpop[14,])  + death_vec_genpop[4]*sum(X00_genpop[15,])  +
    sum(mort_former*X00_S_genpop) +
    death_vec_genpop[1]*sum(X00_S_genpop[12,])  + death_vec_genpop[2]*sum(X00_S_genpop[13,])  + 
    death_vec_genpop[3]*sum(X00_S_genpop[14,])  + death_vec_genpop[4]*sum(X00_S_genpop[15,])  
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
  
  
  
  # PWID
  parameters=parametersL$P
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
  N=state[361:2160]
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
    idu_nep_cess*X10_nep+idu_ost_cess*X10_ost+idu_nep_ost_cess*X10_nep_ost -
    prison_P$incarceration_prob_form*X00 +
    (1-prison_P$percent_return_to_genpop)*prison_P$av_sentence_duration*prison_comps$X00_prison*(scale00_prison/scale00)
  d01=M%*%X01-extra_parms_mat*X01+extra_parms_vals[3]*X11-mort_former*X01+t(age_matrix%*%t(X01)) +
    idu_nep_cess*X11_nep+idu_ost_cess*X11_ost+idu_nep_ost_cess*X11_nep_ost -
    prison_P$incarceration_prob_form*X01 +
    (1-prison_P$percent_return_to_genpop)*prison_P$av_sentence_duration*prison_comps$X01_prison*(scale00_prison/scale00)
  d10=Mdash%*%X10+extra_parms_mat*X00-extra_parms_vals[3]*X10-mort_current*X10+t(age_matrix%*%t(X10))-
    cov_val_rate*X10 + dropout_nep*X10_nep  + dropout_ost*X10_ost  + dropout_nep_ost*X10_nep_ost -
    prison_P$incarceration_prob_PWID*X10 +
    prison_P$av_sentence_duration*prison_comps$X10_prison*(scale10_prison/scale10)
  d11=Mdash%*%X11+extra_parms_mat*X01-extra_parms_vals[3]*X11-mort_current*X11+t(age_matrix%*%t(X11))-
    cov_val_rate*X11 + dropout_nep*X11_nep  + dropout_ost*X11_ost + dropout_nep_ost*X11_nep_ost -
    prison_P$incarceration_prob_PWID*X11 +
    prison_P$av_sentence_duration*prison_comps$X11_prison*(scale10_prison/scale10)
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
  
  # have to be renamed to use in the prison model
  X00_pwid_former = X00
  X01_pwid_former = X01
  X10_pwid_current = X10
  X11_pwid_current = X11
  
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
  
  # prison
  parameters=parametersL$J
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
  scale00_prison=parameters[929] # scale00 and scale10 are the former and current scaling factors
  scale10_prison=parameters[930]
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
  # increases the detah rates for prison
  mort_current = mort_current*prison_P$background_mort_rr_increase
  mort_former = mort_former*prison_P$background_mort_rr_increase
  Irows = c(6,7,8,9,10,11,12,13,14,15) # rows of infectious PWID
  #death_vec= c(-log(1-death_rate_dc), -log(1-death_rate_hcc), -log(1-death_rate_lt1), -log(1- death_rate_lt2)); #DC,HCC,LT1,LT2
  # No already converted to rates in param_setup_daa and param_setup
  death_vec= c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  death_vec=death_vec*prison_P$liver_related_mort_rr_increase
  N=state[2161:3960]
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
  I = top/bot
  scaleI=prison_P$increase_in_infection_rate_for_incarcerated # increased incidence multiplier for incarcerated
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
  d00_prison=M%*%X00-extra_parms_mat*X00+extra_parms_vals[3]*X10-mort_former*X00+t(age_matrix%*%t(X00)) +
    idu_nep_cess*X10_nep+idu_ost_cess*X10_ost+idu_nep_ost_cess*X10_nep_ost +
    prison_P$incarceration_prob_form*X00_pwid_former*(scale00/scale00_prison) + 
    prison_P$incarceration_prob_genp*X00_genpop/scale00_prison + prison_P$incarceration_prob_genp*X00_S_genpop/scale00_prison -
    prison_P$av_sentence_duration*X00
  d01_prison=M%*%X01-extra_parms_mat*X01+extra_parms_vals[3]*X11-mort_former*X01+t(age_matrix%*%t(X01)) +
    idu_nep_cess*X11_nep+idu_ost_cess*X11_ost+idu_nep_ost_cess*X11_nep_ost +
    prison_P$incarceration_prob_form*X01_pwid_former*(scale00/scale00_prison) -
    prison_P$av_sentence_duration*X01
  d10_prison=Mdash%*%X10+extra_parms_mat*X00-extra_parms_vals[3]*X10-mort_current*X10+t(age_matrix%*%t(X10))-
    cov_val_rate*X10 + dropout_nep*X10_nep  + dropout_ost*X10_ost  + dropout_nep_ost*X10_nep_ost +
    prison_P$incarceration_prob_PWID*X10_pwid_current*(scale10/scale10_prison) -
    prison_P$av_sentence_duration*X10
  d11_prison=Mdash%*%X11+extra_parms_mat*X01-extra_parms_vals[3]*X11-mort_current*X11+t(age_matrix%*%t(X11))-
    cov_val_rate*X11 + dropout_nep*X11_nep  + dropout_ost*X11_ost + dropout_nep_ost*X11_nep_ost +
    prison_P$incarceration_prob_PWID*X11_pwid_current*(scale10/scale10_prison) -
    prison_P$av_sentence_duration*X11
  # Add two extra models for d10_nep and d11_nep
  # The NEP enrollment rate is enroll_nep, they can come from any of the d10 or d11 compartments
  d10_nep_prison = cov_probs[1]*cov_val_rate*X10 +
    Mdash_nep%*%X10_nep-mort_current*X10_nep+t(age_matrix%*%t(X10_nep))-
    dropout_nep*X10_nep - idu_nep_cess*X10_nep
  d11_nep_prison = cov_probs[1]*cov_val_rate*X11 +
    Mdash_nep%*%X11_nep-mort_current*X11_nep+t(age_matrix%*%t(X11_nep))-
    dropout_nep*X11_nep - idu_nep_cess*X11_nep
  d10_ost_prison = cov_probs[2]*cov_val_rate*X10 +
    Mdash_ost%*%X10_ost-mort_current*X10_ost+t(age_matrix%*%t(X10_ost))-
    dropout_ost*X10_ost - idu_ost_cess*X10_ost
  d11_ost_prison = cov_probs[2]*cov_val_rate*X11  +
    Mdash_ost%*%X11_ost-mort_current*X11_ost+t(age_matrix%*%t(X11_ost))-
    dropout_ost*X11_ost - idu_ost_cess*X11_ost
  d10_nep_ost_prison = cov_probs[3]*cov_val_rate*X10  +
    Mdash_nep_ost%*%X10_nep_ost-mort_current*X10_nep_ost+t(age_matrix%*%t(X10_nep_ost))-
    dropout_nep_ost*X10_nep_ost - idu_nep_ost_cess*X10_nep_ost
  d11_nep_ost_prison = cov_probs[3]*cov_val_rate*X11  +
    Mdash_nep_ost%*%X11_nep_ost-mort_current*X11_nep_ost+t(age_matrix%*%t(X11_nep_ost))-
    dropout_nep_ost*X11_nep_ost - idu_nep_ost_cess*X11_nep_ost
  # deaths not re-added # check the scales - no scaling for prison data
  dum=treat_comps_pv7(N,scale00_prison,scale10_prison,t,pcom,alpha,time_ofint_inODEunits,
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
  
  trtmodel00_prison=-phi[1:180]+phidash[1:180]
  trtmodel01_prison=(1-alpha*pcom_former)*phi[1:180] -
    phi[181:360]+phidash[181:360] + # This moves the chronic to treated compartments
    (1-alpha*pcom_former)*phi[181:360] # adds back in the failures
  
  trtmodel10_prison=-phi[361:540]+phidash[361:540]
  trtmodel11_prison=(1-alpha*pcom)*phi[361:540] -
    phi[541:720]+phidash[541:720]+
    (1-alpha*pcom)*phi[541:720]
  
  trtmodel10_nep_prison=-phi[721:900]+phidash[721:900]
  trtmodel11_nep_prison=(1-alpha*pcom)*phi[721:900] #treatment failures aplha*pcom is in phidash above
  
  trtmodel10_ost_prison=-phi[1081:1260]+phidash[1081:1260]
  trtmodel11_ost_prison=(1-alpha*pcom)*phi[1081:1260] #treatment failures aplha*pcom is in phidash above
  
  trtmodel10_nep_ost_prison=-phi[1441:1620]+phidash[1441:1620]
  trtmodel11_nep_ost_prison=(1-alpha*pcom)*phi[1441:1620] #treatment failures aplha*pcom is in phidash above
  
  return(list(c(as.vector(d00_genpop),as.vector(d00_S_genpop),
                as.vector(d00)+trtmodel00, as.vector(d01)+trtmodel01, as.vector(d10)+trtmodel10, as.vector(d11)+trtmodel11,
                as.vector(d10_nep)+trtmodel10_nep, as.vector(d11_nep)+trtmodel11_nep,
                as.vector(d10_ost)+trtmodel10_ost, as.vector(d11_ost)+trtmodel11_ost,
                as.vector(d10_nep_ost)+trtmodel10_nep_ost, as.vector(d11_nep_ost)+trtmodel11_nep_ost,
                as.vector(d00_prison)+trtmodel00_prison, as.vector(d01_prison)+trtmodel01_prison, as.vector(d10_prison)+trtmodel10_prison, as.vector(d11_prison)+trtmodel11_prison,
                as.vector(d10_nep_prison)+trtmodel10_nep_prison, as.vector(d11_nep_prison)+trtmodel11_nep_prison,
                as.vector(d10_ost_prison)+trtmodel10_ost_prison, as.vector(d11_ost_prison)+trtmodel11_ost_prison,
                as.vector(d10_nep_ost_prison)+trtmodel10_nep_ost_prison, as.vector(d11_nep_ost_prison)+trtmodel11_nep_ost_prison)))
  
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


plot_areacomps_v2_pwid_for_prison=function(times,C,type_of_plot){
  nt=length(times)
  if (type_of_plot==1){
    titly = "PWID model - Number in chronic compartments"
  } else{
    titly = "Prison model - Number in chronic compartments"
  }
  
  y=c(C$f0,C$f1,C$f2,C$f3,C$f4,C$dc,C$hcc,C$lt)
  Stage=c(rep("F0",nt),rep("F1",nt),rep("F2",nt),rep("F3",nt),rep("F4",nt),rep("DC",nt),rep("HCC",nt),rep("LT",nt))
  t=rep(times,8)
  df=data.frame(y,t,Stage)
  maxy=max(pretty_breaks()(c(0, (C$f0+C$f1+C$f2+C$f3+C$f4+C$dc+C$hcc+C$lt))))
  vecy=pretty_breaks()(c(0, (C$f0+C$f1+C$f2+C$f3+C$f4+C$dc+C$hcc+C$lt)))
  df$Stage=factor(df$Stage,levels=c("F0","F1","F2","F3","F4","DC","HCC","LT"))
  p1=ggplot(data=subset(df,times>=1), aes(x=t+2015, y=y,fill=Stage)) + geom_area() +
    xlab("Year") + ylab("Population") + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2016,2040,1)),expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p1)  
}


getallpop_all_pwid_for_prison=function(X,L,age_weights_current,age_weights_former,nt,age_scale00,age_scale10){
  #X is the 720 * 82 matrix
  #L is the list of all the compartment index
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
  
}


plotstacked_v3_noscaling_genpop=function(X,L){
  times      = seq(0, 25, by = 1)
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
  p1=ggplot(data=subset(df,times>=1 & times<=25), aes(x=t+2015, y=y,fill=Stage)) + geom_area() +
    xlab("Year") + ylab("Population") + ggtitle(paste0("Number in chronic compartments"))+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2016,2040,1)),limits=c(2016,2040),expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

start_value_for_pwid=function(per_of_HCV_in_prison){
  
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
    dum=move_DAA_v4_gen_pop(X,treat_cov_mild,treat_cov_severe,
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


move_DAA_v4_gen_pop=function(X,treat_cov_mild,treat_cov_severe,
                             screening_cov,screening_diagnosis_duration,
                             ignore_screening,
                             Mild_screen,Ad_screen,
                             F0_screen,F1_screen,F2_screen,F3_screen,F4_screen,
                             pcom,alpha){
  # Use the same methodology as the PWID intervention
  
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
  phi=rep(0,360)
  ntreats=c(rep(ntreat_nmild,3),rep(ntreat_nadva,2))
  if (ignore_screening==1){
    # ignores prevention measures
    if (sum(X[F0])>0){
      nDistributedTreats=min(ntreats[1],sum(X[F0]))
      phi[F0]=nDistributedTreats*(X[F0]/sum(X[F0]))
    }
    if (sum(X[F1])>0){
      nDistributedTreats=min(ntreats[2],sum(X[F1]))
      phi[F1]=nDistributedTreats*(X[F1]/sum(X[F1]))
    }
    if (sum(X[F2])>0){
      nDistributedTreats=min(ntreats[3],sum(X[F2]))
      phi[F2]=nDistributedTreats*(X[F2]/sum(X[F2]))
    }
    if (sum(X[F3])>0){
      nDistributedTreats=min(ntreats[4],sum(X[F3]))
      phi[F3]=nDistributedTreats*(X[F3]/sum(X[F3]))
    }
    if (sum(X[F4])>0){
      nDistributedTreats=min(ntreats[5],sum(X[F4]))
      phi[F4]=nDistributedTreats*(X[F4]/sum(X[F4]))
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
    ntreats_screen = ntreats*(1-exp(-screening_cov))*(1-exp(-1/screening_diagnosis_duration))
    if (sum(X[F0_screen])>0){
      nDistributedTreats=min(ntreats_screen[1],sum(X[F0_screen]))
      phi[F0_screen]=nDistributedTreats*(X[F0_screen]/sum(X[F0_screen]))
    }
    if (sum(X[F1_screen])>0){
      nDistributedTreats=min(ntreats_screen[2],sum(X[F1_screen]))
      phi[F1_screen]=nDistributedTreats*(X[F1_screen]/sum(X[F1_screen]))
    }
    if (sum(X[F2_screen])>0){
      nDistributedTreats=min(ntreats_screen[3],sum(X[F2_screen]))
      phi[F2_screen]=nDistributedTreats*(X[F2_screen]/sum(X[F2_screen]))
    }
    if (sum(X[F3_screen])>0){
      nDistributedTreats=min(ntreats_screen[4],sum(X[F3_screen]))
      phi[F3_screen]=nDistributedTreats*(X[F3_screen]/sum(X[F3_screen]))
    }
    if (sum(X[F4_screen])>0){
      nDistributedTreats=min(ntreats_screen[5],sum(X[F4_screen]))
      phi[F4_screen]=nDistributedTreats*(X[F4_screen]/sum(X[F4_screen]))
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
  total_T4 =     phi[F4]+phi[F4_screen]    
  
  # no direct deaths only for DC,HCC and LT1 and LT2
  
  return(list(phi=phi,phidash=phidash,total_not_T4=total_not_T4,total_T4=total_T4))
}


incarceration_parameters_withif=function(ignore_flows){
  # These will be read from the database
  
  # df= data.frame(incarceration_prob_PWID = 0.238,incarceration_prob_form = 0.01,
  #                incarceration_prob_genp = 0.001,av_sentence_duration = 3.5 ,
  #                background_mort_rr_increase = 1.2,liver_related_mort_rr_increase = 1.2,
  #                percent_of_HCV_cases_incarcerated = 0.2,percent_of_HCV_incarcerated_that_are_PWID=0.5,
  #                increase_in_infection_rate_for_incarcerated = 2.8, reduction_in_NSP_OST_going_to_jail = 0.5,
  #                percent_return_to_genpop = 0.5,
  #                tot_prison_pop_in_2015=2300000)
  # # convert to rates
  # df$incarceration_prob_PWID =-log(1- df$incarceration_prob_PWID)
  # df$incarceration_prob_form =-log(1-df$incarceration_prob_form)
  # df$incarceration_prob_genp =-log(1-df$incarceration_prob_genp) 
  # df$av_sentence_duration =1/df$av_sentence_duration
  
  # test
  df= data.frame(incarceration_prob_PWID = 0.1004484,incarceration_prob_form = 0.01,
                 incarceration_prob_genp = 0.0035,av_sentence_duration = 3,
                 background_mort_rr_increase = 1,liver_related_mort_rr_increase = 1,
                 percent_of_HCV_cases_incarcerated = 17.26842,percent_of_HCV_incarcerated_that_are_PWID=0.5,
                 increase_in_infection_rate_for_incarcerated = 3.941874, reduction_in_NSP_OST_going_to_jail = 0.5,
                 percent_return_to_genpop = 0.2,
                 tot_prison_pop_in_2015=2173800)

  
  #**********************
  if (ignore_flows==1){
  ## Turn off flow between models
    df$incarceration_prob_genp=0
    df$percent_return_to_genpop=0
    df$incarceration_prob_PWID=0
    df$incarceration_prob_form=0
    df$av_sentence_duration=1000 # 
   #df$increase_in_infection_rate_for_incarcerated =1
   #df$background_mort_rr_increase = 1
   #df$liver_related_mort_rr_increase = 1
  }
  # convert to rates
  df$incarceration_prob_PWID =-log(1- df$incarceration_prob_PWID)
  df$incarceration_prob_form =-log(1-df$incarceration_prob_form)
  df$incarceration_prob_genp =-log(1-df$incarceration_prob_genp)
  df$av_sentence_duration =1/df$av_sentence_duration
  
  return(df)
  
  
  
}

getprison_comps=function(state){
  # This gets the current state of the prison compartment
  # needed before the prison model is processed
  N=state[2161:3960]
  df=list(X00_prison = matrix(N[1:(20*9)],20,9),
          X01_prison = matrix(N[181:(180+20*9)],20,9),
          X10_prison = matrix(N[361:(360+20*9)],20,9),
          X11_prison = matrix(N[541:(540+20*9)],20,9),
          X10_nep_prison = matrix(N[721:(720+20*9)],20,9),
          X11_nep_prison = matrix(N[901:(900+20*9)],20,9),
          X10_ost_prison = matrix(N[1081:(1080+20*9)],20,9),
          X11_ost_prison = matrix(N[1261:(1260+20*9)],20,9),
          X10_nep_ost_prison = matrix(N[1441:(1440+20*9)],20,9),
          X11_nep_ost_prison = matrix(N[1621:(1620+20*9)],20,9))
  return(df)
}

get_prison_scaling=function(startX){
  startX_prison=rep(0,720)
  prison_pop_with_HCV=(20/100)*3.5*10^6
  percent_prison_pop_with_HCV_PWID = 0.5
  percent_prison_pop_with_HCV_rest = (1-percent_prison_pop_with_HCV_PWID)
  
  susceptible_former = c(seq(1,180,20),seq(2,180,20),seq(3,180,20),seq(4,180,20),seq(5,180,20))
  susceptible_former=c(susceptible_former,susceptible_former+180)
  susceptible_current =susceptible_former+180
  former_HCV = setdiff(1:360,susceptible_former)
  current_HCV = setdiff(361:720,susceptible_current)
  all_HCV=c(former_HCV,current_HCV)
  startX_prison[1:360]=startX[1:360]*percent_prison_pop_with_HCV_rest*sum(startX[all_HCV])/sum(startX[former_HCV])
  startX_prison[361:720]=startX[361:720]*percent_prison_pop_with_HCV_PWID*sum(startX[all_HCV])/sum(startX[current_HCV])
  # startX is the staring for PWID in non-incarcerated
  # Assume distribution is the same for prison pop
  startX_prison = startX_prison*(sum(startX)/sum(startX_prison))
  
  scale00_prison = percent_prison_pop_with_HCV_rest*prison_pop_with_HCV/sum(startX_prison[former_HCV])
  scale10_prison = percent_prison_pop_with_HCV_PWID*prison_pop_with_HCV/sum(startX_prison[current_HCV])
  tot_prison = scale00_prison*sum(startX_prison[susceptible_former])+
    scale00_prison*sum(startX_prison[former_HCV])+
    scale10_prison*sum(startX_prison[susceptible_current])+
    scale10_prison*sum(startX_prison[current_HCV])
  # Assume prison pop of 2,173,800
  add_to_suscpetible = 2173800 - tot_prison
  add_to_susceptible_former =add_to_suscpetible* (sum(startX_prison[susceptible_former])/sum(startX_prison[c(susceptible_former,susceptible_current)]))
  add_to_susceptible_current =   add_to_suscpetible-add_to_susceptible_former
  startX_prison[susceptible_former]=startX_prison[susceptible_former]+(add_to_susceptible_former *(startX_prison[susceptible_former]/sum(startX_prison[susceptible_former])))/scale00_prison
  startX_prison[susceptible_current]=startX_prison[susceptible_current]+(add_to_susceptible_current*(startX_prison[susceptible_current]/sum(startX_prison[susceptible_current])))/scale10_prison
  
  return(list(scale00_prison=scale00_prison,scale10_prison=scale10_prison,startX_prison=startX_prison))
}

get_prison_scaling2=function(startX){
  startX_prison=rep(0,720)
  prison_pop_with_HCV=(20/100)*3.5*10^6
  percent_prison_pop_with_HCV_PWID = 0.5
  percent_prison_pop_with_HCV_rest = (1-percent_prison_pop_with_HCV_PWID)
  acutes_former=c(seq(6,180,20),seq(6,180,20)+180) # remove acutes - HCV is chroic compartments
  acutes_current=acutes_former+360
  susceptible_former = c(seq(1,180,20),seq(2,180,20),seq(3,180,20),seq(4,180,20),seq(5,180,20))
  susceptible_former=c(susceptible_former,susceptible_former+180)
  susceptible_current =susceptible_former+360
  former_HCV = setdiff(1:360,c(susceptible_former,acutes_former))
  current_HCV = setdiff(361:720,c(susceptible_current,acutes_current))
  all_HCV=c(former_HCV,current_HCV)
  startX_prison[1:360]=startX[1:360]*percent_prison_pop_with_HCV_rest*sum(startX[all_HCV])/sum(startX[former_HCV])
  startX_prison[361:720]=startX[361:720]*percent_prison_pop_with_HCV_PWID*sum(startX[all_HCV])/sum(startX[current_HCV])
  # startX is the staring for PWID in non-incarcerated
  # Assume distribution is the same for prison pop
  startX_prison = startX_prison*(sum(startX)/sum(startX_prison))
  
  scale00_prison = percent_prison_pop_with_HCV_rest*prison_pop_with_HCV/sum(startX_prison[former_HCV])
  scale10_prison = percent_prison_pop_with_HCV_PWID*prison_pop_with_HCV/sum(startX_prison[current_HCV])
  tot_prison = scale00_prison*sum(startX_prison[susceptible_former])+
    scale00_prison*sum(startX_prison[former_HCV])+
    scale00_prison*sum(startX_prison[acutes_former])+
    scale10_prison*sum(startX_prison[susceptible_current])+
    scale10_prison*sum(startX_prison[current_HCV]) +
    scale10_prison*sum(startX_prison[acutes_current])
    
  # Assume prison pop of 2,173,800
  add_to_suscpetible = 2173800 - tot_prison
  add_to_susceptible_former =add_to_suscpetible* (sum(startX_prison[susceptible_former])/sum(startX_prison[c(susceptible_former,susceptible_current)]))
  add_to_susceptible_current =   add_to_suscpetible-add_to_susceptible_former
  startX_prison[susceptible_former]=startX_prison[susceptible_former]+(add_to_susceptible_former *(startX_prison[susceptible_former]/sum(startX_prison[susceptible_former])))/scale00_prison
  startX_prison[susceptible_current]=startX_prison[susceptible_current]+(add_to_susceptible_current*(startX_prison[susceptible_current]/sum(startX_prison[susceptible_current])))/scale10_prison
  
  #check
  # Browse[2]> sum(startX_prison[former_HCV]*scale00_prison)+sum(startX_prison[current_HCV]*scale10_prison)
  # [1] 700000
  # Browse[2]> sum(startX_prison[1:360]*scale00_prison)+sum(startX_prison[361:720]*scale10_prison)
  # [1] 2173800
  return(list(scale00_prison=scale00_prison,scale10_prison=scale10_prison,startX_prison=startX_prison))
}

get_prison_scaling_new2=function(startX){
  #startX=readRDS("/home/mrcuser/HCV_vV/startXtest")
  #https://en.wikipedia.org/wiki/Incarceration_in_the_United_States 2.3 million
  prisonpop = 0.9219834*2173800#  2173800 # assume 75% of the prison pop are susceptible
  # 50% are PWID https://www.unodc.org/unodc/en/hiv-aids/people-who-inject-drugs-in-prison.html
  pwidprisonpop = 0.5*prisonpop
  # 230,000,000 adult gen pop, 0.002 are imprisoned each year
  # Asumme that 50% are susceptible
  genprisonpop = 230000000*0.003003233*0.5
  formerprisonpop = prisonpop - pwidprisonpop - genprisonpop
  # redistribute PWID
  former_comp=redistributepop(startX[1:360],formerprisonpop)
  pwid_comp=redistributepop(startX[361:720],pwidprisonpop)
  # add the genprisonpop to susceptible
  #former_comp[1]=former_comp[1]+genprisonpop
  # spread over the ages
  ages = seq(1,179,20)
  person_dist=c(5.4,12.2,16.7,34.1,19.9,8.8,1.8,1.0,0.1)/100
  former_comp[ages]=former_comp[ages] +person_dist*genprisonpop
  startX=cbind(former_comp,pwid_comp)
  startX=1000*startX/sum(startX)
  
  startX_prison=rep(0,720)
  prison_pop_with_HCV=(18.2/100)*3.5*10^6
  percent_prison_pop_with_HCV_PWID = 0.5
  percent_prison_pop_with_HCV_rest = (1-percent_prison_pop_with_HCV_PWID)
  acutes_former=c(seq(6,180,20),seq(6,180,20)+180) # remove acutes - HCV is chroic compartments
  acutes_current=acutes_former+360
  susceptible_former = c(seq(1,180,20),seq(2,180,20),seq(3,180,20),seq(4,180,20),seq(5,180,20))
  susceptible_former=c(susceptible_former,susceptible_former+180)
  susceptible_current =susceptible_former+360
  former_HCV = setdiff(1:360,c(susceptible_former,acutes_former))
  current_HCV = setdiff(361:720,c(susceptible_current,acutes_current))
  all_HCV=c(former_HCV,current_HCV)
  startX_prison[1:360]=startX[1:360]*percent_prison_pop_with_HCV_rest*sum(startX[all_HCV])/sum(startX[former_HCV])
  startX_prison[361:720]=startX[361:720]*percent_prison_pop_with_HCV_PWID*sum(startX[all_HCV])/sum(startX[current_HCV])
  # startX is the staring for PWID in non-incarcerated
  # Assume distribution is the same for prison pop
  startX_prison = startX_prison*(sum(startX)/sum(startX_prison))
  
  scale00_prison = percent_prison_pop_with_HCV_rest*prison_pop_with_HCV/sum(startX_prison[former_HCV])
  scale10_prison = percent_prison_pop_with_HCV_PWID*prison_pop_with_HCV/sum(startX_prison[current_HCV])
  tot_prison = scale00_prison*sum(startX_prison[susceptible_former])+
    scale00_prison*sum(startX_prison[former_HCV])+
    scale00_prison*sum(startX_prison[acutes_former])+
    scale10_prison*sum(startX_prison[susceptible_current])+
    scale10_prison*sum(startX_prison[current_HCV]) +
    scale10_prison*sum(startX_prison[acutes_current])
  
  # Assume prison pop of prisonpop, only susceptible go in the model
  add_to_suscpetible = prisonpop - tot_prison
  add_to_susceptible_former =add_to_suscpetible* (sum(startX_prison[susceptible_former])/sum(startX_prison[c(susceptible_former,susceptible_current)]))
  add_to_susceptible_current =   add_to_suscpetible-add_to_susceptible_former
  startX_prison[susceptible_former]=startX_prison[susceptible_former]+(add_to_susceptible_former *(startX_prison[susceptible_former]/sum(startX_prison[susceptible_former])))/scale00_prison
  startX_prison[susceptible_current]=startX_prison[susceptible_current]+(add_to_susceptible_current*(startX_prison[susceptible_current]/sum(startX_prison[susceptible_current])))/scale10_prison
  df = 45
  
  # # take 10% from chronic and add to susceptible
  # extrab_former = pin* sum(startX_prison[former_HCV])
  # extrab_current = pin*sum(startX_prison[current_HCV])
  # startX_prison[former_HCV] = (1-pin)*startX_prison[former_HCV]
  # startX_prison[current_HCV] = (1-pin)*startX_prison[current_HCV]
  # startX_prison[susceptible_former]=startX_prison[susceptible_former]+(extrab_former *(startX_prison[susceptible_former]/sum(startX_prison[susceptible_former])))/scale00_prison
  # startX_prison[susceptible_current]=startX_prison[susceptible_current]+(extrab_current*(startX_prison[susceptible_current]/sum(startX_prison[susceptible_current])))/scale10_prison
  
  
  #check
  # Browse[2]> sum(startX_prison[former_HCV]*scale00_prison)+sum(startX_prison[current_HCV]*scale10_prison)
  # [1] 700000
  # Browse[2]> sum(startX_prison[1:360]*scale00_prison)+sum(startX_prison[361:720]*scale10_prison)
  # [1] 2173800
  return(list(scale00_prison=scale00_prison,scale10_prison=scale10_prison,startX_prison=startX_prison))
}


get_prison_scaling3=function(startX_prison){
  prison_pop_with_HCV=(20/100)*3.5*10^6
  percent_prison_pop_with_HCV_PWID = 0.5
  percent_prison_pop_with_HCV_rest = (1-percent_prison_pop_with_HCV_PWID)
  acutes_former=c(seq(6,180,20),seq(6,180,20)+180) # remove acutes - HCV is chroic compartments
  acutes_current=acutes_former+360
  susceptible_former = c(seq(1,180,20),seq(2,180,20),seq(3,180,20),seq(4,180,20),seq(5,180,20))
  susceptible_former=c(susceptible_former,susceptible_former+180)
  susceptible_current =susceptible_former+360
  former_HCV = setdiff(1:360,c(susceptible_former,acutes_former))
  current_HCV = setdiff(361:720,c(susceptible_current,acutes_current))
  all_HCV=c(former_HCV,current_HCV)
  scale00_prison = percent_prison_pop_with_HCV_rest*prison_pop_with_HCV/sum(startX_prison[former_HCV])
  scale10_prison = percent_prison_pop_with_HCV_PWID*prison_pop_with_HCV/sum(startX_prison[current_HCV])

  return(list(scale00_prison=scale00_prison,scale10_prison=scale10_prison))
}
get_prison_re_scaling=function(X){
  prison_pop_with_HCV=700000
  percent_prison_pop_with_HCV_PWID = 0.5
  percent_prison_pop_with_HCV_rest = (1-percent_prison_pop_with_HCV_PWID)
  acutes_former=c(seq(6,180,20),seq(6,180,20)+180) # remove acutes - HCV is chroic compartments
  acutes_current=acutes_former+360
  susceptible_former = c(seq(1,180,20),seq(2,180,20),seq(3,180,20),seq(4,180,20),seq(5,180,20))
  susceptible_former=c(susceptible_former,susceptible_former+180)
  susceptible_current =susceptible_former+360
  former_HCV = setdiff(1:360,c(susceptible_former,acutes_former))
  current_HCV = setdiff(361:720,c(susceptible_current,acutes_current))
  all_HCV=c(former_HCV,current_HCV)
  scale00_prison = percent_prison_pop_with_HCV_rest*prison_pop_with_HCV/sum(X[former_HCV])
  scale10_prison = percent_prison_pop_with_HCV_PWID*prison_pop_with_HCV/sum(X[current_HCV])
  
  return(list(scale00_prison=scale00_prison,scale10_prison=scale10_prison))
}


redistributepop=function(X,tot){
  # fills comaprtments proportionally so that the total is tot
  Y=tot*X/sum(X)
  return(Y)
}
