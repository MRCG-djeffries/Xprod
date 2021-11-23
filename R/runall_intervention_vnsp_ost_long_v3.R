runall_intervention_vnsp_ost_long_v3=function (Pformermild,Pformeradva,Pcurrenmild,Pcurrenadva,
                                            Text_FixedyCov,
                                            NSPcov,OSTcov,NSP_OST_cov,
                                            Ditimedur,scenario_number,
                                            treatduration,
                                            mortvec,incivec,
                                            pop_per="not used"){ # pop_per is now Yes or Flat here
  library(gridExtra)
  library(cowplot)
  library(ggplot2)
  library(scales)
  #source("global.R")  
  # update June 2021
  # runs the long version 0:140
  #z7=runall_intervention_vnsp_ost_long_v3(-log(1-0.1),-log(1-0.1),-log(1-0.1),-log(1-0.1),"Yes",0,0,0,1,1,52/8,rep(0,26),rep(0,26))
  #            z6 is 10% coverage and 10% OST intervention
  # mortality reduction is very small on NSP etc only without DAA as needs to run for longer to clear out DCC, HCC and LT
  #z7=runall_intervention_vnsp_ost_long_v3(-log(1-0.0),-log(1-0.0),-log(1-0.1),-log(1-0.0),"No",0,0,0,1,1,52/8,rep(0,26),rep(0,26))
  #Ftab=output_ICER_text_v2(sum(z7$COST),sum(z7$QALYS),sum(z7$Qdattreat$COST),sum(z7$Qdattreat$QALYS),"Yes",
  #z7$stage_vals,z7$Qdattreat$stage_vals,122)

  
  
  TreatmentStartTime=73  
  L= TreatInputVariables_v3(TreatmentStartTime,Pformermild,Pformeradva,Pcurrenmild,Pcurrenadva,
                            Text_FixedyCov,
                            NSPcov,OSTcov,NSP_OST_cov,
                            Ditimedur)
  DAAtreat=L$DAAtreat
  SCreening=L$SCreening  
  NSPOST=L$NSPOST  
  TreatmentStartTime=L$TreatmentStartTime
  # if (file.exists("www/scenarioval.rds")==TRUE){
  #   scenario_number=readRDS("www/scenarioval.rds")
  # }else{
  #   scenario_number=1
  # }
  s=runmodel_v4(scenario_number)
  X=runmodel_nep_v2_with_treat_long_v3(s$scale00,s$scale10,TreatmentStartTime,DAAtreat,SCreening,NSPOST,s,scenario_number,
                                       treatduration,mortvec,incivec,
                                       pop_per)
  return(c(X,s$Qdatbase)) # last two slots are now COST and QALYS *** for base
  # treatment  is Qdattreat COST and QALYS
}

TreatInputVariables_v3=function(TreatmentStartTime,Pformermild,Pformeradva,Pcurrenmild,Pcurrenadva,
                                Text_FixedyCov,
                                NSPcov,OSTcov,NSP_OSTcov,
                                Ditimedur){
  
  # Takes variables from the intervention page
  A=data.frame(Pformermild,Pformeradva,Pcurrenmild,Pcurrenadva)
  if (Text_FixedyCov=="No"){
    FixedyCov=1
  }else{
    FixedyCov=0
  }
  B=data.frame(Ditimedur,FixedyCov)
  C=data.frame(NSPcov,OSTcov,NSP_OSTcov)
  # Transfers then to variables for the model
  return(list(DAAtreat=A,SCreening=B,NSPOST=C,TreatmentStartTime=TreatmentStartTime))
  
  
}
