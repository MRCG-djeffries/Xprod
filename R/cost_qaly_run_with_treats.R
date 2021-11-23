cost_qaly_run_with_treats=function(X,scale00,scale10,number_of_treats,treatduration,discount_rate){
  # X is the compartments * time output from compartment model
  # columns assumed to be relevant time period in years
  # delta is the spontaneous recovery percentage
  # rDCLT is the rate form DC to LT
  # rHCCLT is the rate form HCC to LT  
  # Q=readRDS("testcost.rds")
  # Z=cost_qaly_run_with_treats(Q$X,Q$scale00,Q$scale10,Q$number_of_treats,Q$treatduration)
  dum=pickuprates()
  delta=dum$delta
  rDCLT=dum$rDCLT
  rHCCLT=dum$rHCCLT
  C=coeffsof_v5()
  L=getdata()
  costsof=L$costsof # annual cost data
  utilsof=L$utilsof # qaly data
  
  # This is for DAA treats so all 720 compartments
  # Now subjects cab be in all compartments
  
  Tcompvec10= rbind(C$S0_comps_10,C$S1_comps_10,C$S2_comps_10,C$S3_comps_10,C$S4_comps_10,C$A_comps_10,
                    C$F0_comps_10,C$F1_comps_10,C$F2_comps_10,C$F3_comps_10,C$F4_comps_10,
                    C$DC_comps_10,C$HCC_comps_10,C$LT1_comps_10,C$LT2_comps_10,
                    C$T0_comps_10,C$T1_comps_10,C$T2_comps_10,C$T3_comps_10,C$T4_comps_10)
  
  Tcompvec00= rbind(C$S0_comps_00,C$S1_comps_00,C$S2_comps_00,C$S3_comps_00,C$S4_comps_00,C$A_comps_00,
                    C$F0_comps_00,C$F1_comps_00,C$F2_comps_00,C$F3_comps_00,C$F4_comps_00,
                    C$DC_comps_00,C$HCC_comps_00,C$LT1_comps_00,C$LT2_comps_00,
                    C$T0_comps_00,C$T1_comps_00,C$T2_comps_00,C$T3_comps_00,C$T4_comps_00)
  
  Tcompvec11= rbind(C$S0_comps_11,C$S1_comps_11,C$S2_comps_11,C$S3_comps_11,C$S4_comps_11,C$A_comps_11,
                    C$F0_comps_11,C$F1_comps_11,C$F2_comps_11,C$F3_comps_11,C$F4_comps_11,
                    C$DC_comps_11,C$HCC_comps_11,C$LT1_comps_11,C$LT2_comps_11,
                    C$T0_comps_11,C$T1_comps_11,C$T2_comps_11,C$T3_comps_11,C$T4_comps_11)
  
  Tcompvec01= rbind(C$S0_comps_01,C$S1_comps_01,C$S2_comps_01,C$S3_comps_01,C$S4_comps_01,C$A_comps_01,
                    C$F0_comps_01,C$F1_comps_01,C$F2_comps_01,C$F3_comps_01,C$F4_comps_01,
                    C$DC_comps_01,C$HCC_comps_01,C$LT1_comps_01,C$LT2_comps_01,
                    C$T0_comps_01,C$T1_comps_01,C$T2_comps_01,C$T3_comps_01,C$T4_comps_01)
  
  # order for below is S0-S4,A,F0-F4,(DCC,HCC,LT1,LT2) and T0-T4
  
  Tutility_vec=c(rep(utilsof$`Spontaneous viral clearance, never infected`,5),
               utilsof$`Acute HCV`,
               rep(utilsof$`Mild chronic HCV (F0/F1/F2)`,3), utilsof$`Moderate chronic HCV (F3)`,utilsof$`Compensated cirrhosis (F4)`,
               utilsof$`Decompensated cirrhosis/liver failure`,  utilsof$`Hepatocellular carcinoma`,utilsof$`Liver transplantation year 1`,utilsof$`Liver transplantation year 2+`,
               rep(utilsof$`Treatment from F0-F2`,3),utilsof$`Treatment from F3`,utilsof$`Treatment from F4 or worse`)


  Tqualy_vals00=matrix(data=0,nrow = 20,ncol=dim(X)[2])
  Tqualy_vals01=matrix(data=0,nrow = 20,ncol=dim(X)[2])
  Tqualy_vals10=matrix(data=0,nrow = 20,ncol=dim(X)[2])
  Tqualy_vals11=matrix(data=0,nrow = 20,ncol=dim(X)[2])
  
  for ( i in 1 :20){
    Xtest00=scale00*colSums(X[Tcompvec00[i,],]);
    Tqualy_vals00[i,]=calc_qalyv2(Xtest00,Tutility_vec[i],discount_rate)
    Xtest01=scale00*colSums(X[Tcompvec01[i,],]);
    Tqualy_vals01[i,]=calc_qalyv2(Xtest01,Tutility_vec[i],discount_rate)
    
    Xtest10=scale10*colSums(X[Tcompvec10[i,],]);
    Tqualy_vals10[i,]=calc_qalyv2(Xtest10,Tutility_vec[i],discount_rate)
    Xtest11=scale10*colSums(X[Tcompvec11[i,],]);
    Tqualy_vals11[i,]=calc_qalyv2(Xtest11,Tutility_vec[i],discount_rate)
  }
  
 qaly=colSums(Tqualy_vals00) + colSums(Tqualy_vals01) +colSums(Tqualy_vals10) +colSums(Tqualy_vals11) 
 r_qaly=rowSums(Tqualy_vals00) + rowSums(Tqualy_vals01) +rowSums(Tqualy_vals10) +rowSums(Tqualy_vals11) 
 # now add NSP, OST and both
 
 Tcompvec10_NSP=Tcompvec10 + 360
 Tcompvec11_NSP=Tcompvec11 + 360
 Tcompvec10_OST=Tcompvec10 + 2*360
 Tcompvec11_OST=Tcompvec11 + 2*360
 Tcompvec10_NSP_OST=Tcompvec10 + 3*360
 Tcompvec11_NSP_OST=Tcompvec11 + 3*360

 Tqualy_vals10_NSP=matrix(data=0,nrow = 20,ncol=dim(X)[2])
 Tqualy_vals11_NSP=matrix(data=0,nrow = 20,ncol=dim(X)[2])
 Tqualy_vals10_OST=matrix(data=0,nrow = 20,ncol=dim(X)[2])
 Tqualy_vals11_OST=matrix(data=0,nrow = 20,ncol=dim(X)[2])
 Tqualy_vals10_NSP_OST=matrix(data=0,nrow = 20,ncol=dim(X)[2])
 Tqualy_vals11_NSP_OST=matrix(data=0,nrow = 20,ncol=dim(X)[2])
 
 for ( i in 1 :20){
   Xtest10_NSP=scale10*colSums(X[Tcompvec10_NSP[i,],]);
   Tqualy_vals10_NSP[i,]=calc_qalyv2(Xtest10_NSP,Tutility_vec[i],discount_rate)
   Xtest11_NSP=scale10*colSums(X[Tcompvec11_NSP[i,],]);
   Tqualy_vals11_NSP[i,]=calc_qalyv2(Xtest11_NSP,Tutility_vec[i],discount_rate)
   
   Xtest10_OST=scale10*colSums(X[Tcompvec10_OST[i,],]);
   Tqualy_vals10_OST[i,]=calc_qalyv2(Xtest10_OST,Tutility_vec[i],discount_rate)
   Xtest11_OST=scale10*colSums(X[Tcompvec11_OST[i,],]);
   Tqualy_vals11_OST[i,]=calc_qalyv2(Xtest11_OST,Tutility_vec[i],discount_rate)
   
   Xtest10_NSP_OST=scale10*colSums(X[Tcompvec10_NSP_OST[i,],]);
   Tqualy_vals10_NSP_OST[i,]=calc_qalyv2(Xtest10_NSP_OST,Tutility_vec[i],discount_rate)
   Xtest11_NSP_OST=scale10*colSums(X[Tcompvec11_NSP_OST[i,],]);
   Tqualy_vals11_NSP_OST[i,]=calc_qalyv2(Xtest11_NSP_OST,Tutility_vec[i],discount_rate)
 }
 
 qaly = qaly + colSums(Tqualy_vals10_NSP) +colSums(Tqualy_vals11_NSP) +
               colSums(Tqualy_vals10_OST) +colSums(Tqualy_vals11_OST) +
               colSums(Tqualy_vals10_NSP_OST) +colSums(Tqualy_vals11_NSP_OST)
 
 r_qaly = r_qaly + rowSums(Tqualy_vals10_NSP) + rowSums(Tqualy_vals11_NSP) +
   rowSums(Tqualy_vals10_OST) + rowSums(Tqualy_vals11_OST) +
   rowSums(Tqualy_vals10_NSP_OST) +rowSums(Tqualy_vals11_NSP_OST)
 r_qaly = c(sum(r_qaly[1:5]),r_qaly[6],r_qaly[7:13],sum(r_qaly[14:15]),sum(r_qaly[16:20]))
 
 # costs
 
 Tcompvec10= rbind(C$F0_comps_10,C$F1_comps_10,C$F2_comps_10,C$F3_comps_10,C$F4_comps_10,
                   C$DC_comps_10,C$HCC_comps_10)
 
 Tcompvec00= rbind(C$F0_comps_00,C$F1_comps_00,C$F2_comps_00,C$F3_comps_00,C$F4_comps_00,
                   C$DC_comps_00,C$HCC_comps_00)
 
 Tcompvec11= rbind(C$F0_comps_11,C$F1_comps_11,C$F2_comps_11,C$F3_comps_11,C$F4_comps_11,
                   C$DC_comps_11,C$HCC_comps_11)
 
 Tcompvec01= rbind(C$F0_comps_01,C$F1_comps_01,C$F2_comps_01,C$F3_comps_01,C$F4_comps_01,
                   C$DC_comps_01,C$HCC_comps_01)
 
 Tcost_vals00=matrix(data=0,nrow = 7,ncol=dim(X)[2])
 Tcost_vals01=matrix(data=0,nrow = 7,ncol=dim(X)[2])
 Tcost_vals10=matrix(data=0,nrow = 7,ncol=dim(X)[2])
 Tcost_vals11=matrix(data=0,nrow = 7,ncol=dim(X)[2])
 costsof=unlist(costsof)
 for ( i in 1 :7){
   Xtest00=scale00*colSums(X[Tcompvec00[i,],]);
   Tcost_vals00[i,]=calc_qalyv2(Xtest00,costsof[i],discount_rate)
   Xtest01=scale00*colSums(X[Tcompvec01[i,],]);
   Tcost_vals01[i,]=calc_qalyv2(Xtest01,costsof[i],discount_rate)
   
   Xtest10=scale10*colSums(X[Tcompvec10[i,],]);
   Tcost_vals10[i,]=calc_qalyv2(Xtest10,costsof[i],discount_rate)
   Xtest11=scale10*colSums(X[Tcompvec11[i,],]);
   Tcost_vals11[i,]=calc_qalyv2(Xtest11,costsof[i],discount_rate)
 }
 
 stage_cost=colSums(Tcost_vals00) + colSums(Tcost_vals01) + colSums(Tcost_vals10) +colSums(Tcost_vals11) 
 r_stage_cost=rowSums(Tcost_vals00) + rowSums(Tcost_vals01) + rowSums(Tcost_vals10) +rowSums(Tcost_vals11) 
 
 # now add NSP, OST and both

 Tcompvec10_NSP=Tcompvec10 + 360
 Tcompvec11_NSP=Tcompvec11 + 360
 Tcompvec10_OST=Tcompvec10 + 2*360
 Tcompvec11_OST=Tcompvec11 + 2*360
 Tcompvec10_NSP_OST=Tcompvec10 + 3*360
 Tcompvec11_NSP_OST=Tcompvec11 + 3*360
 
 Tcost_vals10_NSP=matrix(data=0,nrow = 7,ncol=dim(X)[2])
 Tcost_vals11_NSP=matrix(data=0,nrow = 7,ncol=dim(X)[2])
 Tcost_vals10_OST=matrix(data=0,nrow = 7,ncol=dim(X)[2])
 Tcost_vals11_OST=matrix(data=0,nrow = 7,ncol=dim(X)[2])
 Tcost_vals10_NSP_OST=matrix(data=0,nrow = 7,ncol=dim(X)[2])
 Tcost_vals11_NSP_OST=matrix(data=0,nrow = 7,ncol=dim(X)[2])
 
 for ( i in 1 :7){
   
   Xtest10_NSP=scale10*colSums(X[Tcompvec10_NSP[i,],]);
   Tcost_vals10_NSP[i,]=calc_qalyv2(Xtest10_NSP,costsof[i],discount_rate)
   Xtest11_NSP=scale10*colSums(X[Tcompvec11_NSP[i,],]);
   Tcost_vals11_NSP[i,]=calc_qalyv2(Xtest11_NSP,costsof[i],discount_rate)
   
   Xtest10_OST=scale10*colSums(X[Tcompvec10_OST[i,],]);
   Tcost_vals10_OST[i,]=calc_qalyv2(Xtest10_OST,costsof[i],discount_rate)
   Xtest11_OST=scale10*colSums(X[Tcompvec11_OST[i,],]);
   Tcost_vals11_OST[i,]=calc_qalyv2(Xtest11_OST,costsof[i],discount_rate)
   
   Xtest10_NSP_OST=scale10*colSums(X[Tcompvec10_NSP_OST[i,],]);
   Tcost_vals10_NSP_OST[i,]=calc_qalyv2(Xtest10_NSP_OST,costsof[i],discount_rate)
   Xtest11_NSP_OST=scale10*colSums(X[Tcompvec11_NSP_OST[i,],]);
   Tcost_vals11_NSP_OST[i,]=calc_qalyv2(Xtest11_NSP_OST,costsof[i],discount_rate)
 }
 stage_cost=stage_cost + colSums(Tcost_vals10_NSP) +colSums(Tcost_vals11_NSP)+
                         colSums(Tcost_vals10_OST) +colSums(Tcost_vals11_OST)+
                         colSums(Tcost_vals10_NSP_OST) +colSums(Tcost_vals11_NSP_OST)
 r_stage_cost=r_stage_cost + rowSums(Tcost_vals10_NSP) +rowSums(Tcost_vals11_NSP)+
                             rowSums(Tcost_vals10_OST) +rowSums(Tcost_vals11_OST)+
                             rowSums(Tcost_vals10_NSP_OST) +rowSums(Tcost_vals11_NSP_OST)
 # LT costs
 
 Tcompvec10= rbind(C$DC_comps_10,C$HCC_comps_10)
 Tcompvec00= rbind(C$DC_comps_00,C$HCC_comps_00)
 Tcompvec11= rbind(C$DC_comps_11,C$HCC_comps_11)
 Tcompvec01= rbind(C$DC_comps_01,C$HCC_comps_01)
 
 Tcost_vals00=matrix(data=0,nrow = 2,ncol=dim(X)[2])
 Tcost_vals01=matrix(data=0,nrow = 2,ncol=dim(X)[2])
 Tcost_vals10=matrix(data=0,nrow = 2,ncol=dim(X)[2])
 Tcost_vals11=matrix(data=0,nrow = 2,ncol=dim(X)[2])
 multy=c( rDCLT,rHCCLT)
 for ( i in 1 :2){
   Xtest00=multy[i]*scale00*colSums(X[Tcompvec00[i,],]);
   Tcost_vals00[i,]=calc_qalyv2(Xtest00,costsof[9],discount_rate)
   Xtest01=multy[i]*scale00*colSums(X[Tcompvec01[i,],]);
   Tcost_vals01[i,]=calc_qalyv2(Xtest01,costsof[9],discount_rate)
   
   Xtest10=multy[i]*scale10*colSums(X[Tcompvec10[i,],]);
   Tcost_vals10[i,]=calc_qalyv2(Xtest10,costsof[9],discount_rate)
   Xtest11=multy[i]*scale10*colSums(X[Tcompvec11[i,],]);
   Tcost_vals11[i,]=calc_qalyv2(Xtest11,costsof[9],discount_rate)
 }
 
 liverT_cost=colSums(Tcost_vals00) + colSums(Tcost_vals01) +colSums(Tcost_vals10) +colSums(Tcost_vals11) 
 r_liverT_cost=rowSums(Tcost_vals00) + rowSums(Tcost_vals01) +rowSums(Tcost_vals10) +rowSums(Tcost_vals11) 
 
 # LT costs for NEP, OST and both
 
 Tcompvec10_NSP=Tcompvec10 + 360
 Tcompvec11_NSP=Tcompvec11 + 360
 Tcompvec10_OST=Tcompvec10 + 2*360
 Tcompvec11_OST=Tcompvec11 + 2*360
 Tcompvec10_NSP_OST=Tcompvec10 + 3*360
 Tcompvec11_NSP_OST=Tcompvec11 + 3*360
 
 Tcost_vals10_NSP=matrix(data=0,nrow = 2,ncol=dim(X)[2])
 Tcost_vals11_NSP=matrix(data=0,nrow = 2,ncol=dim(X)[2])
 Tcost_vals10_OST=matrix(data=0,nrow = 2,ncol=dim(X)[2])
 Tcost_vals11_OST=matrix(data=0,nrow = 2,ncol=dim(X)[2])
 Tcost_vals10_NSP_OST=matrix(data=0,nrow = 2,ncol=dim(X)[2])
 Tcost_vals11_NSP_OST=matrix(data=0,nrow = 2,ncol=dim(X)[2])
 
 for ( i in 1 :2){

   Xtest10_NSP=multy[i]*scale10*colSums(X[Tcompvec10_NSP[i,],]);
   Tcost_vals10_NSP[i,]=calc_qalyv2(Xtest10_NSP,costsof[9],discount_rate)
   Xtest11_NSP=multy[i]*scale10*colSums(X[Tcompvec11_NSP[i,],]);
   Tcost_vals11_NSP[i,]=calc_qalyv2(Xtest11_NSP,costsof[9],discount_rate)
   
   Xtest10_OST=multy[i]*scale10*colSums(X[Tcompvec10_OST[i,],]);
   Tcost_vals10_OST[i,]=calc_qalyv2(Xtest10_OST,costsof[9],discount_rate)
   Xtest11_OST=multy[i]*scale10*colSums(X[Tcompvec11_OST[i,],]);
   Tcost_vals11_OST[i,]=calc_qalyv2(Xtest11_OST,costsof[9],discount_rate)
   
   Xtest10_NSP_OST=multy[i]*scale10*colSums(X[Tcompvec10_NSP_OST[i,],]);
   Tcost_vals10_NSP_OST[i,]=calc_qalyv2(Xtest10_NSP_OST,costsof[9],discount_rate)
   Xtest11_NSP_OST=multy[i]*scale10*colSums(X[Tcompvec11_NSP_OST[i,],]);
   Tcost_vals11_NSP_OST[i,]=calc_qalyv2(Xtest11_NSP_OST,costsof[9],discount_rate)
 }
 liverT_cost = liverT_cost + colSums(Tcost_vals10_NSP) +colSums(Tcost_vals11_NSP) +
                             colSums(Tcost_vals10_OST) +colSums(Tcost_vals11_OST) +
                             colSums(Tcost_vals10_NSP_OST) +colSums(Tcost_vals11_NSP_OST)
 r_liverT_cost = r_liverT_cost + rowSums(Tcost_vals10_NSP) +rowSums(Tcost_vals11_NSP) +
                                 rowSums(Tcost_vals10_OST) +rowSums(Tcost_vals11_OST) +
                                 rowSums(Tcost_vals10_NSP_OST) +rowSums(Tcost_vals11_NSP_OST) 
 # treat data
 
 Tdata=pickupDAA()
 treatT=number_of_treats*Tdata$DAA_cost
 if (abs(treatduration-6.5)<1e-05){ # treatduration is rate per year 52/8 or 52/12
   Vnum = Tdata$week8_numvisits
 } else {
   Vnum = Tdata$week12_numvisits
 }
 treatT=treatT+(Vnum*Tdata$clinic_cost+Vnum*Tdata$hcv_rna_test)*number_of_treats
 
 costs = stage_cost + liverT_cost + treatT
 
 r_costs = c(0,0,r_stage_cost,sum(r_liverT_cost),sum(treatT)) # r_liverT_cost and treatT are over years so sum
 r_df=data.frame(STAGE=c("Susceptible","Acute","F0","F1","F2","F3","F4","DC","HCC","LT","SVR"),COST=r_costs,QALYs=r_qaly, stringsAsFactors = FALSE)
 #row.names(r_df)=c("Spont Clear","Acute","F0","F1","F2","F3","F4","DC","HCC","LT","SVR")
 return(list(COST=costs,QALYS=qaly,stage_vals = r_df))
  
}
getdata=function (){
  conn_cost <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = 'www/HCV.db'
  )
  
  strsql="select Parameter,value from parameter_data where Variable=='Annual costs' order by vnum"
  costs=dbGetQuery(conn_cost, strsql)
  costsof=as.data.frame(t(costs$Value))
  names(costsof)=costs$Parameter
  
  strsql="select Parameter,value from parameter_data where Variable=='Health utilities' order by vnum"
  utils=dbGetQuery(conn_cost, strsql)
  utilsof=as.data.frame(t(utils$Value))
  names(utilsof)=utils$Parameter
  
  dbDisconnect(conn_cost)
  return(list(costsof=costsof,utilsof=utilsof))
}
calc_qalyv2=function (X,q_weight,discount_rate){
  
  
  # X is the vector of person years over time for a comparrtment
  # discount rate is the discounting for future qalys
  # q_weight is the utility rate for the stage
  
  
  if (discount_rate>=1){
    discount_rate = discount_rate/100
  }
  r = 1 - discount_rate
  qaly = 0
  k = 0
  qualy=rep(0,length(X))
  for (i in 1:length(X)){
    I = X[i]
    qaly[i]=I*1/((1+discount_rate)^k)
    k = k + 1
  }
  qaly=q_weight*qaly
  return(qaly)
}

pickuprates=function(){
  conn_param <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = 'www/HCV.db'
  )
  strsql="
  select value,'p1' as 'val'  from parameter_data where Parameter=='Spontaneous clearance'
  UNION
  select value,'p2' as 'val'  from parameter_data where Parameter=='DC to LT'
  UNION
  select value,'p3' as 'val'  from parameter_data where Parameter=='HCC to LT'
  order by val"
  params=dbGetQuery(conn_param, strsql)
  dbDisconnect(conn_param)
  return(list(delta=params$Value[1],rDCLT=-log(1-params$Value[2]),rHCCLT=-log(1-params$Value[3])))
}

pickupDAA=function(){
  conn_param <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = 'www/HCV.db'
  )
  strsql="
  select value,'p1' as 'val'  from parameter_data where Parameter=='Hepatologist visits per 8-week regimen'
  UNION
  select value,'p2' as 'val'  from parameter_data where Parameter=='Hepatologist visits per 12-week regimen'
  UNION
  select value,'p3' as 'val'  from parameter_data where Parameter=='HCV RNA test'
  UNION
  select value,'p4' as 'val'  from parameter_data where Parameter=='Clinic cost visit'
  UNION
  select value,'p5' as 'val'  from parameter_data where Parameter=='DAA treatment cost'
  order by val"
  params=dbGetQuery(conn_param, strsql)
  dbDisconnect(conn_param)
  return(list(week8_numvisits=params$Value[1],week12_numvisits=params$Value[2],hcv_rna_test=params$Value[3],
              clinic_cost=params$Value[4],DAA_cost=params$Value[5]))
}
