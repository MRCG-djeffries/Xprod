cost_qaly_run_genpop=function(X,scale00,scale10,discount_rate){
  # X is the compartments * time output from compartment model
  # columns assumed to be relevant time period in years
  # delta is the spontaneous recovery percentage
  # rDCLT is the rate form DC to LT
  # rHCCLT is the rate form HCC to LT  
  # This is just for the genpop hence only copartments _00
  # 1 to 180 is gen pop
  # 181 to 360 is the screened 
 
  dum=pickuprates()
  delta=dum$delta
  rDCLT=dum$rDCLT
  rHCCLT=dum$rHCCLT
  C=coeffsof_v5()
  L=getdata()
  costsof=L$costsof # annual cost data
  utilsof=L$utilsof # qaly data
  
  # Calculate qalys - health of prevalence snapshot
  # Acute spontaneous recovery
  # YSPONT=delta*(colSums(scale00*X[C$A_comps_00,])+colSums(scale10*X[C$A_comps_10,]))
  # qaly=calc_qalyv2(3.5,YSPONT,utilsof$`Spontaneous viral clearance, never infected`,3)
  # YA=(1-delta)*(colSums(scale00*X[C$A_comps_00,])+colSums(scale10*X[C$A_comps_10,]))
  # qaly=qaly+calc_qalyv2(3.5,YA,utilsof$`Acute HCV`,3)
  r_qaly=rep(0,11) # These are the stage values
  r_stage_cost=rep(0,11)
  YS0=(colSums(scale00*X[C$S0_comps_00,]))
  qaly=calc_qalyv2(YS0,utilsof$`Spontaneous viral clearance, never infected`,discount_rate)
  r_qaly[1]=sum(qaly)
  YA=(colSums(scale00*X[C$A_comps_00,]))
  dum=calc_qalyv2(YA,utilsof$`Acute HCV`,discount_rate)
  r_qaly[2]=sum(dum)
  qaly=qaly+dum
  YF0=(colSums(scale00*X[C$F0_comps_00,]))
  dum=calc_qalyv2(YF0,utilsof$`Mild chronic HCV (F0/F1/F2)`,discount_rate)
  r_qaly[3]=sum(dum)
  qaly=qaly+dum
  YF1=(colSums(scale00*X[C$F1_comps_00,]))
  dum=calc_qalyv2(YF1,utilsof$`Mild chronic HCV (F0/F1/F2)`,discount_rate)
  r_qaly[4]=sum(dum)
  qaly=qaly+dum
  YF2=(colSums(scale00*X[C$F2_comps_00,]))
  dum=calc_qalyv2(YF2,utilsof$`Mild chronic HCV (F0/F1/F2)`,discount_rate)
  r_qaly[5]=sum(dum)
  qaly=qaly+dum
  YF3=(colSums(scale00*X[C$F3_comps_00,]))
  dum=calc_qalyv2(YF3,utilsof$`Moderate chronic HCV (F3)`,discount_rate)
  r_qaly[6]=sum(dum)
  qaly=qaly+dum
  YF4=(colSums(scale00*X[C$F4_comps_00,]))
  dum=calc_qalyv2(YF4,utilsof$`Compensated cirrhosis (F4)`,discount_rate)
  r_qaly[7]=sum(dum)
  qaly=qaly+dum
  YDC=(colSums(scale00*X[C$DC_comps_00,]))
  dum=calc_qalyv2(YDC,utilsof$`Decompensated cirrhosis/liver failure`,discount_rate)
  r_qaly[8]=sum(dum)
  qaly=qaly+dum
  YHCC=(colSums(scale00*X[C$HCC_comps_00,]))
  dum=calc_qalyv2(YHCC,utilsof$`Hepatocellular carcinoma`,discount_rate)
  r_qaly[9]=sum(dum)
  qaly=qaly+dum
  YLT1=(colSums(scale00*X[C$LT1_comps_00,]))
  dum1=calc_qalyv2(YLT1,utilsof$`Liver transplantation year 1`,discount_rate)
  qaly=qaly+dum1
  YLT2=(colSums(scale00*X[C$LT2_comps_00,]))
  dum2=calc_qalyv2(YLT2,utilsof$`Liver transplantation year 2+`,discount_rate)
  r_qaly[10]=sum(dum1+dum2)
  qaly=qaly+dum2  

  # costs
  costs = calc_qalyv2(YF0,costsof$F0,discount_rate)
  r_stage_cost[3]=sum(costs)
  dum=calc_qalyv2(YF1,costsof$F1,discount_rate)
  r_stage_cost[4]=sum(dum)
  costs = costs + dum
  dum=calc_qalyv2(YF2,costsof$F2,discount_rate)
  r_stage_cost[5]=sum(dum)
  costs = costs + dum 
  dum=calc_qalyv2(YF3,costsof$F3,discount_rate)
  r_stage_cost[6]=sum(dum)
  costs = costs + dum
  dum=calc_qalyv2(YF4,costsof$F4,discount_rate)
  r_stage_cost[7]=sum(dum)
  costs = costs + dum
  dum=calc_qalyv2(YDC,costsof$DC,discount_rate)
  r_stage_cost[8]=sum(dum)
  costs = costs + dum
  dum=calc_qalyv2(YHCC,costsof$HCC,discount_rate)
  r_stage_cost[9]=sum(dum)
  costs = costs + dum
  dum1=calc_qalyv2(rDCLT*YDC,costsof$`Liver transplant`,discount_rate)
  dum2=calc_qalyv2(rHCCLT*YHCC,costsof$`Liver transplant`,discount_rate)
  r_stage_cost[10]=sum(dum1+dum2)
  costs = costs + dum1
  costs = costs + dum2
  r_df=data.frame(STAGE=c("Susceptible","Acute","F0","F1","F2","F3","F4","DC","HCC","LT","SVR"),COST=r_stage_cost,QALYs=r_qaly, stringsAsFactors = FALSE)
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
