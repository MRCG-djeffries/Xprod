# Library in packages used in this application
library(shiny)
library(DT)
library(RSQLite)
library(DBI)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(lubridate)
library(shinyFeedback)
library(dplyr)
library(dbplyr)
library(shinydashboard) 
library(flextable)
library(magrittr)
library(scales)

# source('R/fit_module.R')
# source('R/fit_to_data_module.R')
# source('R/runmodel.R')
# source('R/runmodel_intervention.R')
# source('R/DAA_intervention_module.R')
# source('R/fit_interventionUS.R')
# source('R/runmodel_US_sim.R')
# source('R/run_gen_pop_v4.R')
# source('R/runmodel_nep_v2_with_treat_long_v3.R')
# source('R/runmodel_v4.R')
conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = 'www/HCV.db'
)

shiny::onStop(function() {
  dbDisconnect(conn)
})




# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)
options(spinner.type = 8)

# Create 'names_map' dataframe to convert variable names ('names') to clean
# column names ('display_names') in table (i.e. capitalized words, spaces, etc.)
names_map <- data.frame(
  names = c('Vnum', 'Variable', 'Stratum', 'Parameter', 'Units', 'Type', 'Value', 'Ref'),
  display_names = c('Vnum', 'Variable', 'Stratum', 'Parameter', 'Units', 'Type', 'Value', 'Ref'),
  stringsAsFactors = FALSE
)




coeffsof_v5=function (){
  # compnames - root name sof the 20 compartments
  # chronic_nams - names of ij ordering
  # chronic_numsij - compartment index for all chronically infected with HCV
  
  
  # Gives the index numbers for categories
  # There are 20*9*4 compartments
  # 20 represents the X notation
  # element 1 to 5 S0,S1,S2,S3,S4
  # infection na?ve or previously achieving spontaneous clearance or SVR through treatment from liver fibrosis stage F0 to F4 respectively
  # elemment 6 is A acute stage
  # element 7 to 11 F0,F1,F2,F3,F4 - the fibrosis stages, chronic
  # element 12 DC stage, chronic
  # element 13 HCC stage, chronic
  # element 14 and 15 LT1 and LT2 - liver transplant stages, chronic
  # element 16 to 20 T0 to T4 
  # chronically infected and in treatment achieving sustained viral response (SVR) (T0 to T4-treated from liver fibrosis stage F0 to F4 respectively)
  compnames=c('S0','S1','S2','S3','S4','A','F0','F1','F2','F3','F4','DC','HCC','LT1','LT2', 'T0','T1','T2','T3','T4')
  chronic_nams=c('00=former,never','01=former,failed','10=current,never','11=current,failed')
  
  # each elemet has three subscripts i,j,k
  # i = 0 formwerPWID, i = 1 current PWID
  # j = 0 never failed treatment, j = 1 had treatment failure
  # k = 1 to 9 the 9 age groups
  
  #for the 20*9*4 compartments
  # ordering is 
  # (i=0,j=0,k = 1);(i=0,j=0,k = 2);...;(i=0,j=0,k = 9) former, never failed
  # (i=0,j=1,k = 1);(i=0,j=0,k = 2);...;(i=0,j=0,k = 9) former, failed
  # (i=1,j=0,k = 1);(i=0,j=0,k = 2);...;(i=0,j=0,k = 9) current, never failed
  # (i=1,j=1,k = 1);(i=0,j=0,k = 2);...;(i=0,j=0,k = 9) current, failed
  
  chronic_numsA =7:15
  chronic_nums00 = chronic_numsA
  chronic_nums01 = 180+chronic_numsA
  chronic_nums10 = 360+chronic_numsA
  chronic_nums11 = 540+chronic_numsA
  
  for (i in 2 : 9){
    chronic_nums00 = c(chronic_nums00 ,20*(i-1)+chronic_numsA)
    chronic_nums01 = c(chronic_nums01 ,180+20*(i-1)+chronic_numsA)
    chronic_nums10 = c(chronic_nums10 ,360+20*(i-1)+chronic_numsA)
    chronic_nums11 = c(chronic_nums11 ,540+20*(i-1)+chronic_numsA)
  }
  
  A_comps_00=seq(6,180,20)
  A_comps_01=A_comps_00+180
  A_comps_10=A_comps_00+360
  A_comps_11=A_comps_00+540
  A_comps=c(A_comps_00,A_comps_01,A_comps_10,A_comps_11)
  
  F0_comps_00 = seq(7,180,20)
  F0_comps_01 = F0_comps_00+180
  F0_comps_10 = F0_comps_00+360
  F0_comps_11 = F0_comps_00+540
  F0_comps=c(F0_comps_00,F0_comps_01,F0_comps_10,F0_comps_11)
  
  F1_comps_00 = seq(8,180,20)
  F1_comps_01 = F1_comps_00+180
  F1_comps_10 = F1_comps_00+360
  F1_comps_11 = F1_comps_00+540
  F1_comps=c(F1_comps_00,F1_comps_01,F1_comps_10,F1_comps_11)
  
  F2_comps_00 = seq(9,180,20)
  F2_comps_01 = F2_comps_00+180
  F2_comps_10 = F2_comps_00+360
  F2_comps_11 = F2_comps_00+540
  F2_comps=c(F2_comps_00,F2_comps_01,F2_comps_10,F2_comps_11)
  
  F3_comps_00 = seq(10,180,20)
  F3_comps_01 = F3_comps_00+180
  F3_comps_10 = F3_comps_00+360
  F3_comps_11 = F3_comps_00+540
  F3_comps=c(F3_comps_00,F3_comps_01,F3_comps_10,F3_comps_11)
  
  F4_comps_00 = seq(11,180,20)
  F4_comps_01 = F4_comps_00+180
  F4_comps_10 = F4_comps_00+360
  F4_comps_11 = F4_comps_00+540
  F4_comps=c(F4_comps_00,F4_comps_01,F4_comps_10,F4_comps_11)
  
  DC_comps_00 = seq(12,180,20)
  DC_comps_01 = DC_comps_00+180
  DC_comps_10 = DC_comps_00+360
  DC_comps_11 = DC_comps_00+540
  DC_comps=c(DC_comps_00,DC_comps_01,DC_comps_10,DC_comps_11)
  
  HCC_comps_00 = seq(13,180,20)
  HCC_comps_01 = HCC_comps_00+180
  HCC_comps_10 = HCC_comps_00+360
  HCC_comps_11 = HCC_comps_00+540
  HCC_comps=c(HCC_comps_00,HCC_comps_01,HCC_comps_10,HCC_comps_11)
  
  LT1_comps_00 = seq(14,180,20)
  LT1_comps_01 = LT1_comps_00+180
  LT1_comps_10 = LT1_comps_00+360
  LT1_comps_11 = LT1_comps_00+540
  LT1_comps=c(LT1_comps_00,LT1_comps_01,LT1_comps_10,LT1_comps_11)
  
  LT2_comps_00 = seq(15,180,20)
  LT2_comps_01 = LT2_comps_00+180
  LT2_comps_10 = LT2_comps_00+360
  LT2_comps_11 = LT2_comps_00+540
  LT2_comps=c(LT2_comps_00,LT2_comps_01,LT2_comps_10,LT2_comps_11)
  
  
  S0_comps_00 = seq(1,180,20)
  S0_comps_01 = S0_comps_00+180
  S0_comps_10 = S0_comps_00+360
  S0_comps_11 = S0_comps_00+540
  S0_comps=c(S0_comps_00,S0_comps_01,S0_comps_10,S0_comps_11)
  
  S1_comps_00 = seq(2,180,20)
  S1_comps_01 = S1_comps_00+180
  S1_comps_10 = S1_comps_00+360
  S1_comps_11 = S1_comps_00+540
  S1_comps=c(S1_comps_00,S1_comps_01,S1_comps_10,S1_comps_11)
  
  S2_comps_00 = seq(3,180,20)
  S2_comps_01 = S2_comps_00+180
  S2_comps_10 = S2_comps_00+360
  S2_comps_11 = S2_comps_00+540
  S2_comps=c(S2_comps_00,S2_comps_01,S2_comps_10,S2_comps_11)
  
  S3_comps_00 = seq(4,180,20)
  S3_comps_01 = S3_comps_00+180
  S3_comps_10 = S3_comps_00+360
  S3_comps_11 = S3_comps_00+540
  S3_comps=c(S3_comps_00,S3_comps_01,S3_comps_10,S3_comps_11)
  
  S4_comps_00 = seq(5,180,20)
  S4_comps_01 = S4_comps_00+180
  S4_comps_10 = S4_comps_00+360
  S4_comps_11 = S4_comps_00+540
  S4_comps=c(S4_comps_00,S4_comps_01,S4_comps_10,S4_comps_11)
  
  T0_comps_00 = seq(16,180,20)
  T0_comps_01 = T0_comps_00+180
  T0_comps_10 = T0_comps_00+360
  T0_comps_11 = T0_comps_00+540
  T0_comps=c(T0_comps_00,T0_comps_01,T0_comps_10,T0_comps_11)
  
  T1_comps_00 = seq(17,180,20)
  T1_comps_01 = T1_comps_00+180
  T1_comps_10 = T1_comps_00+360
  T1_comps_11 = T1_comps_00+540
  T1_comps=c(T1_comps_00,T1_comps_01,T1_comps_10,T1_comps_11)
  
  T2_comps_00 = seq(18,180,20)
  T2_comps_01 = T2_comps_00+180
  T2_comps_10 = T2_comps_00+360
  T2_comps_11 = T2_comps_00+540
  T2_comps=c(T2_comps_00,T2_comps_01,T2_comps_10,T2_comps_11)
  
  T3_comps_00 = seq(19,180,20)
  T3_comps_01 = T3_comps_00+180
  T3_comps_10 = T3_comps_00+360
  T3_comps_11 = T3_comps_00+540
  T3_comps=c(T3_comps_00,T3_comps_01,T3_comps_10,T3_comps_11)
  
  T4_comps_00 = seq(20,180,20)
  T4_comps_01 = T4_comps_00+180
  T4_comps_10 = T4_comps_00+360
  T4_comps_11 = T4_comps_00+540
  T4_comps=c(T4_comps_00,T4_comps_01,T4_comps_10,T4_comps_11)
  # S0, A,F0,F1,F2,F3,F4,DC,HCC,LT1,LT2 - all other comps are empty
  agemat_current=rbind(  # current never failed
    c(361, 366:375),
    c(381 ,386:395),
    c(401 ,406:415),
    c(421 ,426:435),
    c(441 ,446:455),
    c(461 ,466:475),
    c(481 ,486:495),
    c(501 ,506:515),
    c(521 ,526:535))
  
  agemat_former=rbind( # former never failed
    c(361, 366:375),
    c(381 ,386:395),
    c(401 ,406:415),
    c(421 ,426:435),
    c(441 ,446:455),
    c(461 ,466:475),
    c(481 ,486:495),
    c(501 ,506:515),
    c(521 ,526:535))-360
  
  
  return(list(comps=compnames,states=chronic_nams,
              chronic00=chronic_nums00,chronic01=chronic_nums01,chronic10=chronic_nums10,chronic11=chronic_nums11,
              A_comps=A_comps,A_comps_00=A_comps_00,A_comps_01=A_comps_01,A_comps_10=A_comps_10,A_comps_11=A_comps_11,
              F0_comps=F0_comps,F0_comps_00=F0_comps_00,F0_comps_01=F0_comps_01,F0_comps_10=F0_comps_10,F0_comps_11=F0_comps_11,
              F1_comps=F1_comps,F1_comps_00=F1_comps_00,F1_comps_01=F1_comps_01,F1_comps_10=F1_comps_10,F1_comps_11=F1_comps_11,
              F2_comps=F2_comps,F2_comps_00=F2_comps_00,F2_comps_01=F2_comps_01,F2_comps_10=F2_comps_10,F2_comps_11=F2_comps_11,
              F3_comps=F3_comps,F3_comps_00=F3_comps_00,F3_comps_01=F3_comps_01,F3_comps_10=F3_comps_10,F3_comps_11=F3_comps_11,
              F4_comps=F4_comps,F4_comps_00=F4_comps_00,F4_comps_01=F4_comps_01,F4_comps_10=F4_comps_10,F4_comps_11=F4_comps_11,
              DC_comps=DC_comps,DC_comps_00=DC_comps_00,DC_comps_01=DC_comps_01,DC_comps_10=DC_comps_10,DC_comps_11=DC_comps_11,
              HCC_comps=HCC_comps,HCC_comps_00=HCC_comps_00,HCC_comps_01=HCC_comps_01,HCC_comps_10=HCC_comps_10,HCC_comps_11=HCC_comps_11,
              LT1_comps=LT1_comps,LT1_comps_00=LT1_comps_00,LT1_comps_01=LT1_comps_01,LT1_comps_10=LT1_comps_10,LT1_comps_11=LT1_comps_11,
              LT2_comps=LT2_comps,LT2_comps_00=LT2_comps_00,LT2_comps_01=LT2_comps_01,LT2_comps_10=LT2_comps_10,LT2_comps_11=LT2_comps_11,
              S0_comps=S0_comps,S0_comps_00=S0_comps_00,S0_comps_01=S0_comps_01,S0_comps_10=S0_comps_10,S0_comps_11=S0_comps_11,
              S1_comps=S1_comps,S1_comps_00=S1_comps_00,S1_comps_01=S1_comps_01,S1_comps_10=S1_comps_10,S1_comps_11=S1_comps_11,
              S2_comps=S2_comps,S2_comps_00=S2_comps_00,S2_comps_01=S2_comps_01,S2_comps_10=S2_comps_10,S2_comps_11=S2_comps_11,
              S3_comps=S3_comps,S3_comps_00=S3_comps_00,S3_comps_01=S3_comps_01,S3_comps_10=S3_comps_10,S3_comps_11=S3_comps_11,
              S4_comps=S4_comps,S4_comps_00=S4_comps_00,S4_comps_01=S4_comps_01,S4_comps_10=S4_comps_10,S4_comps_11=S4_comps_11,
              T0_comps=T0_comps,T0_comps_00=T0_comps_00,T0_comps_01=T0_comps_01,T0_comps_10=T0_comps_10,T0_comps_11=T0_comps_11,
              T1_comps=T1_comps,T1_comps_00=T1_comps_00,T1_comps_01=T1_comps_01,T1_comps_10=T1_comps_10,T1_comps_11=T1_comps_11,
              T2_comps=T2_comps,T2_comps_00=T2_comps_00,T2_comps_01=T2_comps_01,T2_comps_10=T2_comps_10,T2_comps_11=T2_comps_11,
              T3_comps=T3_comps,T3_comps_00=T3_comps_00,T3_comps_01=T3_comps_01,T3_comps_10=T3_comps_10,T3_comps_11=T3_comps_11,
              T4_comps=T4_comps,T4_comps_00=T4_comps_00,T4_comps_01=T4_comps_01,T4_comps_10=T4_comps_10,T4_comps_11=T4_comps_11,
              agemat_current=agemat_current,agemat_former=agemat_former))
}

age_weighted=function(X,agemat_current,agemat_former){
  # X is the compartments matrix 720 *times
  # agemat_current and agemat_former
  # these are 9 by 11 matrices
  # rows are age group compartment numbers
  # cols are S0,A,F0,F1,F2,F3,F4,DC,HCC,LT1,LT2
  # Assumes 25% of current are in first age group in year 66 (i.e. 2015 assuming 1950:2031 for model)
  # current
  bot = colSums(X[361:540,]); # currently infected
  age_weights_current=rep(9,1)
  for (i in 1 : 9){
    agetot=colSums(X[agemat_current[i,],])
    if (i == 1){
      age_weights_current[i] = 0.25*bot[66]/agetot[66]
    }else{
      age_weights_current[i] = (0.75/8)*bot[66]/agetot[66]
    }
  }
  # former
  bot = colSums(X[1:360,]); # currently infected
  age_weights_former=rep(9,1)
  for (i in 1 : 9){
    agetot=colSums(X[agemat_former[i,],])
    if (i == 1){
      age_weights_former[i] = 0.125*bot[66]/agetot[66]
    }else{
      age_weights_former[i] = (0.875/8)*bot[66]/agetot[66]
    }
  }
  return(list(age_weights_current=age_weights_current,age_weights_former=age_weights_former))
  
  
}
getallpop=function(X,L,age_weights_current,age_weights_former,nt,age_scale00,age_scale10){
  #X is the 720 * 82 matrix
  #L is the list of all the compartment index
  Aformer = matrix(rep(age_weights_former,nt),nrow=9,ncol=nt)
  Apwid = matrix(rep(age_weights_current,nt),nrow=9,ncol=nt)
  with(L,{
    lt2=age_scale00*colSums(Aformer*X[LT2_comps_00,])+age_scale10*colSums(Apwid*X[LT2_comps_10,])+
      age_scale00*colSums(Aformer*X[LT2_comps_01,])+age_scale10*colSums(Apwid*X[LT2_comps_11,])
    lt1=age_scale00*colSums(Aformer*X[LT1_comps_00,])+age_scale10*colSums(Apwid*X[LT1_comps_10,])+
      age_scale00*colSums(Aformer*X[LT1_comps_01,])+age_scale10*colSums(Apwid*X[LT1_comps_11,])
    lt=lt1+lt2;
    dc=age_scale00*colSums(Aformer*X[DC_comps_00,])+age_scale10*colSums(Apwid*X[DC_comps_10,])+
      age_scale00*colSums(Aformer*X[DC_comps_01,])+age_scale10*colSums(Apwid*X[DC_comps_11,])
    hcc=age_scale00*colSums(Aformer*X[HCC_comps_00,])+age_scale10*colSums(Apwid*X[HCC_comps_10,])+
      age_scale00*colSums(Aformer*X[HCC_comps_01,])+age_scale10*colSums(Apwid*X[HCC_comps_11,])
    f4=age_scale00*colSums(Aformer*X[F4_comps_00,])+age_scale10*colSums(Apwid*X[F4_comps_10,])+
      age_scale00*colSums(Aformer*X[F4_comps_01,])+age_scale10*colSums(Apwid*X[F4_comps_11,])
    f3=age_scale00*colSums(Aformer*X[F3_comps_00,])+age_scale10*colSums(Apwid*X[F3_comps_10,])+
      age_scale00*colSums(Aformer*X[F3_comps_01,])+age_scale10*colSums(Apwid*X[F3_comps_11,])
    f2=age_scale00*colSums(Aformer*X[F2_comps_00,])+age_scale10*colSums(Apwid*X[F2_comps_10,])+
      age_scale00*colSums(Aformer*X[F2_comps_01,])+age_scale10*colSums(Apwid*X[F2_comps_11,])
    f1=age_scale00*colSums(Aformer*X[F1_comps_00,])+age_scale10*colSums(Apwid*X[F1_comps_10,])+
      age_scale00*colSums(Aformer*X[F1_comps_01,])+age_scale10*colSums(Apwid*X[F1_comps_11,])
    f0=age_scale00*colSums(Aformer*X[F0_comps_00,])+age_scale10*colSums(Apwid*X[F0_comps_10,])+
      age_scale00*colSums(Aformer*X[F0_comps_01,])+age_scale10*colSums(Apwid*X[F0_comps_11,])
    
    # s compartments
    s4=age_scale00*colSums(Aformer*X[S4_comps_00,])+age_scale10*colSums(Apwid*X[S4_comps_10,])+
      age_scale00*colSums(Aformer*X[S4_comps_01,])+age_scale10*colSums(Apwid*X[S4_comps_11,])
    s3=age_scale00*colSums(Aformer*X[S3_comps_00,])+age_scale10*colSums(Apwid*X[S3_comps_10,])+
      age_scale00*colSums(Aformer*X[S3_comps_01,])+age_scale10*colSums(Apwid*X[S3_comps_11,])
    s2=age_scale00*colSums(Aformer*X[S2_comps_00,])+age_scale10*colSums(Apwid*X[S2_comps_10,])+
      age_scale00*colSums(Aformer*X[S2_comps_01,])+age_scale10*colSums(Apwid*X[S2_comps_11,])
    s1=age_scale00*colSums(Aformer*X[S1_comps_00,])+age_scale10*colSums(Apwid*X[S1_comps_10,])+
      age_scale00*colSums(Aformer*X[S1_comps_01,])+age_scale10*colSums(Apwid*X[S1_comps_11,])
    s0=age_scale00*colSums(Aformer*X[S0_comps_00,])+age_scale10*colSums(Apwid*X[S0_comps_10,])+
      age_scale00*colSums(Aformer*X[S0_comps_01,])+age_scale10*colSums(Apwid*X[S0_comps_11,])
    a0=age_scale00*colSums(Aformer*X[A_comps_00,])+age_scale10*colSums(Apwid*X[A_comps_10,])+
      age_scale00*colSums(Aformer*X[A_comps_01,])+age_scale10*colSums(Apwid*X[A_comps_11,])
    
    t4=age_scale00*colSums(Aformer*X[T4_comps_00,])+age_scale10*colSums(Apwid*X[T4_comps_10,])+
      age_scale00*colSums(Aformer*X[T4_comps_01,])+age_scale10*colSums(Apwid*X[T4_comps_11,])
    t3=age_scale00*colSums(Aformer*X[T3_comps_00,])+age_scale10*colSums(Apwid*X[T3_comps_10,])+
      age_scale00*colSums(Aformer*X[T3_comps_01,])+age_scale10*colSums(Apwid*X[T3_comps_11,])
    t2=age_scale00*colSums(Aformer*X[T2_comps_00,])+age_scale10*colSums(Apwid*X[T2_comps_10,])+
      age_scale00*colSums(Aformer*X[T2_comps_01,])+age_scale10*colSums(Apwid*X[T2_comps_11,])
    t1=age_scale00*colSums(Aformer*X[T1_comps_00,])+age_scale10*colSums(Apwid*X[T1_comps_10,])+
      age_scale00*colSums(Aformer*X[T1_comps_01,])+age_scale10*colSums(Apwid*X[T1_comps_11,])
    t0=age_scale00*colSums(Aformer*X[T0_comps_00,])+age_scale10*colSums(Apwid*X[T0_comps_10,])+
      age_scale00*colSums(Aformer*X[T0_comps_01,])+age_scale10*colSums(Apwid*X[T0_comps_11,])
    
    Tformer=age_scale00*(colSums(Aformer*X[LT2_comps_00,])+colSums(Aformer*X[LT2_comps_01,])+
                           colSums(Aformer*X[LT1_comps_00,])+colSums(Aformer*X[LT1_comps_01,])+
                           colSums(Aformer*X[DC_comps_00,]) +colSums(Aformer*X[DC_comps_01,]) + 
                           colSums(Aformer*X[HCC_comps_00,])+colSums(Aformer*X[HCC_comps_01,])+
                           colSums(Aformer*X[F4_comps_00,]) +colSums(Aformer*X[F4_comps_01,])+
                           colSums(Aformer*X[F3_comps_00,]) +colSums(Aformer*X[F3_comps_01,])+
                           colSums(Aformer*X[F2_comps_00,]) +colSums(Aformer*X[F2_comps_01,])+
                           colSums(Aformer*X[F1_comps_00,]) +colSums(Aformer*X[F1_comps_01,])+
                           colSums(Aformer*X[F0_comps_00,]) +colSums(Aformer*X[F0_comps_01,])+ 
                           colSums(Aformer*X[S4_comps_00,]) +colSums(Aformer*X[S4_comps_01,])+
                           colSums(Aformer*X[S3_comps_00,]) +colSums(Aformer*X[S3_comps_01,])+
                           colSums(Aformer*X[S2_comps_00,]) +colSums(Aformer*X[S2_comps_01,])+
                           colSums(Aformer*X[S1_comps_00,]) +colSums(Aformer*X[S1_comps_01,])+
                           colSums(Aformer*X[S0_comps_00,]) +colSums(Aformer*X[S0_comps_01,])+ 
                           colSums(Aformer*X[T4_comps_00,]) +colSums(Aformer*X[T4_comps_01,])+
                           colSums(Aformer*X[T3_comps_00,]) +colSums(Aformer*X[T3_comps_01,])+
                           colSums(Aformer*X[T2_comps_00,]) +colSums(Aformer*X[T2_comps_01,])+
                           colSums(Aformer*X[T1_comps_00,]) +colSums(Aformer*X[T1_comps_01,])+
                           colSums(Aformer*X[T0_comps_00,]) +colSums(Aformer*X[T0_comps_01,])+  
                           colSums(Aformer*X[A_comps_00,]) +colSums(Aformer*X[A_comps_01,])                         
    )   
    
    Tcurrent=age_scale10*(colSums(Apwid*X[LT2_comps_10,])+colSums(Apwid*X[LT2_comps_11,])+
                            colSums(Apwid*X[LT1_comps_10,])+colSums(Apwid*X[LT1_comps_11,])+
                            colSums(Apwid*X[DC_comps_10,]) +colSums(Apwid*X[DC_comps_11,]) + 
                            colSums(Apwid*X[HCC_comps_10,])+colSums(Apwid*X[HCC_comps_11,])+
                            colSums(Apwid*X[F4_comps_10,]) +colSums(Apwid*X[F4_comps_11,])+
                            colSums(Apwid*X[F3_comps_10,]) +colSums(Apwid*X[F3_comps_11,])+
                            colSums(Apwid*X[F2_comps_10,]) +colSums(Apwid*X[F2_comps_11,])+
                            colSums(Apwid*X[F1_comps_10,]) +colSums(Apwid*X[F1_comps_11,])+
                            colSums(Apwid*X[F0_comps_10,]) +colSums(Apwid*X[F0_comps_11,])+ 
                            colSums(Apwid*X[S4_comps_10,]) +colSums(Apwid*X[S4_comps_11,])+
                            colSums(Apwid*X[S3_comps_10,]) +colSums(Apwid*X[S3_comps_11,])+
                            colSums(Apwid*X[S2_comps_10,]) +colSums(Apwid*X[S2_comps_11,])+
                            colSums(Apwid*X[S1_comps_10,]) +colSums(Apwid*X[S1_comps_11,])+
                            colSums(Apwid*X[S0_comps_10,]) +colSums(Apwid*X[S0_comps_11,])+ 
                            colSums(Apwid*X[T4_comps_10,]) +colSums(Apwid*X[T4_comps_11,])+
                            colSums(Apwid*X[T3_comps_10,]) +colSums(Apwid*X[T3_comps_11,])+
                            colSums(Apwid*X[T2_comps_10,]) +colSums(Apwid*X[T2_comps_11,])+
                            colSums(Apwid*X[T1_comps_10,]) +colSums(Apwid*X[T1_comps_11,])+
                            colSums(Apwid*X[T0_comps_10,]) +colSums(Apwid*X[T0_comps_11,])+  
                            colSums(Apwid*X[A_comps_10,]) +colSums(Apwid*X[A_comps_11,])                         
    )   
    
    
    
    return(list(lt=lt,dc=dc,hcc=hcc,s0=s0,s1=s1,s2=s2,s3=s3,s4=s4,a0=a0,f0=f0,f1=f1,f2=f2,f3=f3,f4=f4,
                t0=t0,t1=t1,t2=t2,t3=t3,t4=t4,Tformer=Tformer,Tcurrent=Tcurrent))
  })
  
}

getallpop_all=function(X,L,age_weights_current,age_weights_former,nt,age_scale00,age_scale10){
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

plot_allcomps=function(times,X,chronic_nums10){
  
  y=100*colSums(X[chronic_nums10,])/colSums(X[361:540,])
  df=data.frame(times,y)
  p1=ggplot(data=df, aes(x=times+1950, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Percentage") + ggtitle("% of current PWID with HCV from 1950 to 2030")+
    scale_y_continuous(breaks = c(seq(0,70,10)),labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),lim=c(0,70))+
    scale_x_continuous(breaks = c(seq(1950,2031,5)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p1)  
}##BFD5E3

plot_allcomps_v3=function(times,X,scale10,L){
  
  y=scale10*colSums(X[c(L$F0_comps_10,L$F1_comps_10,L$F2_comps_10,L$F3_comps_10,L$F4_comps_10),])
  df=data.frame(times,y)
  p1=ggplot(data=subset(df,times>=72), aes(x=times+1950, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Number of subjects") + ggtitle("F0-F4 current PWID with HCV from 2022 to 2040")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(2022,2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p1)  
}##BFD5E3

plot_attack_rate_v_treat=function(t1,L,X,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat,SCreening,background_diag){
  ya=scale10*colSums(X[L$chronic10,])+scale00*colSums(X[L$chronic00,])
  yb=scale10*colSums(XT[L$chronic10,])+scale00*colSums(XT[L$chronic00,])
  y=ya-yb
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    dum=treat_comps_pv4(N,scale00,scale10,t1[i],pcom,alpha,time_cut,
                        background_diag,DAAtreat,SCreening)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = dim(XT)[2]
  tdum=1950+t1[time_cut:tend]-1
  ydum=totaltestsF4[time_cut:tend]+totaltests_notF4[time_cut:tend]
  ydum=pmax(ydum,0.0001) # guard against no treats
  d=y[time_cut:tend]/cumsum(ydum)
  df= data.frame(x=tdum,y=d)
  # two scales in one diag (not always recommended, but I WANT that now)
  # some reading up: https://stackoverflow.com/questions/3099219/plot-with-2-y-axes-one-y-axis-on-the-left-and-another-y-axis-on-the-right/3101876#

  C=cumsum(c(y[time_cut:tend]))
  TT=cumsum(c(ydum))
  
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

plot_attack_rate_v_treat_all=function(t1,L,X,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat,SCreening,
                                      enroll_nep,enroll_ost,enroll_nep_ost){
  a=colSums(X[L$chronic00,])+colSums(X[L$chronic10,])
  b=colSums(XT[L$chronic00,])+colSums(XT[L$chronic10,])+
    colSums(XT[360+L$chronic10,])+colSums(XT[720+L$chronic10,])+colSums(XT[1080+L$chronic10,])
  jequalzero=abs(b-a)/a<0.001
  ya=scale10*colSums(X[L$chronic10,])+scale00*colSums(X[L$chronic00,])
  yb=scale10*colSums(XT[L$chronic10,])+scale00*colSums(XT[L$chronic00,])+
     scale10*(colSums(XT[360+L$chronic10,])+colSums(XT[720+L$chronic10,])+colSums(XT[1080+L$chronic10,]))
  y=ya-yb
  y[jequalzero]=0
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    dum=treat_comps_pv7(N,scale00,scale10,(t1[i]-1),pcom,alpha,time_cut,
                        DAAtreat,SCreening,
                        enroll_nep,enroll_ost,enroll_nep_ost)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = dim(XT)[2]
  tdum=1950+t1[time_cut:tend]-1
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


plot_allcomps_v2=function(times,X,chronic_nums10){
  
  y=100*colSums(X[chronic_nums10,])/colSums(X[361:540,])
  df=data.frame(times,y)
  p1=ggplot(data=subset(df,times>=72), aes(x=times+1950, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Percentage") + ggtitle("% of current PWID with HCV from 2022 to 2030")+
    scale_y_continuous(breaks = c(seq(0,70,10)),labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),lim=c(0,70))+
    scale_x_continuous(breaks = c(seq(2022,2030,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p1)  
}##BFD5E3

plot_allcomps_v2_all=function(times,X,chronic_nums10){
  newtop = colSums(X[chronic_nums10,])+
           colSums(X[360+chronic_nums10,])+colSums(X[720+chronic_nums10,])+colSums(X[1080+chronic_nums10,])
  newbot = colSums(X[361:540,])+
           colSums(X[721:900,])+colSums(X[1081:1260,])+colSums(X[1441:1620,])
  y=100*newtop/newbot
  df=data.frame(times,y)
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=subset(df,times>=72), aes(x=times+1950, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Percentage") + ggtitle("% of current PWID with HCV from 2022 to 2040")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2022,2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)  
}

plot_areacomps=function(times,C){
  y=c(C$f0,C$f1,C$f2,C$f3,C$f4,C$dc,C$hcc,C$lt)
  Stage=c(rep("F0",81),rep("F1",81),rep("F2",81),rep("F3",81),rep("F4",81),rep("DC",81),rep("HCC",81),rep("LT",81))
  t=rep(0:80,8)
  df=data.frame(y,t,Stage)
  df$Stage=factor(df$Stage,levels=c("F0","F1","F2","F3","F4","DC","HCC","LT"))
  p1=ggplot(data=df, aes(x=t+1950, y=y,fill=Stage)) + geom_area() +
    xlab("Year") + ylab("Population") + ggtitle("Base model - Number in chronic compartments")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), lim = c(0, 2500000),expand = c(0,0),breaks=seq(0,2500000,250000))+
    scale_x_continuous(breaks = c(seq(1950,2030,5)),expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p1)  
}

plot_areacomps_v2=function(times,C){
  y=c(C$f0,C$f1,C$f2,C$f3,C$f4,C$dc,C$hcc,C$lt)
  Stage=c(rep("F0",91),rep("F1",91),rep("F2",91),rep("F3",91),rep("F4",91),rep("DC",91),rep("HCC",91),rep("LT",91))
  t=rep(0:90,8)
  df=data.frame(y,t,Stage)
  maxy=max(pretty_breaks()(c(0, (C$f0+C$f1+C$f2+C$f3+C$f4+C$dc+C$hcc+C$lt))))
  vecy=pretty_breaks()(c(0, (C$f0+C$f1+C$f2+C$f3+C$f4+C$dc+C$hcc+C$lt)))
  df$Stage=factor(df$Stage,levels=c("F0","F1","F2","F3","F4","DC","HCC","LT"))
  p1=ggplot(data=subset(df,times>=72), aes(x=t+1950, y=y,fill=Stage)) + geom_area() +
    xlab("Year") + ylab("Population") + ggtitle("Intervention model - Number in chronic compartments")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(2022,2040,1)),expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p1)  
}

plot_twolines=function(times,fout1,fout2){
  
  y1=fout1
  y2=fout2
  df=data.frame(times,y1,y2)
  p=ggplot(data=subset(df,times>=71), aes(x=times+1950)) + geom_line(aes(y=y1,colour="baseline"),size=2)+
    geom_line(aes(y=y2,colour="intervention"),size=2)+
    scale_colour_manual("", 
                        breaks = c("baseline", "intervention"),
                        values = c("blue", "red")) +
    xlab("Year") + ylab("Number of people") + ggtitle("Number of current PWID with HCV from 2021 to 2030")+
    
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),lim=c(250000,750000))+
    scale_x_continuous(breaks = c(seq(2021,2030,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p)  
}

plot_output_treats=function(t1,p,XT,scale00,scale10,n,pcom,alpha,time_ofint_inODEunits,n0,n1){
  # XT is the output from the compartment model
  # t1 is the required times
  # plott1 is the time required for treatment
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
      N=XT[,i]
      if (p==0 |p==1){
        dum=treat_comps_pv2(p,N,scale00,scale10,n,t1[i],pcom,alpha,time_ofint_inODEunits)
      }else{
        dum=treat_comps_pv3(n0,n1,N,scale00,scale10,t1[i],pcom,alpha,time_ofint_inODEunits)
      }
      totaltestsF4[i]=sum(dum$total_T4)
      totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = dim(XT)[2]
  tdum=1950+t1[time_ofint_inODEunits:tend]-1
  ydum=totaltestsF4[time_ofint_inODEunits:tend]+totaltests_notF4[time_ofint_inODEunits:tend]
  df= data.frame(x=tdum,y=ydum)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of DAA") + ggtitle("Total number of DAA treatments")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(tdum[1],2030,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
  
  
}

plot_output_cases_averted=function(t1,L,X,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat,SCreening,background_diag){
  beforetrt=scale00*colSums(X[L$chronic00,])+scale10*colSums(X[L$chronic10,])
            #scale00*colSums(X[L$chronic01,])+scale10*colSums(X[L$chronic11,])
  aftertrt= scale00*colSums(XT[L$chronic00,])+scale10*colSums(XT[L$chronic10,])
            #scale00*colSums(XT[L$chronic01,])+scale10*colSums(XT[L$chronic11,])
  t=time_cut-2 # since starts at 1950
  tend = dim(XT)[2]-1
  # total number of treatments
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    dum=treat_comps_pv4(N,scale00,scale10,t1[i],pcom,alpha,time_cut,
                        background_diag,DAAtreat,SCreening)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = dim(XT)[2]
  tdum=1950+t1[time_cut:tend]-1
  ydum=totaltestsF4[time_cut:tend]+totaltests_notF4[time_cut:tend]
  ydum=pmax(ydum,0.0001) # guard against no treats
  d=cumsum(beforetrt[time_cut:tend]-aftertrt[time_cut:tend])/cumsum(ydum)
  #print(paste0("The final is ",rev(d)[1]))
  df= data.frame(x=tdum,y=d)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
  xlab("Year") + ylab("chronic cases averted") + ggtitle("Number of Chronic cases averted by DAA treatment")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
  scale_x_continuous(breaks = c(seq(tdum[1],2030,1)),expand=c(0,0))+
  theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_chronic_all=function(t1,L,XT,scale00,scale10,time_cut,leny){
  aftertrt= scale00*colSums(XT[L$chronic00,])+scale10*colSums(XT[L$chronic10,])+
    scale00*colSums(XT[L$chronic01,])+scale10*colSums(XT[L$chronic11,])+
    scale10*(colSums(XT[360+L$chronic10,])+colSums(XT[720+L$chronic10,])+colSums(XT[1080+L$chronic10,]))
  time_cut=time_cut-1 # since starts at 1950
  tend = dim(XT)[2]-1
  # total number of treatments
  if (leny==1){
    tend = time_cut+18
    tdum=1950+(time_cut:(tend))
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

plot_output_pwid_all=function(t1,L,XT,scale00,scale10,time_cut,leny){
  aftertrt= scale10*colSums(XT[361:720,])+scale10*(colSums(XT[721:1080,])+colSums(XT[1081:1440,])+colSums(XT[1441:1800,]))
  time_cut=time_cut-2 # since starts at 1950
  tend = dim(XT)[2]-1
  # total number of treatments
  if (leny==1){
    tend = time_cut+19
    tdum=1950+(time_cut:(tend))
    bt=tdum
  }else{
    tend=dim(XT)[2]-1
    tdum=1950+(time_cut:(tend))
    bt=c(2021,seq(2025,(1950+tend),5))
  } # starts at zero
  df=data.frame(tdum=tdum,aftertrt=aftertrt[time_cut:(tend)])
  maxy=max(pretty_breaks()(c(0, df$aftertrt)))
  vecy=pretty_breaks()(c(0, df$aftertrt))
  p1=ggplot(data=df, aes(x=tdum, y=aftertrt)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of subjects") + ggtitle("# of current PWID")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = bt,expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_cases_averted_all=function(t1,L,X,XT,scale00,scale10,pcom,alpha,time_cut,DAAtreat,SCreening,
                                       enroll_nep,enroll_ost,enroll_nep_ost){
  a=colSums(X[L$chronic00,])+colSums(X[L$chronic10,])
  b=colSums(XT[L$chronic00,])+colSums(XT[L$chronic10,])+
    colSums(XT[360+L$chronic10,])+colSums(XT[720+L$chronic10,])+colSums(XT[1080+L$chronic10,])
  jequalzero=abs(b-a)/a<0.001
  beforetrt=scale00*colSums(X[L$chronic00,])+scale10*colSums(X[L$chronic10,])
  aftertrt= scale00*colSums(XT[L$chronic00,])+scale10*colSums(XT[L$chronic10,])+
    scale10*(colSums(XT[360+L$chronic10,])+colSums(XT[720+L$chronic10,])+colSums(XT[1080+L$chronic10,]))
    #scale10*(colSums(XT[540+L$chronic10,])+colSums(XT[900+L$chronic10,])+colSums(XT[1260+L$chronic10,]))
    # second line  is the successful treatment in NEP/OST and both
  
  t=time_cut-2 # since starts at 1950
  tend = dim(XT)[2]-1
  # total number of treatments
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    dum=treat_comps_pv7(N,scale00,scale10,t1[i],pcom,alpha,time_cut,
                        DAAtreat,SCreening,
                        enroll_nep,enroll_ost,enroll_nep_ost)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = dim(XT)[2]
  tdum=1950+t1[time_cut:tend]-1
  ydum=totaltestsF4[time_cut:tend]+totaltests_notF4[time_cut:tend]
  ydum=pmax(ydum,0.0001) # guard against no treats
  jequalzero=jequalzero[time_cut:tend]
  topy=beforetrt[time_cut:tend]-aftertrt[time_cut:tend]
  topy[topy<0.1]=0
  topy[jequalzero]=0
  if (sum(ydum)<0.1){
    topy=rep(0,length(topy))
  }
  d=cumsum(topy)/cumsum(ydum)
  #print(paste0("The final is ",rev(d)[1]))
  df= data.frame(x=tdum,y=d)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("chronic cases averted") + ggtitle("Number of Chronic cases averted by DAA treatment")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(tdum[1],2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_treats_v2_all=function(t1,XT,scale00,scale10,pcom,alpha,time_ofint_inODEunits,DAAtreat,SCreening,
                                   enroll_nep,enroll_ost,enroll_nep_ost){
  # XT is the output from the compartment model
  # t1 is the required times
  # This is for all intervention model
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  totP=rep(0,length(t1))
  valy=0
  for (i in 1 : length(t1)){
    N=XT[,i]
    totP[i]= scale10*(sum(N[361:720])+sum(N[721:1080])+sum(N[1081:1440])+sum(N[1441:1800]))
    dum=treat_comps_pv7(N,scale00,scale10,t1[i],pcom,alpha,time_ofint_inODEunits,
                        DAAtreat,SCreening,
                        enroll_nep,enroll_ost,enroll_nep_ost)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = dim(XT)[2]
  tdum=1950+t1[time_ofint_inODEunits:tend]-1
  ydum=totaltestsF4[time_ofint_inODEunits:tend]+totaltests_notF4[time_ofint_inODEunits:tend]
  avy = round(1000*sum(ydum)/sum(totP[time_ofint_inODEunits:tend]))
  df= data.frame(x=tdum,y=ydum)
  maxy=max(pretty_breaks()(c(0, df$y)))
  vecy=pretty_breaks()(c(0, df$y))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of DAA") + ggtitle(paste0("Total number of DAA treatments - average ",avy," per 1,000 PWID"))+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(0,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(tdum[1],2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
  
  
}

plot_output_treats_v2=function(t1,XT,scale00,scale10,pcom,alpha,time_ofint_inODEunits,DAAtreat,SCreening,background_diag){
  # XT is the output from the compartment model
  # t1 is the required times
  # This is for all intervention model
  t1=1+t1; # starts at zero
  totaltestsF4=rep(0,length(t1))
  totaltests_notF4=rep(0,length(t1))
  for (i in 1 : length(t1)){
    N=XT[,i]
    dum=treat_comps_pv4(N,scale00,scale10,t1[i],pcom,alpha,time_ofint_inODEunits,
                        background_diag,DAAtreat,SCreening)
    totaltestsF4[i]=sum(dum$total_T4)
    totaltests_notF4[i]=sum(dum$total_not_T4)
  }
  tend = dim(XT)[2]
  tdum=1950+t1[time_ofint_inODEunits:tend]-1
  ydum=totaltestsF4[time_ofint_inODEunits:tend]+totaltests_notF4[time_ofint_inODEunits:tend]
  df= data.frame(x=tdum,y=ydum)
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("# of DAA") + ggtitle("Total number of DAA treatments")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(tdum[1],2030,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
  
  
}

treat_comps_pv2=function(p,X,scale00,scale10,n,t,pcom,alpha,time_ofint_inODEunits){
  # returns phi and phidash
  # p is the proportion of treatments allocated to people with late liver disease stage
  # where late is F3,F4,LT stages
  # X are the compartments
  # structure
  # 180 (00), 180 (01) ,180 (10),180 (11)
  # each 9*20
  # scale00 and scale10 ar ethe former and current sacling factors
  # n is the number of treatments per year
  # t is the time of the slice
  # NOTE it is assumed to be actual year, i.e. not 0:1/12:80
  # but 1950:1/12:2030
  # pcom is the probability of pwid completing treatement
  # alpha is the SVR probability
  # time2015_inODEunits is the year 2015 in the ODE timesacle
  # so for example 0:1/12:80 for 1950:2030
  # then time2015_inODEunits=781
  
  
  F3_former = seq(10,180,20)
  F4_former = seq(11,180,20)
  LT1_former = seq(14,180,20)
  LT2_former = seq(15,180,20)
  
  F3_pwider = seq(370,540,20)
  F4_pwider = seq(371,540,20)
  LT1_pwider =seq(374,540,20)
  LT2_pwider =seq(375,540,20)
  
  F0_former = seq(7,180,20)
  F1_former = seq(8,180,20)
  F2_former = seq(9,180,20)
  F0_pwider = seq(367,540,20)
  F1_pwider = seq(368,540,20)
  F2_pwider = seq(369,540,20)
  
  
  r_10=c(F3_pwider,F4_pwider,LT1_pwider,LT2_pwider) # advanced stages current
  r_00=c(F3_former,F4_former,LT1_former,LT2_former) # advanced stages former
  
  c_10=c(F0_pwider ,F1_pwider, F2_pwider, F3_pwider,F4_pwider,LT1_pwider,LT2_pwider) # all chronic stages current
  
  Topwider=scale10*sum(X[c_10]);
  Adformer = scale00*sum(X[r_00]); # advanced stage totals
  Adpwider = scale10*sum(X[r_10]);
  Adtotal=Adformer+Adpwider;
  omega = Adpwider/(p*Adtotal+(1-p)*Topwider);
  # This automates on p
  fprop =  n*omega/Adpwider;
  gprop = n*p*(1-omega)/Adformer;
  hprop = n*(1-p)*(1-omega)/(Topwider-Adpwider);
  
  phi=rep(0,720)
  
  jAdpwider=c(F3_pwider ,F4_pwider ,LT1_pwider ,LT2_pwider ); # Advanced pwid
  jMipwider=c(F0_pwider ,F1_pwider ,F2_pwider); # Mild pwid
  jAdformer=c(F3_former ,F4_former ,LT1_former, LT2_former ); # Advanced former
  
  
  if (t>=time_ofint_inODEunits){
    # how any treated in F3,F4,LT1,LT2
    # f function
    phi[jAdpwider]=pmin(fprop*X[jAdpwider],X[jAdpwider]);
    phi[jAdformer]=pmin(gprop*X[jAdformer],X[jAdformer]);
    phi[jMipwider]=pmin(hprop*X[jMipwider],X[jMipwider]);
    
    
    
    # if there are more treats than subjects then there will be surplus
    ndash=n-scale_sumphi_p(phi,scale00,scale10,jAdformer,jAdpwider,jMipwider);
    
    if (ndash > 1e-5){
      if (p==1){
        #Treatments were allocated proportionally across injecting drug use status 
        #(i.e. patients were treated irrespective of whether they were current of former PWID)
        #, and if the number of treatments available was greater than the number of 
        #HCV-infected people with advanced liver disease, 
        #the remaining treatments were allocated to HCV-infected people with early liver disease, 
        #proportionally across injecting drug use status
        
        # n was bigger than the denominatr of prop_treated
        # add extra allocation to phi overall all chronic for treat
        # i.e F0,F1,F2,F3,F4,LT1,LT2
        jformer=c(F0_former ,F1_former ,F2_former)
        jpwider=c(F0_pwider ,F1_pwider ,F2_pwider)
        j=c(jformer, jpwider)
        # needs to be scaled as ndash is scaled
        scaledbot=scale00*sum(X[jformer])+scale10*sum(X[jpwider]);
        phi[j]=phi[j]+ndash*X[j]/scaledbot; # extra allocation
      }else{# p=0
        #Treatments were allocated proportionally across liver disease stages, 
        #and if the number of treatments available was greater than the number 
        #of current PWID eligible for treatment, 
        #remaining treatments were allocated to former PWID, 
        #proportionally across disease stages.
        jformer=c(F0_former ,F1_former ,F2_former ,F3_former ,F4_former ,LT1_former ,LT2_former)
        j=jformer;
        scaledbot=scale00*sum(X[jformer]);
        phi[j]=phi[j]+ndash*X[j]/scaledbot; # extra allocation
      }
    }
  }  
  # now move the treated to the T compartments
  
  phidash=rep(0,720);
  T0_former = seq(16,180,20)
  T1_former = seq(17,180,20)
  T2_former = seq(18,180,20)
  T3_former = seq(19,180,20)
  T4_former =seq(20,180,20)
  T0_pwider = seq(376,540,20);
  T1_pwider = seq(377,540,20);
  T2_pwider = seq(378,540,20);
  T3_pwider = seq(379,540,20);
  T4_pwider = seq(380,540,20);
  
  phidash[T0_former]=alpha*phi[F0_former];
  phidash[T1_former]=alpha*phi[F1_former];
  phidash[T2_former]=alpha*phi[F2_former];
  phidash[T3_former]=alpha*phi[F3_former];
  phidash[T4_former]=alpha*(phi[F4_former]+phi[LT1_former]+phi[LT2_former]);
  
  phidash[T0_pwider]=alpha*pcom*phi[F0_pwider];
  phidash[T1_pwider]=alpha*pcom*phi[F1_pwider];
  phidash[T2_pwider]=alpha*pcom*phi[F2_pwider];
  phidash[T3_pwider]=alpha*pcom*phi[F3_pwider];
  phidash[T4_pwider]=alpha*pcom*(phi[F4_pwider]+phi[LT1_pwider]+phi[LT2_pwider]);
  
  # These are used to count the number of treatments
  total_not_T4 = scale10*(phi[F0_pwider]+phi[F1_pwider]+phi[F2_pwider]+phi[F3_pwider]+phi[LT1_pwider]+phi[LT2_pwider])+
    scale00*(phi[F0_former]+phi[F1_former]+phi[F2_former]+phi[F3_former]+phi[LT1_former]+phi[LT2_former])
  total_T4=scale00*phi[F4_former]+ scale10*phi[F4_pwider];    
  
  return(list(phi=phi,phidash=phidash,total_not_T4=total_not_T4,total_T4=total_T4))
}

treat_comps_pv3=function(n0,n1,X,scale00,scale10,t,pcom,alpha,time_ofint_inODEunits){
  # runs a mixture, so no p just n0 and n1
  # returns phi and phidash
  # p is the proportion of treatments allocated to people with late liver disease stage
  # where late is F3,F4,LT stages
  # X are the compartments
  # structure
  # 180 (00), 180 (01) ,180 (10),180 (11)
  # each 9*20
  # scale00 and scale10 ar ethe former and current sacling factors
  # n is the number of treatments per year
  # t is the time of the slice
  # NOTE it is assumed to be actual year, i.e. not 0:1/12:80
  # but 1950:1/12:2030
  # pcom is the probability of pwid completing treatement
  # alpha is the SVR probability
  # time2015_inODEunits is the year 2015 in the ODE timesacle
  # so for example 0:1/12:80 for 1950:2030
  # then time2015_inODEunits=781
  
  
  F3_former = seq(10,180,20)
  F4_former = seq(11,180,20)
  LT1_former = seq(14,180,20)
  LT2_former = seq(15,180,20)
  
  F3_pwider = seq(370,540,20)
  F4_pwider = seq(371,540,20)
  LT1_pwider =seq(374,540,20)
  LT2_pwider =seq(375,540,20)
  
  F0_former = seq(7,180,20)
  F1_former = seq(8,180,20)
  F2_former = seq(9,180,20)
  F0_pwider = seq(367,540,20)
  F1_pwider = seq(368,540,20)
  F2_pwider = seq(369,540,20)
  
  
  r_10=c(F3_pwider,F4_pwider,LT1_pwider,LT2_pwider) # advanced stages current
  r_00=c(F3_former,F4_former,LT1_former,LT2_former) # advanced stages former
  
  c_10=c(F0_pwider ,F1_pwider, F2_pwider, F3_pwider,F4_pwider,LT1_pwider,LT2_pwider) # all chronic stages current
  
  Topwider=scale10*sum(X[c_10]);
  Adformer = scale00*sum(X[r_00]); # advanced stage totals
  Adpwider = scale10*sum(X[r_10]);
  Adtotal=Adformer+Adpwider;


  fprop_p0 =  n0/Topwider;
  gprop_p0 =  0;
  hprop_p0 = n0/Topwider;
  
  fprop_p1=n1/Adtotal;
  gprop_p1=n1/Adtotal;
  hprop_p1 =  0;
  n=n0+n1;
  
  phi=rep(0,720)
  
  jAdpwider=c(F3_pwider ,F4_pwider ,LT1_pwider ,LT2_pwider ); # Advanced pwid
  jMipwider=c(F0_pwider ,F1_pwider ,F2_pwider); # Mild pwid
  jAdformer=c(F3_former ,F4_former ,LT1_former, LT2_former ); # Advanced former
  
  
  if (t>=time_ofint_inODEunits){
    # how any treated in F3,F4,LT1,LT2
    # f function

    phi[jMipwider]=pmin(hprop_p0*X[jMipwider],X[jMipwider]);
    phi[jAdpwider]=pmin((fprop_p0+fprop_p1)*X[jAdpwider],X[jAdpwider]);
    phi[jAdformer]=pmin(gprop_p1*X[jAdformer],X[jAdformer]);
    
    # if there are more treats than subjects then there will be surplus
    ndash=n-scale_sumphi_p(phi,scale00,scale10,jAdformer,jAdpwider,jMipwider);
    
    if (ndash > 1e-5){
        #Treatments were allocated proportionally across injecting drug use status 
        #(i.e. patients were treated irrespective of whether they were current of former PWID)
        #, and if the number of treatments available was greater than the number of 
        #HCV-infected people with advanced liver disease, 
        #the remaining treatments were allocated to HCV-infected people with early liver disease, 
        #proportionally across injecting drug use status
        
        # n was bigger than the denominatr of prop_treated
        # add extra allocation to phi overall all chronic for treat
        # i.e F0,F1,F2,F3,F4,LT1,LT2
        jformer=c(F0_former ,F1_former ,F2_former)
        # needs to be scaled as ndash is scaled
        scaledbot=scale00*sum(X[jformer])
        phi[jformer]=phi[jformer]+ndash*X[jformer]/scaledbot; # extra allocation
    }
  }  
  # now move the treated to the T compartments
  
  phidash=rep(0,720);
  T0_former = seq(16,180,20)
  T1_former = seq(17,180,20)
  T2_former = seq(18,180,20)
  T3_former = seq(19,180,20)
  T4_former =seq(20,180,20)
  T0_pwider = seq(376,540,20);
  T1_pwider = seq(377,540,20);
  T2_pwider = seq(378,540,20);
  T3_pwider = seq(379,540,20);
  T4_pwider = seq(380,540,20);
  
  phidash[T0_former]=alpha*phi[F0_former];
  phidash[T1_former]=alpha*phi[F1_former];
  phidash[T2_former]=alpha*phi[F2_former];
  phidash[T3_former]=alpha*phi[F3_former];
  phidash[T4_former]=alpha*(phi[F4_former]+phi[LT1_former]+phi[LT2_former]);
  
  phidash[T0_pwider]=alpha*pcom*phi[F0_pwider];
  phidash[T1_pwider]=alpha*pcom*phi[F1_pwider];
  phidash[T2_pwider]=alpha*pcom*phi[F2_pwider];
  phidash[T3_pwider]=alpha*pcom*phi[F3_pwider];
  phidash[T4_pwider]=alpha*pcom*(phi[F4_pwider]+phi[LT1_pwider]+phi[LT2_pwider]);
  
  # These are used to count the number of treatments
  total_not_T4 = scale10*(phi[F0_pwider]+phi[F1_pwider]+phi[F2_pwider]+phi[F3_pwider]+phi[LT1_pwider]+phi[LT2_pwider])+
    scale00*(phi[F0_former]+phi[F1_former]+phi[F2_former]+phi[F3_former]+phi[LT1_former]+phi[LT2_former])
  total_T4=scale00*phi[F4_former]+ scale10*phi[F4_pwider];    
  
  return(list(phi=phi,phidash=phidash,total_not_T4=total_not_T4,total_T4=total_T4))
}

plot_output_incidence_new_chronic_cases=function(XT,scale00,scale10,delta,rateA_F0,time_cut,
                                     A_comps_00,A_comps_01,A_comps_10,A_comps_11){
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80

T00=(1-delta)*rateA_F0*scale00*colSums(XT[A_comps_00,]);
T01=(1-delta)*rateA_F0*scale00*colSums(XT[A_comps_01,]);
T11=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11,]);
T10=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10,]);
# integrate
t=time_cut-2 # since starts at 1950
tend = dim(XT)[2]-1
yinci_PWID=T00[t:tend]+T01[t:tend]+T10[t:tend]+T11[t:tend]
df= data.frame(x=1950+(t:tend),y=100*((yinci_PWID/yinci_PWID[1]) - 1))
p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
  xlab("Year") + ylab("Decrease %") + ggtitle("% decrease in new chronic infections compared to 2021")+
  scale_y_continuous(breaks = c(seq(10,-30,-5)),labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),lim=c(-30,10))+
  scale_x_continuous(breaks = c(seq(1950+t,1950+tend,1)),expand=c(0,0))+
  theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
return(p1)
}

plot_output_incidence_new_chronic_cases_v2=function(XT,scale00,scale10,delta,rateA_F0,time_cut,
                                                 A_comps_00,A_comps_01,A_comps_10,A_comps_11){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  
  T00=(1-delta)*rateA_F0*scale00*colSums(XT[A_comps_00,]);
  T01=(1-delta)*rateA_F0*scale00*colSums(XT[A_comps_01,]);
  T11=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11,]);
  T10=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10,]);
  # integrate
  t=time_cut-2 # since starts at 1950
  tend = dim(XT)[2]-1
  yinci_PWID=T00[t:tend]+T01[t:tend]+T10[t:tend]+T11[t:tend]
  df= data.frame(x=1950+(t:tend),y=100*((yinci_PWID/yinci_PWID[1]) - 1))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Change %") + ggtitle("% change in new chronic infections compared to 2021")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(1950+t,1950+tend,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_incidence_new_chronic_cases_v2_all=function(XT,scale00,scale10,delta,rateA_F0,time_cut,
                                                    A_comps_00,A_comps_01,A_comps_10,A_comps_11,typey,
                                                    curr_mort_pwid,curr_mort_former){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  
  T00=(1-delta)*rateA_F0*scale00*colSums((1-curr_mort_former)%*%XT[A_comps_00,]);
  T01=(1-delta)*rateA_F0*scale00*colSums((1-curr_mort_former)%*%XT[A_comps_01,]);
  T11=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_11,]);
  T10=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_10,]);

  T11_nep=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11+360,]);
  T10_nep=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10+360,]);

  T11_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11+720,]);
  T10_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10+720,]);
  
  T11_nep_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11+1080,]);
  T10_nep_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10+1080,]);
  
  # integrate
  t=time_cut # since starts at 1950
  tval=t-7
  yinci_PWID_bot = T00[tval]+T01[tval]+T10[tval]+T11[tval]+
    T10_nep[tval]+T11_nep[tval]+
    T10_ost[tval]+T11_ost[tval]+
    T10_nep_ost[tval]+T11_nep_ost[tval]
  tend = dim(XT)[2]
  yinci_PWID=T00[t:tend]+T01[t:tend]+T10[t:tend]+T11[t:tend]+
             T10_nep[t:tend]+T11_nep[t:tend]+
             T10_ost[t:tend]+T11_ost[t:tend]+
             T10_nep_ost[t:tend]+T11_nep_ost[t:tend]
  if (typey ==1){
    df= data.frame(x=1949+(t:tend),y=100*((yinci_PWID/yinci_PWID_bot) - 1))# instead of yinci_PWID[1]
    ylaby = "Change %"
    titly = "% change in new chronic infections compared to 2015"
    ywholim=-80
    minylim= -80*105.5/100
  }else{
    df= data.frame(x=1949+(t:tend),y=yinci_PWID)
    ylaby = "# of subjects"
    titly = "Number of new chronic infections"
    ywholim = 0.2*yinci_PWID_bot
    minylim = ywholim * 0.95
  }
  
  maxy=max(pretty_breaks()(c(0, df$y)))
  miny=min(min(pretty_breaks()(c(0, df$y))),minylim)
  vecy=pretty_breaks()(c(miny, df$y))

  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab(ylaby) + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(miny,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(1949+t,1949+tend,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    geom_hline(yintercept = ywholim,color="blue",size=2) + 
    annotate("text", min(1950+t+7), ywholim, vjust = -1, label = "WHO new infections target")
  return(p1)
}

plot_output_incidence_new_chronic_cases_v2_all_genpop_extra=function(XT,scale00,scale10,delta,rateA_F0,time_cut,
                                                        A_comps_00,A_comps_01,A_comps_10,A_comps_11,typey,
                                                        curr_mort_pwid,curr_mort_former,genpop_inci){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  
  T00=(1-delta)*rateA_F0*scale00*colSums((1-curr_mort_former)%*%XT[A_comps_00,]);
  T01=(1-delta)*rateA_F0*scale00*colSums((1-curr_mort_former)%*%XT[A_comps_01,]);
  T11=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_11,]);
  T10=(1-delta)*rateA_F0*scale10*colSums((1-curr_mort_pwid)%*%XT[A_comps_10,]);
  
  T11_nep=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11+360,]);
  T10_nep=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10+360,]);
  
  T11_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11+720,]);
  T10_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10+720,]);
  
  T11_nep_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11+1080,]);
  T10_nep_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10+1080,]);
  
  # integrate
  t=time_cut # since starts at 1950
  tval=t-7
  yinci_PWID_bot = T00[tval]+T01[tval]+T10[tval]+T11[tval]+
    T10_nep[tval]+T11_nep[tval]+
    T10_ost[tval]+T11_ost[tval]+
    T10_nep_ost[tval]+T11_nep_ost[tval]
  tend = dim(XT)[2]
  yinci_PWID=T00[t:tend]+T01[t:tend]+T10[t:tend]+T11[t:tend]+
    T10_nep[t:tend]+T11_nep[t:tend]+
    T10_ost[t:tend]+T11_ost[t:tend]+
    T10_nep_ost[t:tend]+T11_nep_ost[t:tend]
  yinci_PWID = yinci_PWID+genpop_inci[8:26]
  yinci_PWID_bot=yinci_PWID_bot+genpop_inci[4]
  if (typey ==1){
    df= data.frame(x=1949+(t:tend),y=100*((yinci_PWID/yinci_PWID_bot) - 1))# instead of yinci_PWID[1]
    ylaby = "Change %"
    titly = "% change in new chronic infections compared to 2015, PWID & gen pop"
    ywholim=-80
    minylim= -80*105.5/100
  }else{
    df= data.frame(x=1949+(t:tend),y=yinci_PWID)
    ylaby = "# of subjects"
    titly = "Number of new chronic infections,PWID & gen pop"
    ywholim = 0.2*yinci_PWID_bot
    minylim = ywholim * 0.95
  }
  
  maxy=max(pretty_breaks()(c(0, df$y)))
  miny=min(min(pretty_breaks()(c(0, df$y))),minylim)
  vecy=pretty_breaks()(c(miny, df$y))
  
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab(ylaby) + ggtitle(titly)+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(miny,maxy),breaks=vecy)+
    scale_x_continuous(breaks = c(seq(1949+t,1949+tend,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    geom_hline(yintercept = ywholim,color="blue",size=2) + 
    annotate("text", min(1950+t+7), ywholim, vjust = -1, label = "WHO new infections target")
  return(p1)
}

plot_output_incidence_new_chronic_cases_v2_all_long=function(XT,scale00,scale10,delta,rateA_F0,time_cut,
                                                        A_comps_00,A_comps_01,A_comps_10,A_comps_11){
  # As above but with no scaling
  # X is the output from the compartment model
  # A_comps are the acute compartments
  # ij, i is 0 former 1 current, j is 0 never failed , 1 failed
  # scale00 and scale10 are the scaling parameters form teh base line fit
  # delta is the proportion of infections spontaneously clearing
  # rateA_F0 rate of change of new chronic cases
  # time_cut # time of intervention, on scale 0:80
  
  T00=(1-delta)*rateA_F0*scale00*colSums(XT[A_comps_00,]);
  T01=(1-delta)*rateA_F0*scale00*colSums(XT[A_comps_01,]);
  T11=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11,]);
  T10=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10,]);
  
  T11_nep=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11+360,]);
  T10_nep=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10+360,]);
  
  T11_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11+720,]);
  T10_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10+720,]);
  
  T11_nep_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_11+1080,]);
  T10_nep_ost=(1-delta)*rateA_F0*scale10*colSums(XT[A_comps_10+1080,]);
  
  # integrate
  t=time_cut-2 # since starts at 1950
  tend = dim(XT)[2]-1
  yinci_PWID=T00[t:tend]+T01[t:tend]+T10[t:tend]+T11[t:tend]+
    T10_nep[t:tend]+T11_nep[t:tend]+
    T10_ost[t:tend]+T11_ost[t:tend]+
    T10_nep_ost[t:tend]+T11_nep_ost[t:tend]
  
  df= data.frame(x=1950+(t:tend),y=100*((yinci_PWID/yinci_PWID[1]) - 1))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Change %") + ggtitle("% change in new chronic infections compared to 2021")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(1950+t,seq(1950+t+4,1950+tend,5)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}
param_setup=function(rin){
  if (length(rin) == 5){
    r6=rin[1];
    r5=rin[2];
    r4=rin[3];
    r2=rin[4];
    r3=rin[5];
  }else{
    r6=rin[1];
    r2=rin[2];
    r3=rin[3];
    r5=0.027 # default values
    r4=17
  }
  
  # This creates two matrices for the static parameters
  # M is for former i=0
  # Mdash is for current i=1, i.e. PWID
  parm_names = c('delta','r_AF0','w','r_SVR4DC','r_SVR4HCC','r_F0F1','r_F1F2','r_F2F3','r_F3F4','r_F4DC','r_F4HCC',
                 'r_DCHCC','r_DCLT','r_DCdeath','r_HCCLT','r_HCCdeath',
                 'r_LT1death','r_LT2death')
  
  parm_current=rep(0,18)
  parm_former=rep(0,18)
  parm_current[1]=0.26; parm_former[1] = parm_current[1]; # spontaneous clearance
  parm_current[2]=52/12; parm_former[2] = parm_current[2];# acute duration is 12 weeks
  parm_current[3]=0; parm_former[3] = parm_current[3];# duration to return to susceptible after treated - no treats for burden model
  parm_current[4]=0; parm_former[4] = parm_current[4];# SVR4 to DC
  parm_current[5]=0; parm_former[5] = parm_current[5];# SVR4 to HCC
  parm_current[6] = -log(1- r2 ); parm_former[6] = -log(1-r3); # F0 to F1 current then former
  parm_current[7] = -log(1- 0.085 ); parm_former[7] = -log(1- 0.074); # F1 to F2 current then former
  parm_current[8] = -log(1-0.085); parm_former[8] = -log(1-0.106); # F2 to F3 current then former
  parm_current[9] = -log(1-0.130); parm_former[9] = -log(1-0.105); # F3 to F4 current then former
  parm_current[10] = -log(1-0.037); parm_former[10] = parm_current[10]; # F4 to DC current then former
  parm_current[11] = -log(1-0.01); parm_former[11] = parm_current[11]; # F4 to HCC current then former
  parm_current[12] = -log(1-0.068); parm_former[12] = parm_current[12]; # DC to HCC
  parm_current[13] = -log(1-0.033); parm_former[13] = parm_current[13]; # DC to LT
  parm_current[15] = -log(1-0.1); parm_former[15] = parm_current[15]; # HCC to LT
  parm_current[14] = -log(1-0.138) ; parm_former[14] = parm_current[14]; # DC to death 
  parm_current[16] = -log(1-0.605); parm_former[16] = parm_current[16]; # HCC death
  parm_current[17] = -log(1-0.169); parm_former[17] = parm_current[17]; # LT to death year 1
  parm_current[18] = -log(1-0.034); parm_former[18] = parm_current[18]; # LT to death year 2
  
  phi = 0;# place holder
  
  Mdash=matrix(data = 0,nrow=20,ncol=20);
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
  Mdash[12,11]=parm_current[10]+parm_current[4];Mdash[12,12] = -parm_current[12]-parm_current[13]-parm_current[14];
  Mdash[13,11]=parm_current[11]+parm_current[5];Mdash[13,12]=parm_current[12];Mdash[13,13]=-parm_current[15]-parm_current[16];
  Mdash[14,12]=parm_current[13];Mdash[14,13]=parm_current[15];Mdash[14,14]=-1-parm_current[17];
  Mdash[15,14]=1;Mdash[15,15]=-parm_current[18];
  Mdash[16,16]=-parm_current[3];
  Mdash[17,17]=-parm_current[3];
  Mdash[18,18]=-parm_current[3];
  Mdash[19,19]=-parm_current[3];
  Mdash[20,20]=-parm_current[3];
  
  
  M=matrix(data = 0,nrow=20,ncol=20);
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
  M[12,11]=parm_former[10]+parm_former[4];M[12,12] = -parm_former[12]-parm_former[13]-parm_former[14];
  M[13,11]=parm_former[11]+parm_former[5];M[13,12]=parm_former[12];M[13,13]=-parm_former[15]-parm_former[16];
  M[14,12]=parm_former[13];M[14,13]=parm_former[15];M[14,14]=-1-parm_former[17];
  M[15,14]=1;M[15,15]=-parm_former[18];
  M[16,16]=-parm_former[3];
  M[17,17]=-parm_former[3];
  M[18,18]=-parm_former[3];
  M[19,19]=-parm_former[3];
  M[20,20]=-parm_former[3];
  
  age_matrix=matrix(data = 0, nrow=9,ncol=9)
  age_matrix[1,1] = -1/5;
  age_matrix[2,1] = 1/5;age_matrix[2,2] = -1/5;
  age_matrix[3,2] = 1/5;age_matrix[3,3] = -1/5;
  age_matrix[4,3] = 1/5;age_matrix[4,4] = -1/10;
  age_matrix[5,4] = 1/10;age_matrix[5,5] = -1/10;
  age_matrix[6,5] = 1/10;age_matrix[6,6] = -1/10;
  age_matrix[7,6] = 1/10;age_matrix[7,7] = -1/10;
  age_matrix[8,7] = 1/10;age_matrix[8,8] = -1/10;
  age_matrix[9,8] = 1/10;
  
  curr_mort_pwid=c(0.96 ,0.96 ,1.12 ,0.18 ,0.22 ,0.53 ,1.38 ,4.28 ,14.96)/1000; # mortality per year for PWID
  curr_mort_former=c(0.044 ,0.051 ,0.062 ,0.1 ,0.222 ,0.534 ,1.376 ,4.282 ,14.956 )/1000; # mortality per year for PWID
  
  
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
  
  death_rate_dc=  -log(1-0.138) 
  death_rate_hcc= -log(1-0.605) 
  death_rate_lt1= -log(1-0.169) 
  death_rate_lt2= -log(1-0.034) 
  
  param_vals=c(Mvec,Mdashvec,extra_parms_vals,
               phiminusvals1,phiminusvals2,phiminusvals3,phiminusvals4,phiminusvals5,
               phiplussvals1,phiplussvals2,phiplussvals3,phiplussvals4,phiplussvals5,
               Magevec,
               curr_mort_pwid,curr_mort_former,
               death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  
  return(param_vals)
}

param_setup_daa=function(rin,treatduration){
  # Includes the rate to go from treat to svr
  if (length(rin) == 5){
    r6=rin[1];
    r5=rin[2];
    r4=rin[3];
    r2=rin[4];
    r3=rin[5];
  }else{
    r6=rin[1];
    r2=rin[2];
    r3=rin[3];
    r5=0.027 # default values
    r4=17
  }
  
  # This creates two matrices for the static parameters
  # M is for former i=0
  # Mdash is for current i=1, i.e. PWID
  parm_names = c('delta','r_AF0','w','r_SVR4DC','r_SVR4HCC','r_F0F1','r_F1F2','r_F2F3','r_F3F4','r_F4DC','r_F4HCC',
                 'r_DCHCC','r_DCLT','r_DCdeath','r_HCCLT','r_HCCdeath',
                 'r_LT1death','r_LT2death')
  
  parm_current=rep(0,18)
  parm_former=rep(0,18)
  parm_current[1]=0.26; parm_former[1] = parm_current[1]; # spontaneous clearance
  parm_current[2]=52/12; parm_former[2] = parm_current[2];# acute duration is 12 weeks
  parm_current[3]=treatduration; parm_former[3] = parm_current[3];# duration to return to susceptible after treated - no treats for burden model
  parm_current[4]=0; parm_former[4] = parm_current[4];# SVR4 to DC
  parm_current[5]=0; parm_former[5] = parm_current[5];# SVR4 to HCC
  parm_current[6] = -log(1- r2 ); parm_former[6] = -log(1-r3); # F0 to F1 current then former
  parm_current[7] = -log(1- 0.085 ); parm_former[7] = -log(1- 0.074); # F1 to F2 current then former
  parm_current[8] = -log(1-0.085); parm_former[8] = -log(1-0.106); # F2 to F3 current then former
  parm_current[9] = -log(1-0.130); parm_former[9] = -log(1-0.105); # F3 to F4 current then former
  parm_current[10] = -log(1-0.037); parm_former[10] = parm_current[10]; # F4 to DC current then former
  parm_current[11] = -log(1-0.01); parm_former[11] = parm_current[11]; # F4 to HCC current then former
  parm_current[12] = -log(1-0.068); parm_former[12] = parm_current[12]; # DC to HCC
  parm_current[13] = -log(1-0.033); parm_former[13] = parm_current[13]; # DC to LT
  parm_current[15] = -log(1-0.1); parm_former[15] = parm_current[15]; # HCC to LT
  parm_current[14] = -log(1-0.138) ; parm_former[14] = parm_current[14]; # DC to death 
  parm_current[16] = -log(1-0.605); parm_former[16] = parm_current[16]; # HCC death
  parm_current[17] = -log(1-0.169); parm_former[17] = parm_current[17]; # LT to death year 1
  parm_current[18] = -log(1-0.034); parm_former[18] = parm_current[18]; # LT to death year 2
  
  phi = 0;# place holder
  
  Mdash=matrix(data = 0,nrow=20,ncol=20);
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
  Mdash[12,11]=parm_current[10]+parm_current[4];Mdash[12,12] = -parm_current[12]-parm_current[13]-parm_current[14];
  Mdash[13,11]=parm_current[11]+parm_current[5];Mdash[13,12]=parm_current[12];Mdash[13,13]=-parm_current[15]-parm_current[16];
  Mdash[14,12]=parm_current[13];Mdash[14,13]=parm_current[15];Mdash[14,14]=-1-parm_current[17];
  Mdash[15,14]=1;Mdash[15,15]=-parm_current[18];
  Mdash[16,16]=-parm_current[3];
  Mdash[17,17]=-parm_current[3];
  Mdash[18,18]=-parm_current[3];
  Mdash[19,19]=-parm_current[3];
  Mdash[20,20]=-parm_current[3];
  
  
  M=matrix(data = 0,nrow=20,ncol=20);
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
  M[12,11]=parm_former[10]+parm_former[4];M[12,12] = -parm_former[12]-parm_former[13]-parm_former[14];
  M[13,11]=parm_former[11]+parm_former[5];M[13,12]=parm_former[12];M[13,13]=-parm_former[15]-parm_former[16];
  M[14,12]=parm_former[13];M[14,13]=parm_former[15];M[14,14]=-1-parm_former[17];
  M[15,14]=1;M[15,15]=-parm_former[18];
  M[16,16]=-parm_former[3];
  M[17,17]=-parm_former[3];
  M[18,18]=-parm_former[3];
  M[19,19]=-parm_former[3];
  M[20,20]=-parm_former[3];
  
  age_matrix=matrix(data = 0, nrow=9,ncol=9)
  age_matrix[1,1] = -1/5;
  age_matrix[2,1] = 1/5;age_matrix[2,2] = -1/5;
  age_matrix[3,2] = 1/5;age_matrix[3,3] = -1/5;
  age_matrix[4,3] = 1/5;age_matrix[4,4] = -1/10;
  age_matrix[5,4] = 1/10;age_matrix[5,5] = -1/10;
  age_matrix[6,5] = 1/10;age_matrix[6,6] = -1/10;
  age_matrix[7,6] = 1/10;age_matrix[7,7] = -1/10;
  age_matrix[8,7] = 1/10;age_matrix[8,8] = -1/10;
  age_matrix[9,8] = 1/10;
  
  curr_mort_pwid=c(0.96 ,0.96 ,1.12 ,0.18 ,0.22 ,0.53 ,1.38 ,4.28 ,14.96)/1000; # mortality per year for PWID
  curr_mort_former=c(0.044 ,0.051 ,0.062 ,0.1 ,0.222 ,0.534 ,1.376 ,4.282 ,14.956 )/1000; # mortality per year for PWID
  
  
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
  
  death_rate_dc=  -log(1-0.138) 
  death_rate_hcc= -log(1-0.605) 
  death_rate_lt1= -log(1-0.169) 
  death_rate_lt2= -log(1-0.034) 
  
  param_vals=c(Mvec,Mdashvec,extra_parms_vals,
               phiminusvals1,phiminusvals2,phiminusvals3,phiminusvals4,phiminusvals5,
               phiplussvals1,phiplussvals2,phiplussvals3,phiplussvals4,phiplussvals5,
               Magevec,
               curr_mort_pwid,curr_mort_former,
               death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  
  return(param_vals)
}

param_setup_daa_v2=function(rin,treatduration){
  # Includes the rate to go from treat to svr
  # April 22 allows two trends
  # Always three parameters now
  # pop_growth handled outside
  r6=rin[1];
  r5=rin[2];
  r4=rin[3];

  
  # This creates two matrices for the static parameters
  # M is for former i=0
  # Mdash is for current i=1, i.e. PWID
  parm_names = c('delta','r_AF0','w','r_SVR4DC','r_SVR4HCC','r_F0F1','r_F1F2','r_F2F3','r_F3F4','r_F4DC','r_F4HCC',
                 'r_DCHCC','r_DCLT','r_DCdeath','r_HCCLT','r_HCCdeath',
                 'r_LT1death','r_LT2death')
  
  parm_current=rep(0,18)
  parm_former=rep(0,18)
  parm_current[1]=0.26; parm_former[1] = parm_current[1]; # spontaneous clearance
  parm_current[2]=52/12; parm_former[2] = parm_current[2];# acute duration is 12 weeks
  parm_current[3]=treatduration; parm_former[3] = parm_current[3];# duration to return to susceptible after treated - no treats for burden model
  parm_current[4]=0; parm_former[4] = parm_current[4];# SVR4 to DC
  parm_current[5]=0; parm_former[5] = parm_current[5];# SVR4 to HCC
  parm_current[6] = -log(1- 0.116 ); parm_former[6] = -log(1-0.106); # F0 to F1 current then former
  #parm_current[6] = -log(1- r2 ); parm_former[6] = -log(1-r3); # F0 to F1 current then former
  parm_current[7] = -log(1- 0.085 ); parm_former[7] = -log(1- 0.074); # F1 to F2 current then former
  parm_current[8] = -log(1-0.085); parm_former[8] = -log(1-0.106); # F2 to F3 current then former
  parm_current[9] = -log(1-0.130); parm_former[9] = -log(1-0.105); # F3 to F4 current then former
  parm_current[10] = -log(1-0.037); parm_former[10] = parm_current[10]; # F4 to DC current then former
  parm_current[11] = -log(1-0.01); parm_former[11] = parm_current[11]; # F4 to HCC current then former
  parm_current[12] = -log(1-0.068); parm_former[12] = parm_current[12]; # DC to HCC
  parm_current[13] = -log(1-0.033); parm_former[13] = parm_current[13]; # DC to LT
  parm_current[15] = -log(1-0.1); parm_former[15] = parm_current[15]; # HCC to LT
  parm_current[14] = -log(1-0.138) ; parm_former[14] = parm_current[14]; # DC to death 
  parm_current[16] = -log(1-0.605); parm_former[16] = parm_current[16]; # HCC death
  parm_current[17] = -log(1-0.169); parm_former[17] = parm_current[17]; # LT to death year 1
  parm_current[18] = -log(1-0.034); parm_former[18] = parm_current[18]; # LT to death year 2
  
  phi = 0;# place holder
  
  Mdash=matrix(data = 0,nrow=20,ncol=20);
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
  Mdash[12,11]=parm_current[10]+parm_current[4];Mdash[12,12] = -parm_current[12]-parm_current[13]-parm_current[14];
  Mdash[13,11]=parm_current[11]+parm_current[5];Mdash[13,12]=parm_current[12];Mdash[13,13]=-parm_current[15]-parm_current[16];
  Mdash[14,12]=parm_current[13];Mdash[14,13]=parm_current[15];Mdash[14,14]=-1-parm_current[17];
  Mdash[15,14]=1;Mdash[15,15]=-parm_current[18];
  Mdash[16,16]=-parm_current[3];
  Mdash[17,17]=-parm_current[3];
  Mdash[18,18]=-parm_current[3];
  Mdash[19,19]=-parm_current[3];
  Mdash[20,20]=-parm_current[3];
  
  
  M=matrix(data = 0,nrow=20,ncol=20);
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
  M[12,11]=parm_former[10]+parm_former[4];M[12,12] = -parm_former[12]-parm_former[13]-parm_former[14];
  M[13,11]=parm_former[11]+parm_former[5];M[13,12]=parm_former[12];M[13,13]=-parm_former[15]-parm_former[16];
  M[14,12]=parm_former[13];M[14,13]=parm_former[15];M[14,14]=-1-parm_former[17];
  M[15,14]=1;M[15,15]=-parm_former[18];
  M[16,16]=-parm_former[3];
  M[17,17]=-parm_former[3];
  M[18,18]=-parm_former[3];
  M[19,19]=-parm_former[3];
  M[20,20]=-parm_former[3];
  
  age_matrix=matrix(data = 0, nrow=9,ncol=9)
  age_matrix[1,1] = -1/5;
  age_matrix[2,1] = 1/5;age_matrix[2,2] = -1/5;
  age_matrix[3,2] = 1/5;age_matrix[3,3] = -1/5;
  age_matrix[4,3] = 1/5;age_matrix[4,4] = -1/10;
  age_matrix[5,4] = 1/10;age_matrix[5,5] = -1/10;
  age_matrix[6,5] = 1/10;age_matrix[6,6] = -1/10;
  age_matrix[7,6] = 1/10;age_matrix[7,7] = -1/10;
  age_matrix[8,7] = 1/10;age_matrix[8,8] = -1/10;
  age_matrix[9,8] = 1/10;
  
  curr_mort_pwid=c(0.96 ,0.96 ,1.12 ,0.18 ,0.22 ,0.53 ,1.38 ,4.28 ,14.96)/1000; # mortality per year for PWID
  curr_mort_former=c(0.044 ,0.051 ,0.062 ,0.1 ,0.222 ,0.534 ,1.376 ,4.282 ,14.956 )/1000; # mortality per year for PWID
  
  
  mort_current = t(matrix(rep(curr_mort_pwid,20),ncol=20))
  mort_former = t(matrix(rep(curr_mort_former,20),ncol=20))
  extra_parms_nams=c('piv','relapse','nu'); # infection rate (piv instead of pi), relapse to IDU, 1/duration of injecting span
  
  extra_parms_vals=c(r6,-log(1-r5),1/r4) # was 5.6%, nu was 1/17
  rout=c(r6,r5,r4)
  
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
  
  death_rate_dc=  -log(1-0.138) 
  death_rate_hcc= -log(1-0.605) 
  death_rate_lt1= -log(1-0.169) 
  death_rate_lt2= -log(1-0.034) 
  
  param_vals=c(Mvec,Mdashvec,extra_parms_vals,
               phiminusvals1,phiminusvals2,phiminusvals3,phiminusvals4,phiminusvals5,
               phiplussvals1,phiplussvals2,phiplussvals3,phiplussvals4,phiplussvals5,
               Magevec,
               curr_mort_pwid,curr_mort_former,
               death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  
  return(param_vals)
}

param_setup_daa_v3=function(rin,treatduration){
  # Includes the rate to go from treat to svr
  # April 22 allows two trends
  # Always three parameters now
  # pop_growth handled outside
  r6=rin[1];
  r5=rin[2];
  r4=rin[3];
  
  
  # This creates two matrices for the static parameters
  # M is for former i=0
  # Mdash is for current i=1, i.e. PWID
  parm_names = c('delta','r_AF0','w','r_SVR4DC','r_SVR4HCC','r_F0F1','r_F1F2','r_F2F3','r_F3F4','r_F4DC','r_F4HCC',
                 'r_DCHCC','r_DCLT','r_DCdeath','r_HCCLT','r_HCCdeath',
                 'r_LT1death','r_LT2death')
  
  parm_current=rep(0,18)
  parm_former=rep(0,18)
  parm_current[1]=0.26; parm_former[1] = parm_current[1]; # spontaneous clearance
  parm_current[2]=52/12; parm_former[2] = parm_current[2];# acute duration is 12 weeks
  parm_current[3]=treatduration; parm_former[3] = parm_current[3];# duration to return to susceptible after treated - no treats for burden model
  parm_current[4]=0; parm_former[4] = parm_current[4];# SVR4 to DC
  parm_current[5]=0; parm_former[5] = parm_current[5];# SVR4 to HCC
  parm_current[6] = -log(1- 0.116 ); parm_former[6] = -log(1-0.106); # F0 to F1 current then former
  #parm_current[6] = -log(1- r2 ); parm_former[6] = -log(1-r3); # F0 to F1 current then former
  parm_current[7] = -log(1- 0.085 ); parm_former[7] = -log(1- 0.074); # F1 to F2 current then former
  parm_current[8] = -log(1-0.085); parm_former[8] = -log(1-0.106); # F2 to F3 current then former
  parm_current[9] = -log(1-0.130); parm_former[9] = -log(1-0.105); # F3 to F4 current then former
  parm_current[10] = -log(1-0.037); parm_former[10] = parm_current[10]; # F4 to DC current then former
  parm_current[11] = -log(1-0.01); parm_former[11] = parm_current[11]; # F4 to HCC current then former
  parm_current[12] = -log(1-0.068); parm_former[12] = parm_current[12]; # DC to HCC
  parm_current[13] = -log(1-0.033); parm_former[13] = parm_current[13]; # DC to LT
  parm_current[15] = -log(1-0.1); parm_former[15] = parm_current[15]; # HCC to LT
  parm_current[14] = -log(1-0.138) ; parm_former[14] = parm_current[14]; # DC to death 
  parm_current[16] = -log(1-0.605); parm_former[16] = parm_current[16]; # HCC death
  parm_current[17] = -log(1-0.169); parm_former[17] = parm_current[17]; # LT to death year 1
  parm_current[18] = -log(1-0.034); parm_former[18] = parm_current[18]; # LT to death year 2
  
  phi = 0;# place holder
  
  Mdash=matrix(data = 0,nrow=20,ncol=20);
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
  Mdash[12,11]=parm_current[10]+parm_current[4];Mdash[12,12] = -parm_current[12]-parm_current[13]-parm_current[14];
  Mdash[13,11]=parm_current[11]+parm_current[5];Mdash[13,12]=parm_current[12];Mdash[13,13]=-parm_current[15]-parm_current[16];
  Mdash[14,12]=parm_current[13];Mdash[14,13]=parm_current[15];Mdash[14,14]=-1-parm_current[17];
  Mdash[15,14]=1;Mdash[15,15]=-parm_current[18];
  Mdash[16,16]=-parm_current[3];
  Mdash[17,17]=-parm_current[3];
  Mdash[18,18]=-parm_current[3];
  Mdash[19,19]=-parm_current[3];
  Mdash[20,20]=-parm_current[3];
  
  
  M=matrix(data = 0,nrow=20,ncol=20);
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
  M[12,11]=parm_former[10]+parm_former[4];M[12,12] = -parm_former[12]-parm_former[13]-parm_former[14];
  M[13,11]=parm_former[11]+parm_former[5];M[13,12]=parm_former[12];M[13,13]=-parm_former[15]-parm_former[16];
  M[14,12]=parm_former[13];M[14,13]=parm_former[15];M[14,14]=-1-parm_former[17];
  M[15,14]=1;M[15,15]=-parm_former[18];
  M[16,16]=-parm_former[3];
  M[17,17]=-parm_former[3];
  M[18,18]=-parm_former[3];
  M[19,19]=-parm_former[3];
  M[20,20]=-parm_former[3];
  
  age_matrix=matrix(data = 0, nrow=9,ncol=9)
  age_matrix[1,1] = -1/5;
  age_matrix[2,1] = 1/5;age_matrix[2,2] = -1/5;
  age_matrix[3,2] = 1/5;age_matrix[3,3] = -1/5;
  age_matrix[4,3] = 1/5;age_matrix[4,4] = -1/10;
  age_matrix[5,4] = 1/10;age_matrix[5,5] = -1/10;
  age_matrix[6,5] = 1/10;age_matrix[6,6] = -1/10;
  age_matrix[7,6] = 1/10;age_matrix[7,7] = -1/10;
  age_matrix[8,7] = 1/10;age_matrix[8,8] = -1/10;
  age_matrix[9,8] = 1/10;
  
  curr_mort_pwid=c(0.96 ,0.96 ,1.12 ,0.18 ,0.22 ,0.53 ,1.38 ,4.28 ,14.96)/1000; # mortality per year for PWID
  curr_mort_former=c(0.044 ,0.051 ,0.062 ,0.1 ,0.222 ,0.534 ,1.376 ,4.282 ,14.956 )/1000; # mortality per year for PWID
  
  
  mort_current = t(matrix(rep(curr_mort_pwid,20),ncol=20))
  mort_former = t(matrix(rep(curr_mort_former,20),ncol=20))
  extra_parms_nams=c('piv','relapse','nu'); # infection rate (piv instead of pi), relapse to IDU, 1/duration of injecting span
  
  extra_parms_vals=c(r6,-log(1-r5),1/r4) # was 5.6%, nu was 1/17
  rout=c(r6,r5,r4)
  
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
  
  death_rate_dc=  -log(1-0.138) 
  death_rate_hcc= -log(1-0.605) 
  death_rate_lt1= -log(1-0.169) 
  death_rate_lt2= -log(1-0.034) 
  
  param_vals=c(Mvec,Mdashvec,extra_parms_vals,
               phiminusvals1,phiminusvals2,phiminusvals3,phiminusvals4,phiminusvals5,
               phiplussvals1,phiplussvals2,phiplussvals3,phiplussvals4,phiplussvals5,
               Magevec,
               curr_mort_pwid,curr_mort_former,
               death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  
  return(param_vals)
}


plot_output_mortality =function(XT,death_vec,scale00,scale10,time_cut){
  # death_vec=c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  death_vals10=rep(0,4)
  death_vals00=rep(0,4)
  death_vals11=rep(0,4)
  death_vals01=rep(0,4)
  time_end=dim(XT)[2]
  XTdeaths=rep(0,time_end-time_cut+1)
  # Now can have deaths in the failed treatment compartments
  testcompvec00=t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec10=t(matrix(360+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec01_failed=t(matrix(c(seq(192,360,20),seq(193,360,20),seq(194,360,20),seq(195,360,20)),nrow=9,ncol=4))
  testcompvec11_failed=t(matrix(c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
  for (i in 1 : 4){
    Xtest=scale10*colSums(XT[testcompvec10[i,],])
    Ytest=scale00*colSums(XT[testcompvec00[i,],]);
    X1test=scale10*colSums(XT[testcompvec11_failed[i,],]);
    Y1test=scale00*colSums(XT[testcompvec01_failed[i,],]);
    XTdeaths=XTdeaths+Xtest[time_cut:time_end]+Ytest[time_cut:time_end]+
                      X1test[time_cut:time_end]+Y1test[time_cut:time_end]
  }
  df= data.frame(x=-1+1950+(time_cut:time_end),y=100*((XTdeaths/XTdeaths[1]) - 1))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Decrease %") + ggtitle("% decrease in liver related mortality compared to 2021")+
    scale_y_continuous(breaks = c(seq(15,-30,-5)),labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),lim=c(-30,15))+
    scale_x_continuous(breaks = c(seq(1950+time_cut-1,1950+time_end-1,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_mortality_v2 =function(XT,death_vec,scale00,scale10,time_cut){
  # death_vec=c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  death_vals10=rep(0,4)
  death_vals00=rep(0,4)
  death_vals11=rep(0,4)
  death_vals01=rep(0,4)
  time_end=dim(XT)[2]
  XTdeaths=rep(0,time_end-time_cut+1)
  # Now can have deaths in the failed treatment compartments
  testcompvec00=t(matrix(c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec10=t(matrix(360+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec01_failed=t(matrix(c(seq(192,360,20),seq(193,360,20),seq(194,360,20),seq(195,360,20)),nrow=9,ncol=4))
  testcompvec11_failed=t(matrix(c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
  for (i in 1 : 4){
    Xtest=scale10*colSums(XT[testcompvec10[i,],])
    Ytest=scale00*colSums(XT[testcompvec00[i,],]);
    X1test=scale10*colSums(XT[testcompvec11_failed[i,],]);
    Y1test=scale00*colSums(XT[testcompvec01_failed[i,],]);
    XTdeaths=XTdeaths+Xtest[time_cut:time_end]+Ytest[time_cut:time_end]+
      X1test[time_cut:time_end]+Y1test[time_cut:time_end]
  }
  df= data.frame(x=-1+1950+(time_cut:time_end),y=100*((XTdeaths/XTdeaths[1]) - 1))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Change %") + ggtitle("% change in liver related mortality compared to 2021")+
    scale_y_continuous(breaks = c(seq(15,-30,-5)),labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),lim=c(-30,15))+
    scale_x_continuous(breaks = c(seq(1950+time_cut-1,1950+time_end-1,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}

plot_output_mortality_v2_all =function(XT,death_vec,scale00,scale10,time_cut,t1,pcom,alpha,DAAtreat,SCreening,enroll_nep,enroll_ost,enroll_nep_ost,typey){
  #t1,XT,scale00,scale10,pcom,alpha,time_ofint_inODEunits,DAAtreat,SCreening,
  #enroll_nep,enroll_ost,enroll_nep_ost
  # death_vec=c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  death_vals10=rep(0,4)
  death_vals00=rep(0,4)
  death_vals11=rep(0,4)
  death_vals01=rep(0,4)
  time_cut=time_cut
  time_end=dim(XT)[2]
  XTdeaths=rep(0,time_end-time_cut+1)
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
    if (i > 73){
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
  
  #death_vec=1-exp(-death_vec)
  # This should be rates
  death_vec=-log(1-death_vec)
  Xbot_deaths=0
  tval = time_cut-6
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


plot_output_mortality_v2_all_genpop_extra =function(XT,death_vec,scale00,scale10,time_cut,t1,pcom,alpha,DAAtreat,SCreening,enroll_nep,enroll_ost,enroll_nep_ost,typey,
                                                    mortvec){
  #t1,XT,scale00,scale10,pcom,alpha,time_ofint_inODEunits,DAAtreat,SCreening,
  #enroll_nep,enroll_ost,enroll_nep_ost
  # death_vec=c(death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  death_vals10=rep(0,4)
  death_vals00=rep(0,4)
  death_vals11=rep(0,4)
  death_vals01=rep(0,4)
  time_cut=time_cut
  time_end=dim(XT)[2]
  XTdeaths=rep(0,time_end-time_cut+1)
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
    if (i > 73){
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
  
  #death_vec=1-exp(-death_vec)
  # BUG - death_vec was originally a rate
  # converted to back prob in call
  # so this needs tobe converted to a rate
  death_vec=-log(1-death_vec)
  Xbot_deaths=0
  tval = time_cut-6
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
  XTdeaths=XTdeaths+mortvec[8:26]
  Xbot_deaths=Xbot_deaths+mortvec[4]
  

  if (typey==1){
    df= data.frame(x=-1+1950+(time_cut:time_end),y=100*((XTdeaths/Xbot_deaths) - 1)) # XTdeaths[1]
    titly="% change in liver related mortality (DCC,HCC,LT) compared to 2015, PWID & gen pop"
    ylaby="Change %"
    ywholim=-65
    minylim= -65*105.5/100
  }else{
    df= data.frame(x=-1+1950+(time_cut:time_end),y=XTdeaths)
    titly="Number of liver related deaths (DCC,HCC,LT), PWID & gen pop"
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


plot_output_mortality_v2_all_long =function(XT,death_vec,scale00,scale10,time_cut){
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
  
  testcompvec10_nep=t(matrix(720+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec11_failed_nep=t(matrix(360+c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
  
  testcompvec10_ost=t(matrix(1080+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec11_failed_ost=t(matrix(720+c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
  
  testcompvec10_nep_ost=t(matrix(1440+c(seq(12,180,20),seq(13,180,20),seq(14,180,20),seq(15,180,20)),nrow=9,ncol=4))
  testcompvec11_failed_nep_ost=t(matrix(1080+c(seq(552,720,20),seq(553,720,20),seq(554,720,20),seq(555,720,20)),nrow=9,ncol=4))
  
  death_vec=1-exp(-death_vec)
  for (i in 1 : 4){
    Xtest=death_vec[i]*scale10*colSums(XT[testcompvec10[i,],])
    Ytest=death_vec[i]*scale00*colSums(XT[testcompvec00[i,],]);
    X1test=death_vec[i]*scale10*colSums(XT[testcompvec11_failed[i,],]);
    Y1test=death_vec[i]*scale00*colSums(XT[testcompvec01_failed[i,],]);
    
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
  }
  #print(XTdeaths)
  df= data.frame(x=-1+1950+(time_cut:time_end),y=100*((XTdeaths/XTdeaths[1]) - 1))
  p1=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
    xlab("Year") + ylab("Change %") + ggtitle("% change in liver related mortality (DCC,HCC,LT) compared to 2021")+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(1950+time_cut-1,seq(1950+time_cut+3,1950+time_end,5)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p1)
}


plot_output_nep_ost_v2_all=function(t1,XT,scale10,time_ofint_inODEunits){
  # XT is the output from the compartment model
  # t1 is the required times
  # This is for all intervention model
  
  F0_current_nep = seq(367,540,20) + 360
  F1_current_nep = seq(368,540,20) + 360
  F2_current_nep = seq(369,540,20) + 360 
  F3_current_nep = seq(370,540,20) + 360
  F4_current_nep = seq(371,540,20) + 360
  
  F0_current_ost = seq(367,540,20) + 720
  F1_current_ost = seq(368,540,20) + 720
  F2_current_ost = seq(369,540,20) + 720 
  F3_current_ost = seq(370,540,20) + 720
  F4_current_ost = seq(371,540,20) + 720
  
  F0_current_nep_ost = seq(367,540,20) + 1080
  F1_current_nep_ost = seq(368,540,20) + 1080
  F2_current_nep_ost = seq(369,540,20) + 1080 
  F3_current_nep_ost = seq(370,540,20) + 1080
  F4_current_nep_ost = seq(371,540,20) + 1080
  
  T0_current_nep_ost = seq(367,540,20) + 1080+9
  T1_current_nep_ost = seq(368,540,20) + 1080+9
  T2_current_nep_ost = seq(369,540,20) + 1080+9 
  T3_current_nep_ost = seq(370,540,20) + 1080+9
  T4_current_nep_ost = seq(371,540,20) + 1080+9
  
  r_10_nep=c(F3_current_nep,F4_current_nep) # advanced stages current
  r_10_ost=c(F3_current_ost,F4_current_ost) # advanced stages current
  r_10_nep_ost=c(F3_current_nep_ost,F4_current_nep_ost)
                 #T3_current_nep_ost ,T4_current_nep_ost) # advanced stages current
  
  c_10_nep=c(F0_current_nep ,F1_current_nep, F2_current_nep) # all chronic stages current
  c_10_ost=c(F0_current_ost ,F1_current_ost, F2_current_ost) # all chronic stages current
  c_10_nep_ost=c(F0_current_nep_ost ,F1_current_nep_ost, F2_current_nep_ost)
                 #T0_current_nep_ost ,T1_current_nep_ost, T2_current_nep_ost) # all chronic stages current
  
  t1=t1; # starts at zero
  total_nep=rep(0,length(t1))
  total_ost=rep(0,length(t1))
  total_nep_ost=rep(0,length(t1))
  
  for (i in 1 : length(t1)){
    N=XT[,i]
    
    Mild_current_nep=scale10*sum(N[c_10_nep])
    Mild_current_ost=scale10*sum(N[c_10_ost])
    Mild_current_nep_ost=scale10*sum(N[c_10_nep_ost])
    
    Ad_current_nep=scale10*sum(N[r_10_nep])
    Ad_current_ost=scale10*sum(N[r_10_ost])
    Ad_current_nep_ost=scale10*sum(N[r_10_nep_ost])
    
    total_nep[i]=Mild_current_nep+Ad_current_nep
    total_ost[i]=Mild_current_ost+Ad_current_ost
    total_nep_ost[i]=Mild_current_nep_ost+Ad_current_nep_ost
  }
  
  y1=total_nep
  y2=total_ost
  y3=total_nep_ost
  df=data.frame(t1,y1,y2,y3)
  p=ggplot(data=subset(df,t1>=72), aes(x=t1+1950)) + geom_line(aes(y=y1,colour="nep"),size=2)+
    geom_line(aes(y=y2,colour="ost"),size=2)+ geom_line(aes(y=y3,colour="nep-ost"),size=2)+
    scale_colour_manual("", 
                        breaks = c("nep", "ost","nep-ost"),
                        values = c("blue", "green","red")) +
    xlab("Year") + ylab("Number of people") + ggtitle("Number of subjects enrolled to prevention from 2022 to 2030")+
    
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(2022,2030,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p)  
}

plot_output_nep_ost_v3_all=function(t1,XT,scale10,time_ofint_inODEunits){
  # XT is the output from the compartment model
  # t1 is the required times
  # This is for all intervention model
  
  F0_current_nep = seq(367,540,20) + 360
  F1_current_nep = seq(368,540,20) + 360
  F2_current_nep = seq(369,540,20) + 360 
  F3_current_nep = seq(370,540,20) + 360
  F4_current_nep = seq(371,540,20) + 360
  
  F0_current_ost = seq(367,540,20) + 720
  F1_current_ost = seq(368,540,20) + 720
  F2_current_ost = seq(369,540,20) + 720 
  F3_current_ost = seq(370,540,20) + 720
  F4_current_ost = seq(371,540,20) + 720
  
  F0_current_nep_ost = seq(367,540,20) + 1080
  F1_current_nep_ost = seq(368,540,20) + 1080
  F2_current_nep_ost = seq(369,540,20) + 1080 
  F3_current_nep_ost = seq(370,540,20) + 1080
  F4_current_nep_ost = seq(371,540,20) + 1080
  
  T0_current_nep_ost = seq(367,540,20) + 1080+9
  T1_current_nep_ost = seq(368,540,20) + 1080+9
  T2_current_nep_ost = seq(369,540,20) + 1080+9 
  T3_current_nep_ost = seq(370,540,20) + 1080+9
  T4_current_nep_ost = seq(371,540,20) + 1080+9
  
  r_10_nep=c(F3_current_nep,F4_current_nep) # advanced stages current
  r_10_ost=c(F3_current_ost,F4_current_ost) # advanced stages current
  r_10_nep_ost=c(F3_current_nep_ost,F4_current_nep_ost)
  #T3_current_nep_ost ,T4_current_nep_ost) # advanced stages current
  
  c_10_nep=c(F0_current_nep ,F1_current_nep, F2_current_nep) # all chronic stages current
  c_10_ost=c(F0_current_ost ,F1_current_ost, F2_current_ost) # all chronic stages current
  c_10_nep_ost=c(F0_current_nep_ost ,F1_current_nep_ost, F2_current_nep_ost)
  #T0_current_nep_ost ,T1_current_nep_ost, T2_current_nep_ost) # all chronic stages current
  
  t1=t1; # starts at zero
  total_nep=rep(0,length(t1))
  total_ost=rep(0,length(t1))
  total_nep_ost=rep(0,length(t1))
  
  for (i in 1 : length(t1)){
    N=XT[,i]
    
    Mild_current_nep=scale10*sum(N[c_10_nep])
    Mild_current_ost=scale10*sum(N[c_10_ost])
    Mild_current_nep_ost=scale10*sum(N[c_10_nep_ost])
    
    Ad_current_nep=scale10*sum(N[r_10_nep])
    Ad_current_ost=scale10*sum(N[r_10_ost])
    Ad_current_nep_ost=scale10*sum(N[r_10_nep_ost])
    
    total_nep[i]=Mild_current_nep+Ad_current_nep
    total_ost[i]=Mild_current_ost+Ad_current_ost
    total_nep_ost[i]=Mild_current_nep_ost+Ad_current_nep_ost
  }
  
  y1=total_nep
  y2=total_ost
  y3=total_nep_ost
  t1=1950+(0:80)
  df=data.frame(t1,y1,y2,y3)

  p=ggplot(data=subset(df,t1>=2022), aes(x=t1)) + geom_line(aes(y=y1,colour="nep"),size=2)+
    geom_line(aes(y=y2,colour="ost"),size=2)+ geom_line(aes(y=y3,colour="nep-ost"),size=2)+
    scale_colour_manual("", 
                        breaks = c("nep", "ost","nep-ost"),
                        values = c("blue", "green","red")) +
    xlab("Year") + ylab("Number of people") + ggtitle("Number of subjects enrolled to prevention from 2022 to 2030")+
    
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(2022,2030,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p)  
}

plot_output_nep_ost_v3_all_count=function(t1,XT,scale10,time_ofint_inODEunits){
  # XT is the output from the compartment model
  # t1 is the required times
  # This is for all intervention model
  
  F0_current_nep = seq(367,540,20) + 360
  F1_current_nep = seq(368,540,20) + 360
  F2_current_nep = seq(369,540,20) + 360 
  F3_current_nep = seq(370,540,20) + 360
  F4_current_nep = seq(371,540,20) + 360
  T0_current_nep = seq(367,540,20) + 360 + 9
  T1_current_nep = seq(368,540,20) + 360 + 9
  T2_current_nep = seq(369,540,20) + 360 + 9
  T3_current_nep = seq(370,540,20) + 360 + 9
  T4_current_nep = seq(371,540,20) + 360 + 9
  cn=c(F0_current_nep,F1_current_nep,F2_current_nep,F3_current_nep,F4_current_nep,
       T0_current_nep,T1_current_nep,T2_current_nep,T3_current_nep,T4_current_nep)
  F0_current_ost = seq(367,540,20) + 720
  F1_current_ost = seq(368,540,20) + 720
  F2_current_ost = seq(369,540,20) + 720 
  F3_current_ost = seq(370,540,20) + 720
  F4_current_ost = seq(371,540,20) + 720
  T0_current_ost = seq(367,540,20) + 720 + 9
  T1_current_ost = seq(368,540,20) + 720 + 9
  T2_current_ost = seq(369,540,20) + 720 + 9
  T3_current_ost = seq(370,540,20) + 720 + 9
  T4_current_ost = seq(371,540,20) + 720 + 9
  co=c(F0_current_ost,F1_current_ost,F2_current_ost,F3_current_ost,F4_current_ost,
       T0_current_ost,T1_current_ost,T2_current_ost,T3_current_ost,T4_current_ost)
  F0_current_nep_ost = seq(367,540,20) + 1080
  F1_current_nep_ost = seq(368,540,20) + 1080
  F2_current_nep_ost = seq(369,540,20) + 1080 
  F3_current_nep_ost = seq(370,540,20) + 1080
  F4_current_nep_ost = seq(371,540,20) + 1080
  T0_current_nep_ost = seq(367,540,20) + 1080+9
  T1_current_nep_ost = seq(368,540,20) + 1080+9
  T2_current_nep_ost = seq(369,540,20) + 1080+9 
  T3_current_nep_ost = seq(370,540,20) + 1080+9
  T4_current_nep_ost = seq(371,540,20) + 1080+9
  cno=c(F0_current_nep_ost,F1_current_nep_ost,F2_current_nep_ost,F3_current_nep_ost,F4_current_nep_ost,
        T0_current_nep_ost,T1_current_nep_ost,T2_current_nep_ost,T3_current_nep_ost,T4_current_nep_ost)
  # *** note need all the subjects that have been moved not just F0-F4
  # for nep = 721:1080, for OST 1081:1440, for both 1441:1800
  total_nep = scale10*c(0,diff(colSums(XT[721:1080,])))
  total_ost = scale10*c(0,diff(colSums(XT[1081:1440,])))
  total_nep_ost = scale10*c(0,diff(colSums(XT[1441:1800,])))
  y1=total_nep
  y2=total_ost
  y3=total_nep_ost
  t1=1950+(0:90)
  df=data.frame(t1,y1,y2,y3)
  
  p=ggplot(data=subset(df,t1>=2022), aes(x=t1)) + geom_line(aes(y=y1,colour="nep"),size=2)+
    geom_line(aes(y=y2,colour="ost"),size=2)+ geom_line(aes(y=y3,colour="nep-ost"),size=2)+
    scale_colour_manual("", 
                        breaks = c("nep", "ost","nep-ost"),
                        values = c("blue", "green","red")) +
    xlab("Year") + ylab("Number of people") + ggtitle("Number of subjects at stage F0-F4 enrolled per year to prevention from 2022 to 2030")+
    
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0))+
    scale_x_continuous(breaks = c(seq(2022,2040,1)),expand=c(0,0))+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_discrete(labels=c("2015","2017","2019","2021","2023","2025","2027","2029","2031"))
  return(p)  
}

treat_comps_pv7=function(X,scale00,scale10,t,pcom,alpha,time_ofint_inODEunits,
                         DAAtreat,
                         SCreening,
                         enroll_nep,enroll_ost,enroll_nep_ost){
  # The enrollment rates are required as they define the screening effort within NEP/OST
  # Define the intervention populations for DAA
  
  # These are screen separately * not used as all directed at current
  F0_former_nep = seq(7,180,20) 
  F1_former_nep = seq(8,180,20) 
  F2_former_nep = seq(9,180,20) 
  F3_former_nep = seq(10,180,20) 
  F4_former_nep = seq(11,180,20) 
  
  # These are used for DAA intervention only
  F0_current = seq(367,540,20)
  F1_current = seq(368,540,20)
  F2_current = seq(369,540,20)  
  F3_current = seq(370,540,20)
  F4_current = seq(371,540,20)
  
  DC_current = seq(372,540,20)
  HCC_current = seq(373,540,20)
  LT1_current = seq(374,540,20)
  LT2_current = seq(375,540,20)
  
  DC_former = seq(12,180,20)
  HCC_former = seq(13,180,20)
  LT1_former = seq(14,180,20)
  LT2_former = seq(15,180,20)
  
  F0_current_nep = seq(367,540,20) + 360
  F1_current_nep = seq(368,540,20) + 360
  F2_current_nep = seq(369,540,20) + 360 
  F3_current_nep = seq(370,540,20) + 360
  F4_current_nep = seq(371,540,20) + 360
  
  F0_current_ost = seq(367,540,20) + 720
  F1_current_ost = seq(368,540,20) + 720
  F2_current_ost = seq(369,540,20) + 720 
  F3_current_ost = seq(370,540,20) + 720
  F4_current_ost = seq(371,540,20) + 720
  
  F0_current_nep_ost = seq(367,540,20) + 1080
  F1_current_nep_ost = seq(368,540,20) + 1080
  F2_current_nep_ost = seq(369,540,20) + 1080 
  F3_current_nep_ost = seq(370,540,20) + 1080
  F4_current_nep_ost = seq(371,540,20) + 1080
  
  r_10=c(F3_current,F4_current) 
  c_10=c(F0_current ,F1_current, F2_current)
  c_p_10=c(DC_current,HCC_current,LT1_current,LT2_current) # current post
  f_p_10=c(DC_former,HCC_former,LT1_former,LT2_former) # former post
  
  r_10_nep=c(F3_current_nep,F4_current_nep) # advanced stages current
  r_10_ost=c(F3_current_ost,F4_current_ost) # advanced stages current
  r_10_nep_ost=c(F3_current_nep_ost,F4_current_nep_ost) # advanced stages current
  
  c_10_nep=c(F0_current_nep ,F1_current_nep, F2_current_nep) # all chronic stages current
  c_10_ost=c(F0_current_ost ,F1_current_ost, F2_current_ost) # all chronic stages current
  c_10_nep_ost=c(F0_current_nep_ost ,F1_current_nep_ost, F2_current_nep_ost) # all chronic stages current
  
  #r_00=c(F3_former,F4_former) # advanced stages former
  #c_00=c(F0_former ,F1_former, F2_former) # all chronic stages current
  
  phi=rep(0,1800)
  
  # Total number of subjects
  Mild_current=sum(X[c_10])
  Mild_current_nep=sum(X[c_10_nep])#Mild_current*(1-exp(-enroll_nep))
  Mild_current_ost=sum(X[c_10_ost])#Mild_current*(1-exp(-enroll_ost))
  Mild_current_nep_ost=sum(X[c_10_nep_ost])#Mild_current*(1-exp(-enroll_nep_ost))
  #print(paste0("mild current nep ost ",Mild_current_nep_ost," at t=",t,"\n"))
  Ad_current=sum(X[r_10])
  Ad_current_nep=sum(X[r_10_nep])#Ad_current*(1-exp(-enroll_nep))
  Ad_current_ost=sum(X[r_10_ost])#Ad_current*(1-exp(-enroll_ost))
  Ad_current_nep_ost=sum(X[r_10_nep_ost])#Ad_current*(1-exp(-enroll_nep_ost))
  # for post compartments
  Post_current=sum(X[c_p_10])
  Post_former=sum(X[f_p_10])
  
  if (t>=time_ofint_inODEunits){
    # this is the number of treatments available
    dum=move_DAA_v4(X,DAAtreat,enroll_nep,enroll_ost,enroll_nep_ost,SCreening,
                 Mild_current,Mild_current_nep,Mild_current_ost,Mild_current_nep_ost,
                 Ad_current,Ad_current_nep,Ad_current_ost,Ad_current_nep_ost,
                 F0_current,F1_current,F2_current,F3_current,F4_current,
                 F0_current_nep,F1_current_nep,F2_current_nep,F3_current_nep,F4_current_nep,
                 F0_current_ost,F1_current_ost,F2_current_ost,F3_current_ost,F4_current_ost,
                 F0_current_nep_ost,F1_current_nep_ost,F2_current_nep_ost,F3_current_nep_ost,F4_current_nep_ost,
                 Post_current,Post_former,
                 HCC_current,DC_current,LT1_current,LT2_current,
                 HCC_former,DC_former,LT1_former,LT2_former,
                 scale00,scale10,pcom,alpha)
    direct_deaths=dum$direct_deaths
    direct_deaths_nep=dum$direct_deaths_nep
    direct_deaths_ost=dum$direct_deaths_ost
    direct_deaths_nep_ost=dum$direct_deaths_nep_ost
    # dum=move_DAA_v5(X,1951.345,DAAtreat,enroll_nep,enroll_ost,enroll_nep_ost,SCreening,
    #                 Mild_current,Mild_current_nep,Mild_current_ost,Mild_current_nep_ost,
    #                 Ad_current,Ad_current_nep,Ad_current_ost,Ad_current_nep_ost,
    #                 F0_current,F1_current,F2_current,F3_current,F4_current,
    #                 F0_current_nep,F1_current_nep,F2_current_nep,F3_current_nep,F4_current_nep,
    #                 F0_current_ost,F1_current_ost,F2_current_ost,F3_current_ost,F4_current_ost,
    #                 F0_current_nep_ost,F1_current_nep_ost,F2_current_nep_ost,F3_current_nep_ost,F4_current_nep_ost,
    #                 scale10,pcom,alpha)
    
  }else{
    # no treatments so values are zero (9 age groups for the totals)
    dum=data.frame(phi=phi,phidash=phi,total_not_T4=rep(0,9),total_T4=rep(0,9))
    direct_deaths=matrix(data =0,nrow = 4,ncol = 4) # rows are 00,01,10,11 and cols are DC,HCC,LT1,LT2
    direct_deaths_nep=matrix(data =0,nrow = 2,ncol = 4)
    direct_deaths_ost=matrix(data =0,nrow = 2,ncol = 4)
    direct_deaths_nep_ost=matrix(data =0,nrow = 2,ncol = 4)
  }
  
  return(list(phi=dum$phi,phidash=dum$phidash,total_not_T4=dum$total_not_T4,total_T4=dum$total_T4,
              direct_deaths=direct_deaths,direct_deaths_nep=direct_deaths_nep,direct_deaths_ost=direct_deaths_ost,
              direct_deaths_nep_ost=direct_deaths_nep_ost))
  
  
  
}

 move_DAA_v4=function(X,DAAtreat,enroll_nep,enroll_ost,enroll_nep_ost,SCreening,
                     Mild_current,Mild_current_nep,Mild_current_ost,Mild_current_nep_ost,
                     Ad_current,Ad_current_nep,Ad_current_ost,Ad_current_nep_ost,
                     F0_current,F1_current,F2_current,F3_current,F4_current,
                     F0_current_nep,F1_current_nep,F2_current_nep,F3_current_nep,F4_current_nep,
                     F0_current_ost,F1_current_ost,F2_current_ost,F3_current_ost,F4_current_ost,
                     F0_current_nep_ost,F1_current_nep_ost,F2_current_nep_ost,F3_current_nep_ost,F4_current_nep_ost,
                     Post_current,Post_former,
                     HCC_current,DC_current,LT1_current,LT2_current,
                     HCC_former,DC_former,LT1_former,LT2_former,
                     scale00,scale10,pcom,alpha){
   
  # Notes
  # X is teh comaprtment cols for a time t
  # enroll_nep,enroll_ost,enroll_nep_ost,SCreening - nep/ost/both coverage (as a rate)
  # no longer used, left in for call continuity
  # Mild_current,Mild_current_nep,Mild_current_ost,Mild_current_nep_ost,
  # Ad_current,Ad_current_nep,Ad_current_ost,Ad_current_nep_ost, 
  # Post_current,Post_former,
  # ******* pcom_former not taken as 1  pcom_former=pcom   
   
   
  
  # total PWID
  totPWID= (sum(X[361:720])+sum(X[721:1080])+sum(X[1081:1440])+sum(X[1441:1800]))
  # Mild_current=scale10*Mild_current
  # Mild_current_nep=scale10*Mild_current_nep
  # Mild_current_ost=scale10*Mild_current_ost
  # Mild_current_nep_ost=scale10*Mild_current_nep_ost
  # Ad_current=scale10*Ad_current
  # Ad_current_nep=scale10*Ad_current_nep
  # Ad_current_ost=scale10*Ad_current_ost
  # Ad_current_nep_ost=scale10*Ad_current_nep_ost
  
  # convert to treatments in mild and advanced
  ntreat_nmild=(1-exp(-DAAtreat$Pcurrenmild))*totPWID # actually F0-F3
  ntreat_nadva=(1-exp(-DAAtreat$Pcurrenadva))*totPWID # F4-LT2
  ntreat_former_nmild=(1-exp(-DAAtreat$Pformermild))*totPWID # ** for testing
  ntreat_former_nadva=(1-exp(-DAAtreat$Pformeradva))*totPWID # ** for testing
  
  # for testing
  F0_former = seq(7,180,20)
  F1_former = seq(8,180,20)
  F2_former = seq(9,180,20)  
  F3_former = seq(10,180,20)
  F4_former = seq(11,180,20)
  F0_Rtformer = 180 + seq(7,180,20)
  F1_Rtformer = 180 + seq(8,180,20)
  F2_Rtformer = 180 + seq(9,180,20)  
  F3_Rtformer = 180 + seq(10,180,20)
  F4_Rtformer = 180 + seq(11,180,20)
  DC_Rtformer = 180+seq(12,180,20)
  HCC_Rtformer = 180+seq(13,180,20)
  LT1_Rtformer = 180+seq(14,180,20)
  LT2_Rtformer = 180+seq(15,180,20)
  F0_Rtcurrent = 180 + F0_current
  F1_Rtcurrent = 180 + F1_current
  F2_Rtcurrent = 180 + F2_current
  F3_Rtcurrent = 180 + F3_current
  F4_Rtcurrent = 180 + F4_current
  DC_Rtcurrent = 180 + DC_current
  HCC_Rtcurrent = 180 + HCC_current
  LT1_Rtcurrent = 180 + LT1_current
  LT2_Rtcurrent = 180 + LT2_current
  # nep
  HCC_current_nep=HCC_current+360
  DC_current_nep=DC_current+360
  LT1_current_nep=LT1_current+360
  LT2_current_nep=LT2_current+360
  F0_Rtcurrent_nep=F0_current_nep+180
  F1_Rtcurrent_nep=F1_current_nep+180
  F2_Rtcurrent_nep=F2_current_nep+180
  F3_Rtcurrent_nep=F3_current_nep+180
  F4_Rtcurrent_nep=F3_current_nep+180
  DC_Rtcurrent_nep=DC_current_nep+180    
  HCC_Rtcurrent_nep=HCC_current_nep+180     
  LT1_Rtcurrent_nep=LT1_current_nep+180   
  LT2_Rtcurrent_nep=LT2_current_nep+180     
  # OST
  HCC_current_ost=HCC_current+720
  DC_current_ost=DC_current+720
  LT1_current_ost=LT1_current+720
  LT2_current_ost=LT2_current+720
  F0_Rtcurrent_ost=F0_current_ost+180
  F1_Rtcurrent_ost=F1_current_ost+180
  F2_Rtcurrent_ost=F2_current_ost+180
  F3_Rtcurrent_ost=F3_current_ost+180
  F4_Rtcurrent_ost=F3_current_ost+180
  DC_Rtcurrent_ost=DC_current_ost+180    
  HCC_Rtcurrent_ost=HCC_current_ost+180     
  LT1_Rtcurrent_ost=LT1_current_ost+180   
  LT2_Rtcurrent_ost=LT2_current_ost+180   
  # nep_ost
  HCC_current_nep_ost=HCC_current+1080
  DC_current_nep_ost=DC_current+1080
  LT1_current_nep_ost=LT1_current+1080
  LT2_current_nep_ost=LT2_current+1080
  F0_Rtcurrent_nep_ost=F0_current_nep_ost+180
  F1_Rtcurrent_nep_ost=F1_current_nep_ost+180
  F2_Rtcurrent_nep_ost=F2_current_nep_ost+180
  F3_Rtcurrent_nep_ost=F3_current_nep_ost+180
  F4_Rtcurrent_nep_ost=F3_current_nep_ost+180
  DC_Rtcurrent_nep_ost=DC_current_nep_ost+180    
  HCC_Rtcurrent_nep_ost=HCC_current_nep_ost+180     
  LT1_Rtcurrent_nep_ost=LT1_current_nep_ost+180   
  LT2_Rtcurrent_nep_ost=LT2_current_nep_ost+180       
  
  # # The totals number of treatements is say 50/1000
  # dum=-log(1-50/1000)
  # ntreat_all=(1-exp(-dum))*totPWID
  # # divide this proportionally into each compartment
  # snapshot=c(sum(X[F0_current]),sum(X[F1_current]),sum(X[F2_current]),sum(X[F3_current]),sum(X[F4_current]),
  #            sum(X[DC_current]),sum(X[HCC_current]),sum(X[LT1_current]),sum(X[LT2_current]),
  #            sum(X[F0_former]),sum(X[F1_former]),sum(X[F2_former]),sum(X[F3_former]),sum(X[F4_former]),
  #            sum(X[DC_former]),sum(X[HCC_former]),sum(X[LT1_former]),sum(X[LT2_former]),
  #            sum(X[F0_Rtcurrent]),sum(X[F1_Rtcurrent]),sum(X[F2_Rtcurrent]),sum(X[F3_Rtcurrent]),sum(X[F4_Rtcurrent]),
  #            sum(X[DC_Rtcurrent]),sum(X[HCC_Rtcurrent]),sum(X[LT1_Rtcurrent]),sum(X[LT2_Rtcurrent]),
  #            sum(X[F0_Rtformer]),sum(X[F1_Rtformer]),sum(X[F2_Rtformer]),sum(X[F3_Rtformer]),sum(X[F4_Rtformer]),
  #            sum(X[DC_Rtformer]),sum(X[HCC_Rtformer]),sum(X[LT1_Rtformer]),sum(X[LT2_Rtformer]))
  # multy=c(rep(scale10,9),rep(scale00,9),rep(scale10,9),rep(scale00,9))
  
  ntreats_attributable=rep(rep(ntreat_nmild,4),)#ntreat_all*snapshot/(sum(snapshot))
 
  ntreats_current=c(rep(ntreat_nmild,4),rep(ntreat_nadva,5))
  ntreats_former=c(rep(ntreat_former_nmild,4),rep(ntreat_former_nadva,5))
  ntreats_Rtcurrent=c(rep(ntreat_nmild,4),rep(ntreat_nadva,5))
  ntreats_Rtformer=c(rep(ntreat_former_nmild,4),rep(ntreat_former_nadva,5))
  
  # # groups are
  # # current F0,F1,F2,F3,F4,DC,HCC,LT1,LT2
  # ntreats_current=c(ntreat_nmild,ntreat_nmild,ntreat_nmild,ntreat_nmild,
  #                   rep(ntreat_nadva,5))
  # # former DC,HCC,LT1,LT2
  # ntreats_former=c(rep(ntreat_former_mild,4),rep(ntreat_former_adva,5))
  # ntreats_Rtformer=ntreats_former
  # ntreats_Rtcurrent=ntreats_current
  
  phi=rep(0,1800)
  
  if (SCreening$FixedyCov==0){
    # ignores prevention measures
    if (sum(X[F0_current])>0){
      nDistributedTreats=min(ntreats_current[1],sum(X[F0_current]))
      phi[F0_current]=nDistributedTreats*(X[F0_current]/sum(X[F0_current]))
    }
    if (sum(X[F1_current])>0){
      nDistributedTreats=min(ntreats_current[2],sum(X[F1_current]))
      phi[F1_current]=nDistributedTreats*(X[F1_current]/sum(X[F1_current]))
    }
    if (sum(X[F2_current])>0){
      nDistributedTreats=min(ntreats_current[3],sum(X[F2_current]))
      phi[F2_current]=nDistributedTreats*(X[F2_current]/sum(X[F2_current]))
    }    
    if (sum(X[F3_current])>0){
      nDistributedTreats=min(ntreats_current[4],sum(X[F3_current]))
      phi[F3_current]=nDistributedTreats*(X[F3_current]/sum(X[F3_current]))
    }        
    if (sum(X[F4_current])>0){
      nDistributedTreats=min(ntreats_current[5],sum(X[F4_current]))
      phi[F4_current]=nDistributedTreats*(X[F4_current]/sum(X[F4_current]))
    }     
    if (sum(X[HCC_current])>0){
      nDistributedTreats=min(ntreats_current[6],sum(X[HCC_current]))
      phi[HCC_current]=nDistributedTreats*(X[HCC_current]/sum(X[HCC_current]))
    }   
    if (sum(X[DC_current])>0){
      nDistributedTreats=min(ntreats_current[7],sum(X[DC_current]))
      phi[DC_current]=nDistributedTreats*(X[DC_current]/sum(X[DC_current]))
    }      
    if (sum(X[LT1_current])>0){
      nDistributedTreats=min(ntreats_current[8],sum(X[LT1_current]))
      phi[LT1_current]=nDistributedTreats*(X[LT1_current]/sum(X[LT1_current]))
    }     
    if (sum(X[LT2_current])>0){
      nDistributedTreats=min(ntreats_current[9],sum(X[LT2_current]))
      phi[LT2_current]=nDistributedTreats*(X[LT2_current]/sum(X[LT2_current]))
    }      
    # former
    if (sum(X[F0_former])>0){
      nDistributedTreats=min(ntreats_former[1],sum(X[F0_former]))
      phi[F0_former]=nDistributedTreats*(X[F0_former]/sum(X[F0_former]))
    }
    if (sum(X[F1_former])>0){
      nDistributedTreats=min(ntreats_former[2],sum(X[F1_former]))
      phi[F1_former]=nDistributedTreats*(X[F1_former]/sum(X[F1_former]))
    }
    if (sum(X[F2_former])>0){
      nDistributedTreats=min(ntreats_former[3],sum(X[F2_former]))
      phi[F2_former]=nDistributedTreats*(X[F2_former]/sum(X[F2_former]))
    }    
    if (sum(X[F3_former])>0){
      nDistributedTreats=min(ntreats_former[4],sum(X[F3_former]))
      phi[F3_former]=nDistributedTreats*(X[F3_former]/sum(X[F3_former]))
    }        
    if (sum(X[F4_former])>0){
      nDistributedTreats=min(ntreats_former[5],sum(X[F4_former]))
      phi[F4_former]=nDistributedTreats*(X[F4_former]/sum(X[F4_former]))
    }     
    if (sum(X[HCC_former])>0){
      nDistributedTreats=min(ntreats_former[6],sum(X[HCC_former]))
      phi[HCC_former]=nDistributedTreats*(X[HCC_former]/sum(X[HCC_former]))
    }   
    if (sum(X[DC_former])>0){
      nDistributedTreats=min(ntreats_former[7],sum(X[DC_former]))
      phi[DC_former]=nDistributedTreats*(X[DC_former]/sum(X[DC_former]))
    }      
    if (sum(X[LT1_former])>0){
      nDistributedTreats=min(ntreats_former[8],sum(X[LT1_former]))
      phi[LT1_former]=nDistributedTreats*(X[LT1_former]/sum(X[LT1_former]))
    }     
    if (sum(X[LT2_former])>0){
      nDistributedTreats=min(ntreats_former[9],sum(X[LT2_former]))
      phi[LT2_former]=nDistributedTreats*(X[LT2_former]/sum(X[LT2_former]))
    }      
    
    # retreats

    if (sum(X[F0_Rtcurrent])>0){
      nDistributedTreats=min(ntreats_Rtcurrent[1],sum(X[F0_Rtcurrent]))
      phi[F0_Rtcurrent]=nDistributedTreats*(X[F0_Rtcurrent]/sum(X[F0_Rtcurrent]))
    }
    if (sum(X[F1_Rtcurrent])>0){
      nDistributedTreats=min(ntreats_Rtcurrent[2],sum(X[F1_Rtcurrent]))
      phi[F1_Rtcurrent]=nDistributedTreats*(X[F1_Rtcurrent]/sum(X[F1_Rtcurrent]))
    }
    if (sum(X[F2_Rtcurrent])>0){
      nDistributedTreats=min(ntreats_Rtcurrent[3],sum(X[F2_Rtcurrent]))
      phi[F2_Rtcurrent]=nDistributedTreats*(X[F2_Rtcurrent]/sum(X[F2_Rtcurrent]))
    }    
    if (sum(X[F3_Rtcurrent])>0){
      nDistributedTreats=min(ntreats_Rtcurrent[4],sum(X[F3_Rtcurrent]))
      phi[F3_Rtcurrent]=nDistributedTreats*(X[F3_Rtcurrent]/sum(X[F3_Rtcurrent]))
    }        
    if (sum(X[F4_Rtcurrent])>0){
      nDistributedTreats=min(ntreats_Rtcurrent[5],sum(X[F4_Rtcurrent]))
      phi[F4_Rtcurrent]=nDistributedTreats*(X[F4_Rtcurrent]/sum(X[F4_Rtcurrent]))
    }     
    if (sum(X[HCC_Rtcurrent])>0){
      nDistributedTreats=min(ntreats_Rtcurrent[6],sum(X[HCC_Rtcurrent]))
      phi[HCC_Rtcurrent]=nDistributedTreats*(X[HCC_Rtcurrent]/sum(X[HCC_Rtcurrent]))
    }   
    if (sum(X[DC_Rtcurrent])>0){
      nDistributedTreats=min(ntreats_Rtcurrent[7],sum(X[DC_Rtcurrent]))
      phi[DC_Rtcurrent]=nDistributedTreats*(X[DC_Rtcurrent]/sum(X[DC_Rtcurrent]))
    }      
    if (sum(X[LT1_Rtcurrent])>0){
      nDistributedTreats=min(ntreats_Rtcurrent[8],sum(X[LT1_Rtcurrent]))
      phi[LT1_Rtcurrent]=nDistributedTreats*(X[LT1_Rtcurrent]/sum(X[LT1_Rtcurrent]))
    }     
    if (sum(X[LT2_Rtcurrent])>0){
      nDistributedTreats=min(ntreats_Rtcurrent[9],sum(X[LT2_Rtcurrent]))
      phi[LT2_Rtcurrent]=nDistributedTreats*(X[LT2_Rtcurrent]/sum(X[LT2_Rtcurrent]))
    }      
    if (sum(X[F0_Rtformer])>0){
      nDistributedTreats=min(ntreats_Rtformer[1],sum(X[F0_Rtformer]))
      phi[F0_Rtformer]=nDistributedTreats*(X[F0_Rtformer]/sum(X[F0_Rtformer]))
    }
    if (sum(X[F1_Rtformer])>0){
      nDistributedTreats=min(ntreats_Rtformer[2],sum(X[F1_Rtformer]))
      phi[F1_Rtformer]=nDistributedTreats*(X[F1_Rtformer]/sum(X[F1_Rtformer]))
    }
    if (sum(X[F2_Rtformer])>0){
      nDistributedTreats=min(ntreats_Rtformer[3],sum(X[F2_Rtformer]))
      phi[F2_Rtformer]=nDistributedTreats*(X[F2_Rtformer]/sum(X[F2_Rtformer]))
    }    
    if (sum(X[F3_Rtformer])>0){
      nDistributedTreats=min(ntreats_Rtformer[4],sum(X[F3_Rtformer]))
      phi[F3_Rtformer]=nDistributedTreats*(X[F3_Rtformer]/sum(X[F3_Rtformer]))
    }        
    if (sum(X[F4_Rtformer])>0){
      nDistributedTreats=min(ntreats_Rtformer[5],sum(X[F4_Rtformer]))
      phi[F4_Rtformer]=nDistributedTreats*(X[F4_Rtformer]/sum(X[F4_Rtformer]))
    }     
    if (sum(X[HCC_Rtformer])>0){
      nDistributedTreats=min(ntreats_Rtformer[6],sum(X[HCC_Rtformer]))
      phi[HCC_Rtformer]=nDistributedTreats*(X[HCC_Rtformer]/sum(X[HCC_Rtformer]))
    }   
    if (sum(X[DC_Rtformer])>0){
      nDistributedTreats=min(ntreats_Rtformer[7],sum(X[DC_Rtformer]))
      phi[DC_Rtformer]=nDistributedTreats*(X[DC_Rtformer]/sum(X[DC_Rtformer]))
    }      
    if (sum(X[LT1_Rtformer])>0){
      nDistributedTreats=min(ntreats_Rtformer[8],sum(X[LT1_Rtformer]))
      phi[LT1_Rtformer]=nDistributedTreats*(X[LT1_Rtformer]/sum(X[LT1_Rtformer]))
    }     
    if (sum(X[LT2_Rtformer])>0){
      nDistributedTreats=min(ntreats_Rtformer[9],sum(X[LT2_Rtformer]))
      phi[LT2_Rtformer]=nDistributedTreats*(X[LT2_Rtformer]/sum(X[LT2_Rtformer]))
    }      
    # don't treat former F0-F4
    # phi[F0_Rtformer] =0
    # phi[F1_Rtformer] =0
    # phi[F2_Rtformer] =0
    # phi[F3_Rtformer] =0
    # phi[F4_Rtformer] =0
    # 
    # phi[F0_former] =0
    # phi[F1_former] =0
    # phi[F2_former] =0
    # phi[F3_former] =0
    # phi[F4_former] =0    
      #print(sum(phi))
  } else {
    # N_Mildcurrent_nep is the former F0,F1,F2 * rate of NEP, i.e. the number of subjects into the NEP compartments
    # these are then diagnosed at rate of 1/SCreening$Ditimedur, so that with prob =(1-exp(-1/SCreening$Ditimedur)
    # these ar ethe subjects that are then treated
    # Note Mild_current_nep etc has already be multiplied by enrollment
    
    # 5 July
    # For scenario 1
    # run with z7=runall_intervention_vnsp_ost_long_v3(-log(1-0.1),-log(1-0.1),"No",-log(1-0.5),-log(1-0.5),0,1/12,1,52/8)
    # approx 500,000 available for mild and advanced
    # Note this high because 100/1000 of PWID (this includes susceptible population)
    # This gives 250,000 (exact number is 260,241.9) for OST and NEP mild - assuming 50% for OST and 50% for NSP
    # Same for advanced
    # However there are not this many advanced to treat - around 130000
    # SO total treats for first year is 2*(260,241.9+129,375.9) = 779235.6
    
    # compare to only NSP                                                                                  note OST set to zero
    # so run with z7=runall_intervention_vnsp_ost_long_v3(-log(1-0.1),-log(1-0.1),"No",-log(1-0.5),-log(1-0.0),0,1/12,1,52/8)
    # Mild_current_nep*scale10 = 1,341,115
    # ntreat_nmild*scale10 = 521,230
    # mild_treat*scale10 = (521,230  ,    0.0   ,   0.0)
    # Treats for mild then = min(1,341,115 and 521,230)
    # 
    # For advanced mult_N_Ad_current_nep*scale10 = 126,172
    # 
    # So total test = 647,403
    
    # so the high number in the mild can't all be treated limited by 100/1000
    # Could add allocation schemes to keep treatment allocation constant

    # DAA selection * enrollment * time to diagnosis
    #ntreats_current_nep = ntreats_current*(1-exp(-1/enroll_nep))*(1-exp(-1/SCreening$Ditimedur))
    #ntreats_Rtcurrent_nep = ntreats_Rtcurrent*(1-exp(-1/enroll_nep))*(1-exp(-1/SCreening$Ditimedur))
    
    # ntreats_current_nep = ntreats_current*(1-exp(-enroll_nep))*(1-exp(-1/SCreening$Ditimedur))
    # ntreats_Rtcurrent_nep = ntreats_Rtcurrent*(1-exp(-enroll_nep))*(1-exp(-1/SCreening$Ditimedur))
    
    # Note don't need nep rate here as it is already accounted for as they have been moved to NEP compartments at this stage
    # treatment
    # ntreats_current_nep = ntreats_current*(1-exp(-1/SCreening$Ditimedur))
    # ntreats_Rtcurrent_nep = ntreats_Rtcurrent*(1-exp(-1/SCreening$Ditimedur))
    
    ntreats_current_nep = ntreats_current
    ntreats_Rtcurrent_nep = ntreats_Rtcurrent
    q=(1-exp(-1/SCreening$Ditimedur))
    
    if (sum(X[F0_current_nep])>0){
      nDistributedTreats=min(ntreats_current_nep[1],q*sum(X[F0_current_nep]))
      phi[F0_current_nep]=nDistributedTreats*(X[F0_current_nep]/sum(X[F0_current_nep]))
      }
    if (sum(X[F1_current_nep])>0){
      nDistributedTreats=min(ntreats_current_nep[2],q*sum(X[F1_current_nep]))
      phi[F1_current_nep]=nDistributedTreats*(X[F1_current_nep]/sum(X[F1_current_nep]))
    }
    if (sum(X[F2_current_nep])>0){
      nDistributedTreats=min(ntreats_current_nep[3],q*sum(X[F2_current_nep]))
      phi[F2_current_nep]=nDistributedTreats*(X[F2_current_nep]/sum(X[F2_current_nep]))
    }   
    if (sum(X[F3_current_nep])>0){
      nDistributedTreats=min(ntreats_current_nep[4],q*sum(X[F3_current_nep]))
      phi[F3_current_nep]=nDistributedTreats*(X[F3_current_nep]/sum(X[F3_current_nep]))
    }      
    if (sum(X[F4_current_nep])>0){
      nDistributedTreats=min(ntreats_current_nep[5],q*sum(X[F4_current_nep]))
      phi[F4_current_nep]=nDistributedTreats*(X[F4_current_nep]/sum(X[F4_current_nep]))
    }  
    if (sum(X[DC_current_nep])>0){
      nDistributedTreats=min(ntreats_current_nep[6],q*sum(X[DC_current_nep]))
      phi[DC_current_nep]=nDistributedTreats*(X[DC_current_nep]/sum(X[DC_current_nep]))
    }     
    if (sum(X[HCC_current_nep])>0){
      nDistributedTreats=min(ntreats_current_nep[7],q*sum(X[HCC_current_nep]))
      phi[HCC_current_nep]=nDistributedTreats*(X[HCC_current_nep]/sum(X[HCC_current_nep]))
    }  
    if (sum(X[LT1_current_nep])>0){
      nDistributedTreats=min(ntreats_current_nep[8],q*sum(X[LT1_current_nep]))
      phi[LT1_current_nep]=nDistributedTreats*(X[LT1_current_nep]/sum(X[LT1_current_nep]))
    }      
    if (sum(X[LT2_current_nep])>0){
      nDistributedTreats=min(ntreats_current_nep[9],q*sum(X[LT2_current_nep]))
      phi[LT2_current_nep]=nDistributedTreats*(X[LT2_current_nep]/sum(X[LT2_current_nep]))
    }  
    
    # retreats
    if (sum(X[F0_Rtcurrent_nep])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep[1],q*sum(X[F0_Rtcurrent_nep]))
      phi[F0_Rtcurrent_nep]=nDistributedTreats*(X[F0_Rtcurrent_nep]/sum(X[F0_Rtcurrent_nep]))
    }
    if (sum(X[F1_Rtcurrent_nep])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep[2],q*sum(X[F1_Rtcurrent_nep]))
      phi[F1_Rtcurrent_nep]=nDistributedTreats*(X[F1_Rtcurrent_nep]/sum(X[F1_Rtcurrent_nep]))
    }
    if (sum(X[F2_Rtcurrent_nep])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep[3],q*sum(X[F2_Rtcurrent_nep]))
      phi[F2_Rtcurrent_nep]=nDistributedTreats*(X[F2_Rtcurrent_nep]/sum(X[F2_Rtcurrent_nep]))
    }   
    if (sum(X[F3_Rtcurrent_nep])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep[4],q*sum(X[F3_Rtcurrent_nep]))
      phi[F3_Rtcurrent_nep]=nDistributedTreats*(X[F3_Rtcurrent_nep]/sum(X[F3_Rtcurrent_nep]))
    }      
    if (sum(X[F4_Rtcurrent_nep])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep[5],q*sum(X[F4_Rtcurrent_nep]))
      phi[F4_Rtcurrent_nep]=nDistributedTreats*(X[F4_Rtcurrent_nep]/sum(X[F4_Rtcurrent_nep]))
    }  
    if (sum(X[DC_Rtcurrent_nep])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep[6],q*sum(X[DC_Rtcurrent_nep]))
      phi[DC_Rtcurrent_nep]=nDistributedTreats*(X[DC_Rtcurrent_nep]/sum(X[DC_Rtcurrent_nep]))
    }     
    if (sum(X[HCC_Rtcurrent_nep])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep[7],q*sum(X[HCC_Rtcurrent_nep]))
      phi[HCC_Rtcurrent_nep]=nDistributedTreats*(X[HCC_Rtcurrent_nep]/sum(X[HCC_Rtcurrent_nep]))
    }  
    if (sum(X[LT1_Rtcurrent_nep])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep[8],q*sum(X[LT1_Rtcurrent_nep]))
      phi[LT1_Rtcurrent_nep]=nDistributedTreats*(X[LT1_Rtcurrent_nep]/sum(X[LT1_Rtcurrent_nep]))
    }      
    if (sum(X[LT2_Rtcurrent_nep])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep[9],q*sum(X[LT2_Rtcurrent_nep]))
      phi[LT2_Rtcurrent_nep]=nDistributedTreats*(X[LT2_Rtcurrent_nep]/sum(X[LT2_Rtcurrent_nep]))
    }  


    # DAA selection * enrollment * time to diagnosis
    #ntreats_current_ost = ntreats_current*(1-exp(-enroll_ost))*(1-exp(-1/SCreening$Ditimedur))
    #ntreats_Rtcurrent_ost = ntreats_Rtcurrent*(1-exp(-enroll_ost))*(1-exp(-1/SCreening$Ditimedur))
    ntreats_current_ost = ntreats_current
    ntreats_Rtcurrent_ost = ntreats_Rtcurrent
    
    if (sum(X[F0_current_ost])>0){
      nDistributedTreats=min(ntreats_current_ost[1],q*sum(X[F0_current_ost]))
      phi[F0_current_ost]=nDistributedTreats*(X[F0_current_ost]/sum(X[F0_current_ost]))
    }
    if (sum(X[F1_current_ost])>0){
      nDistributedTreats=min(ntreats_current_ost[2],q*sum(X[F1_current_ost]))
      phi[F1_current_ost]=nDistributedTreats*(X[F1_current_ost]/sum(X[F1_current_ost]))
    }
    if (sum(X[F2_current_ost])>0){
      nDistributedTreats=min(ntreats_current_ost[3],q*sum(X[F2_current_ost]))
      phi[F2_current_ost]=nDistributedTreats*(X[F2_current_ost]/sum(X[F2_current_ost]))
    }   
    if (sum(X[F3_current_ost])>0){
      nDistributedTreats=min(ntreats_current_ost[4],q*sum(X[F3_current_ost]))
      phi[F3_current_ost]=nDistributedTreats*(X[F3_current_ost]/sum(X[F3_current_ost]))
    }      
    if (sum(X[F4_current_ost])>0){
      nDistributedTreats=min(ntreats_current_ost[5],q*sum(X[F4_current_ost]))
      phi[F4_current_ost]=nDistributedTreats*(X[F4_current_ost]/sum(X[F4_current_ost]))
    }  
    if (sum(X[DC_current_ost])>0){
      nDistributedTreats=min(ntreats_current_ost[6],q*sum(X[DC_current_ost]))
      phi[DC_current_ost]=nDistributedTreats*(X[DC_current_ost]/sum(X[DC_current_ost]))
    }     
    if (sum(X[HCC_current_ost])>0){
      nDistributedTreats=min(ntreats_current_ost[7],q*sum(X[HCC_current_ost]))
      phi[HCC_current_ost]=nDistributedTreats*(X[HCC_current_ost]/sum(X[HCC_current_ost]))
    }  
    if (sum(X[LT1_current_ost])>0){
      nDistributedTreats=min(ntreats_current_ost[8],q*sum(X[LT1_current_ost]))
      phi[LT1_current_ost]=nDistributedTreats*(X[LT1_current_ost]/sum(X[LT1_current_ost]))
    }      
    if (sum(X[LT2_current_ost])>0){
      nDistributedTreats=min(ntreats_current_ost[9],q*sum(X[LT2_current_ost]))
      phi[LT2_current_ost]=nDistributedTreats*(X[LT2_current_ost]/sum(X[LT2_current_ost]))
    }  
    
    # retreats
    if (sum(X[F0_Rtcurrent_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_ost[1],q*sum(X[F0_Rtcurrent_ost]))
      phi[F0_Rtcurrent_ost]=nDistributedTreats*(X[F0_Rtcurrent_ost]/sum(X[F0_Rtcurrent_ost]))
    }
    if (sum(X[F1_Rtcurrent_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_ost[2],q*sum(X[F1_Rtcurrent_ost]))
      phi[F1_Rtcurrent_ost]=nDistributedTreats*(X[F1_Rtcurrent_ost]/sum(X[F1_Rtcurrent_ost]))
    }
    if (sum(X[F2_Rtcurrent_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_ost[3],q*sum(X[F2_Rtcurrent_ost]))
      phi[F2_Rtcurrent_ost]=nDistributedTreats*(X[F2_Rtcurrent_ost]/sum(X[F2_Rtcurrent_ost]))
    }   
    if (sum(X[F3_Rtcurrent_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_ost[4],q*sum(X[F3_Rtcurrent_ost]))
      phi[F3_Rtcurrent_ost]=nDistributedTreats*(X[F3_Rtcurrent_ost]/sum(X[F3_Rtcurrent_ost]))
    }      
    if (sum(X[F4_Rtcurrent_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_ost[5],q*sum(X[F4_Rtcurrent_ost]))
      phi[F4_Rtcurrent_ost]=nDistributedTreats*(X[F4_Rtcurrent_ost]/sum(X[F4_Rtcurrent_ost]))
    }  
    if (sum(X[DC_Rtcurrent_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_ost[6],q*sum(X[DC_Rtcurrent_ost]))
      phi[DC_Rtcurrent_ost]=nDistributedTreats*(X[DC_Rtcurrent_ost]/sum(X[DC_Rtcurrent_ost]))
    }     
    if (sum(X[HCC_Rtcurrent_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_ost[7],q*sum(X[HCC_Rtcurrent_ost]))
      phi[HCC_Rtcurrent_ost]=nDistributedTreats*(X[HCC_Rtcurrent_ost]/sum(X[HCC_Rtcurrent_ost]))
    }  
    if (sum(X[LT1_Rtcurrent_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_ost[8],q*sum(X[LT1_Rtcurrent_ost]))
      phi[LT1_Rtcurrent_ost]=nDistributedTreats*(X[LT1_Rtcurrent_ost]/sum(X[LT1_Rtcurrent_ost]))
    }      
    if (sum(X[LT2_Rtcurrent_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_ost[9],q*sum(X[LT2_Rtcurrent_ost]))
      phi[LT2_Rtcurrent_ost]=nDistributedTreats*(X[LT2_Rtcurrent_ost]/sum(X[LT2_Rtcurrent_ost]))
    } 
    

    # DAA selection * enrollment * time to diagnosis
    #ntreats_current_nep_ost = ntreats_current*(1-exp(-enroll_nep_ost))*(1-exp(-1/SCreening$Ditimedur))
    #ntreats_Rtcurrent_nep_ost = ntreats_Rtcurrent*(1-exp(-enroll_nep_ost))*(1-exp(-1/SCreening$Ditimedur))
    ntreats_current_nep_ost = ntreats_current
    ntreats_Rtcurrent_nep_ost = ntreats_Rtcurrent
    if (sum(X[F0_current_nep_ost])>0){
      nDistributedTreats=min(ntreats_current_nep_ost[1],q*sum(X[F0_current_nep_ost]))
      phi[F0_current_nep_ost]=nDistributedTreats*(X[F0_current_nep_ost]/sum(X[F0_current_nep_ost]))
    }
    if (sum(X[F1_current_nep_ost])>0){
      nDistributedTreats=min(ntreats_current_nep_ost[2],q*sum(X[F1_current_nep_ost]))
      phi[F1_current_nep_ost]=nDistributedTreats*(X[F1_current_nep_ost]/sum(X[F1_current_nep_ost]))
    }
    if (sum(X[F2_current_nep_ost])>0){
      nDistributedTreats=min(ntreats_current_nep_ost[3],q*sum(X[F2_current_nep_ost]))
      phi[F2_current_nep_ost]=nDistributedTreats*(X[F2_current_nep_ost]/sum(X[F2_current_nep_ost]))
    }   
    if (sum(X[F3_current_nep_ost])>0){
      nDistributedTreats=min(ntreats_current_nep_ost[4],q*sum(X[F3_current_nep_ost]))
      phi[F3_current_nep_ost]=nDistributedTreats*(X[F3_current_nep_ost]/sum(X[F3_current_nep_ost]))
    }      
    if (sum(X[F4_current_nep_ost])>0){
      nDistributedTreats=min(ntreats_current_nep_ost[5],q*sum(X[F4_current_nep_ost]))
      phi[F4_current_nep_ost]=nDistributedTreats*(X[F4_current_nep_ost]/sum(X[F4_current_nep_ost]))
    }  
    if (sum(X[DC_current_nep_ost])>0){
      nDistributedTreats=min(ntreats_current_nep_ost[6],q*sum(X[DC_current_nep_ost]))
      phi[DC_current_nep_ost]=nDistributedTreats*(X[DC_current_nep_ost]/sum(X[DC_current_nep_ost]))
    }     
    if (sum(X[HCC_current_nep_ost])>0){
      nDistributedTreats=min(ntreats_current_nep_ost[7],q*sum(X[HCC_current_nep_ost]))
      phi[HCC_current_nep_ost]=nDistributedTreats*(X[HCC_current_nep_ost]/sum(X[HCC_current_nep_ost]))
    }  
    if (sum(X[LT1_current_nep_ost])>0){
      nDistributedTreats=min(ntreats_current_nep_ost[8],q*sum(X[LT1_current_nep_ost]))
      phi[LT1_current_nep_ost]=nDistributedTreats*(X[LT1_current_nep_ost]/sum(X[LT1_current_nep_ost]))
    }      
    if (sum(X[LT2_current_nep_ost])>0){
      nDistributedTreats=min(ntreats_current_nep_ost[9],q*sum(X[LT2_current_nep_ost]))
      phi[LT2_current_nep_ost]=nDistributedTreats*(X[LT2_current_nep_ost]/sum(X[LT2_current_nep_ost]))
    }  
    
    # retreats
    if (sum(X[F0_Rtcurrent_nep_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep_ost[1],q*sum(X[F0_Rtcurrent_nep_ost]))
      phi[F0_Rtcurrent_nep_ost]=nDistributedTreats*(X[F0_Rtcurrent_nep_ost]/sum(X[F0_Rtcurrent_nep_ost]))
    }
    if (sum(X[F1_Rtcurrent_nep_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep_ost[2],q*sum(X[F1_Rtcurrent_nep_ost]))
      phi[F1_Rtcurrent_nep_ost]=nDistributedTreats*(X[F1_Rtcurrent_nep_ost]/sum(X[F1_Rtcurrent_nep_ost]))
    }
    if (sum(X[F2_Rtcurrent_nep_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep_ost[3],q*sum(X[F2_Rtcurrent_nep_ost]))
      phi[F2_Rtcurrent_nep_ost]=nDistributedTreats*(X[F2_Rtcurrent_nep_ost]/sum(X[F2_Rtcurrent_nep_ost]))
    }   
    if (sum(X[F3_Rtcurrent_nep_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep_ost[4],q*sum(X[F3_Rtcurrent_nep_ost]))
      phi[F3_Rtcurrent_nep_ost]=nDistributedTreats*(X[F3_Rtcurrent_nep_ost]/sum(X[F3_Rtcurrent_nep_ost]))
    }      
    if (sum(X[F4_Rtcurrent_nep_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep_ost[5],q*sum(X[F4_Rtcurrent_nep_ost]))
      phi[F4_Rtcurrent_nep_ost]=nDistributedTreats*(X[F4_Rtcurrent_nep_ost]/sum(X[F4_Rtcurrent_nep_ost]))
    }  
    if (sum(X[DC_Rtcurrent_nep_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep_ost[6],q*sum(X[DC_Rtcurrent_nep_ost]))
      phi[DC_Rtcurrent_nep_ost]=nDistributedTreats*(X[DC_Rtcurrent_nep_ost]/sum(X[DC_Rtcurrent_nep_ost]))
    }     
    if (sum(X[HCC_Rtcurrent_nep_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep_ost[7],q*sum(X[HCC_Rtcurrent_nep_ost]))
      phi[HCC_Rtcurrent_nep_ost]=nDistributedTreats*(X[HCC_Rtcurrent_nep_ost]/sum(X[HCC_Rtcurrent_nep_ost]))
    }  
    if (sum(X[LT1_Rtcurrent_nep_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep_ost[8],q*sum(X[LT1_Rtcurrent_nep_ost]))
      phi[LT1_Rtcurrent_nep_ost]=nDistributedTreats*(X[LT1_Rtcurrent_nep_ost]/sum(X[LT1_Rtcurrent_nep_ost]))
    }      
    if (sum(X[LT2_Rtcurrent_nep_ost])>0){
      nDistributedTreats=min(ntreats_Rtcurrent_nep_ost[9],q*sum(X[LT2_Rtcurrent_nep_ost]))
      phi[LT2_Rtcurrent_nep_ost]=nDistributedTreats*(X[LT2_Rtcurrent_nep_ost]/sum(X[LT2_Rtcurrent_nep_ost]))
    } 
 
  }
  
  phidash=rep(0,1800);
  T0_current = seq(376,540,20);
  T1_current = seq(377,540,20);
  T2_current = seq(378,540,20);
  T3_current = seq(379,540,20);
  T4_current = seq(380,540,20);
  
  T0_former = seq(16,180,20);
  T1_former = seq(17,180,20);
  T2_former = seq(18,180,20);
  T3_former = seq(19,180,20);
  T4_former = seq(20,180,20);
  
  T0_Rtcurrent = 180 + seq(376,540,20);
  T1_Rtcurrent = 180 + seq(377,540,20);
  T2_Rtcurrent = 180 + seq(378,540,20);
  T3_Rtcurrent = 180 + seq(379,540,20);
  T4_Rtcurrent = 180 + seq(380,540,20);
  
  T0_Rtformer = 180 + seq(16,180,20);
  T1_Rtformer = 180 + seq(17,180,20);
  T2_Rtformer = 180 + seq(18,180,20);
  T3_Rtformer = 180 + seq(19,180,20);
  T4_Rtformer = 180 + seq(20,180,20);
  
  T0_current_nep = 360+seq(376,540,20);
  T1_current_nep = 360+seq(377,540,20);
  T2_current_nep = 360+seq(378,540,20);
  T3_current_nep = 360+seq(379,540,20);
  T4_current_nep = 360+seq(380,540,20);
  
  T0_current_ost = 720+seq(376,540,20);
  T1_current_ost = 720+seq(377,540,20);
  T2_current_ost = 720+seq(378,540,20);
  T3_current_ost = 720+seq(379,540,20);
  T4_current_ost = 720+seq(380,540,20);
  
  T0_current_nep_ost = 1080+seq(376,540,20);
  T1_current_nep_ost = 1080+seq(377,540,20);
  T2_current_nep_ost = 1080+seq(378,540,20);
  T3_current_nep_ost = 1080+seq(379,540,20);
  T4_current_nep_ost = 1080+seq(380,540,20);
  
  phidash[T0_current]=alpha*pcom*phi[F0_current];
  phidash[T1_current]=alpha*pcom*phi[F1_current];
  phidash[T2_current]=alpha*pcom*phi[F2_current];
  phidash[T3_current]=alpha*pcom*phi[F3_current];
  
  phidash[T0_Rtcurrent]=alpha*pcom*phi[F0_Rtcurrent];
  phidash[T1_Rtcurrent]=alpha*pcom*phi[F1_Rtcurrent];
  phidash[T2_Rtcurrent]=alpha*pcom*phi[F2_Rtcurrent];
  phidash[T3_Rtcurrent]=alpha*pcom*phi[F3_Rtcurrent];

  pcom_former=pcom
  phidash[T0_former]=alpha*pcom_former*phi[F0_former];
  phidash[T1_former]=alpha*pcom_former*phi[F1_former];
  phidash[T2_former]=alpha*pcom_former*phi[F2_former];
  phidash[T3_former]=alpha*pcom_former*phi[F3_former];
  
  phidash[T0_Rtformer]=alpha*pcom_former*phi[F0_Rtformer];
  phidash[T1_Rtformer]=alpha*pcom_former*phi[F1_Rtformer];
  phidash[T2_Rtformer]=alpha*pcom_former*phi[F2_Rtformer];
  phidash[T3_Rtformer]=alpha*pcom_former*phi[F3_Rtformer];
  
  phidash[T4_current]=alpha*pcom*(phi[F4_current]+phi[HCC_current]+phi[DC_current]+phi[LT1_current]+phi[LT2_current])
  phidash[T4_former]=alpha*pcom_former*(phi[F4_former]+phi[HCC_former]+phi[DC_former]+phi[LT1_former]+phi[LT2_former])
  
  phidash[T4_Rtcurrent]=alpha*pcom*(phi[F4_Rtcurrent]+phi[HCC_Rtcurrent]+phi[DC_Rtcurrent]+phi[LT1_Rtcurrent]+phi[LT2_Rtcurrent])
  phidash[T4_Rtformer]=alpha*pcom_former*(phi[F4_Rtformer]+phi[HCC_Rtformer]+phi[DC_Rtformer]+phi[LT1_Rtformer]+phi[LT2_Rtformer])
  
  phidash[T0_current_nep]=alpha*pcom*phi[F0_current_nep];
  phidash[T1_current_nep]=alpha*pcom*phi[F1_current_nep];
  phidash[T2_current_nep]=alpha*pcom*phi[F2_current_nep];
  phidash[T3_current_nep]=alpha*pcom*phi[F3_current_nep];
  phidash[T4_current_nep]=alpha*pcom*(phi[F4_current_nep]);
  
  phidash[T0_current_ost]=alpha*pcom*phi[F0_current_ost];
  phidash[T1_current_ost]=alpha*pcom*phi[F1_current_ost];
  phidash[T2_current_ost]=alpha*pcom*phi[F2_current_ost];
  phidash[T3_current_ost]=alpha*pcom*phi[F3_current_ost];
  phidash[T4_current_ost]=alpha*pcom*(phi[F4_current_ost]);
  
  phidash[T0_current_nep_ost]=alpha*pcom*phi[F0_current_nep_ost];
  phidash[T1_current_nep_ost]=alpha*pcom*phi[F1_current_nep_ost];
  phidash[T2_current_nep_ost]=alpha*pcom*phi[F2_current_nep_ost];
  phidash[T3_current_nep_ost]=alpha*pcom*phi[F3_current_nep_ost];
  phidash[T4_current_nep_ost]=alpha*pcom*(phi[F4_current_nep_ost]);
  
  # These are used to count the number of treatments
   total_not_T4 = scale10*(phi[F0_current]+phi[F1_current]+phi[F2_current]+phi[F3_current]+
                           phi[F0_Rtcurrent]+phi[F1_Rtcurrent]+phi[F2_Rtcurrent]+phi[F3_Rtcurrent]+
                           phi[F0_current_nep]+phi[F1_current_nep]+phi[F2_current_nep]+phi[F3_current_nep]+
                           phi[F0_Rtcurrent_nep]+phi[F1_Rtcurrent_nep]+phi[F2_Rtcurrent_nep]+phi[F3_Rtcurrent_nep]+
                           phi[F0_current_ost]+phi[F1_current_ost]+phi[F2_current_ost]+phi[F3_current_ost]+
                           phi[F0_Rtcurrent_ost]+phi[F1_Rtcurrent_ost]+phi[F2_Rtcurrent_ost]+phi[F3_Rtcurrent_ost]+                            
                           phi[F0_current_nep_ost]+phi[F1_current_nep_ost]+phi[F2_current_nep_ost]+phi[F3_current_nep_ost]+
                           phi[F0_Rtcurrent_nep_ost]+phi[F1_Rtcurrent_nep_ost]+phi[F2_Rtcurrent_nep_ost]+phi[F3_Rtcurrent_nep_ost])+                   
                  scale00*(phi[F0_former]+phi[F1_former]+phi[F2_former]+phi[F3_former]+
                           phi[F0_Rtformer]+phi[F1_Rtformer]+phi[F2_Rtformer]+phi[F3_Rtformer])
  
  total_T4=scale10*(phi[F4_current]+phi[DC_current]+phi[HCC_current]+phi[LT1_current]+phi[LT2_current]+
                    phi[F4_Rtcurrent]+phi[DC_Rtcurrent]+phi[HCC_Rtcurrent]+phi[LT1_Rtcurrent]+phi[LT2_Rtcurrent]+  
                    phi[F4_current_nep]+phi[DC_current_nep]+phi[HCC_current_nep]+phi[LT1_current_nep]+phi[LT2_current_nep]+
                    phi[F4_Rtcurrent_nep]+phi[DC_Rtcurrent_nep]+phi[HCC_Rtcurrent_nep]+phi[LT1_Rtcurrent_nep]+phi[LT2_Rtcurrent_nep]+
                    phi[F4_current_ost]+phi[DC_current_ost]+phi[HCC_current_ost]+phi[LT1_current_ost]+phi[LT2_current_ost]+
                    phi[F4_Rtcurrent_ost]+phi[DC_Rtcurrent_ost]+phi[HCC_Rtcurrent_ost]+phi[LT1_Rtcurrent_ost]+phi[LT2_Rtcurrent_ost]+
                    phi[F4_current_nep_ost]+phi[DC_current_nep_ost]+phi[HCC_current_nep_ost]+phi[LT1_current_nep_ost]+phi[LT2_current_nep_ost]+
                    phi[F4_Rtcurrent_nep_ost]+phi[DC_Rtcurrent_nep_ost]+phi[HCC_Rtcurrent_nep_ost]+phi[LT1_Rtcurrent_nep_ost]+phi[LT2_Rtcurrent_nep_ost])+
           scale00*(phi[F4_former]+phi[DC_former]+phi[HCC_former]+phi[LT1_former]+phi[LT2_former]+
                    phi[F4_Rtformer]+phi[DC_Rtformer]+phi[HCC_Rtformer]+phi[LT1_Rtformer]+phi[LT2_Rtformer])
  # extract the treatments in the DC/HCC and LT compartments
  # need to add back in as no reduction in deaths for treating these compartments
  
  direct_deaths00=scale00*alpha*pcom_former*c(sum(phi[DC_former]),sum(phi[HCC_former]),sum(phi[LT1_former]),sum(phi[LT2_former]))
  direct_deaths01=scale00*alpha*pcom_former*c(sum(phi[DC_Rtformer]),sum(phi[HCC_Rtformer]),sum(phi[LT1_Rtformer]),sum(phi[LT2_Rtformer]))
  direct_deaths10=scale10*alpha*pcom*c(sum(phi[DC_current]),sum(phi[HCC_current]),sum(phi[LT1_current]),sum(phi[LT2_current]))
  direct_deaths11=scale10*alpha*pcom*c(sum(phi[DC_Rtcurrent]),sum(phi[HCC_Rtcurrent]),sum(phi[LT1_Rtcurrent]),sum(phi[LT2_Rtcurrent]))
  direct_deaths10_nep=scale10*alpha*pcom*c(sum(phi[DC_current_nep]),sum(phi[HCC_current_nep]),sum(phi[LT1_current_nep]),sum(phi[LT2_current_nep]))
  direct_deaths11_nep=scale10*alpha*pcom*c(sum(phi[DC_Rtcurrent_nep]),sum(phi[HCC_Rtcurrent_nep]),sum(phi[LT1_Rtcurrent_nep]),sum(phi[LT2_Rtcurrent_nep]))
  direct_deaths10_ost=scale10*alpha*pcom*c(sum(phi[DC_current_ost]),sum(phi[HCC_current_ost]),sum(phi[LT1_current_ost]),sum(phi[LT2_current_ost]))
  direct_deaths11_ost=scale10*alpha*pcom*c(sum(phi[DC_Rtcurrent_ost]),sum(phi[HCC_Rtcurrent_ost]),sum(phi[LT1_Rtcurrent_ost]),sum(phi[LT2_Rtcurrent_ost]))
  direct_deaths10_nep_ost=scale10*alpha*pcom*c(sum(phi[DC_current_nep_ost]),sum(phi[HCC_current_nep_ost]),sum(phi[LT1_current_nep_ost]),sum(phi[LT2_current_nep_ost]))
  direct_deaths11_nep_ost=scale10*alpha*pcom*c(sum(phi[DC_Rtcurrent_nep_ost]),sum(phi[HCC_Rtcurrent_nep_ost]),sum(phi[LT1_Rtcurrent_nep_ost]),sum(phi[LT2_Rtcurrent_nep_ost]))
  
  return(list(phi=phi,phidash=phidash,total_not_T4=total_not_T4,total_T4=total_T4,
              direct_deaths=rbind(direct_deaths00,direct_deaths01,direct_deaths10,direct_deaths11),
              direct_deaths_nep=rbind(direct_deaths10_nep,direct_deaths11_nep),
              direct_deaths_ost=rbind(direct_deaths10_ost,direct_deaths11_ost),
              direct_deaths_nep_ost=rbind(direct_deaths10_nep_ost,direct_deaths11_nep_ost)))
}




param_setup_v2=function(rin,fitType){
  if (fitType==2){
    r6=rin[1];
    r5=rin[2];
    r4=rin[3];
    
  }else{
    r6=rin[1];
    r5=rin[2];
    r4=rin[3];
    # r5=0.027 # default values
    # r4=17
  }
  
  # This creates two matrices for the static parameters
  # M is for former i=0
  # Mdash is for current i=1, i.e. PWID
  parm_names = c('delta','r_AF0','w','r_SVR4DC','r_SVR4HCC','r_F0F1','r_F1F2','r_F2F3','r_F3F4','r_F4DC','r_F4HCC',
                 'r_DCHCC','r_DCLT','r_DCdeath','r_HCCLT','r_HCCdeath',
                 'r_LT1death','r_LT2death')
  
  parm_current=rep(0,18)
  parm_former=rep(0,18)
  parm_current[1]=0.26; parm_former[1] = parm_current[1]; # spontaneous clearance
  parm_current[2]=52/12; parm_former[2] = parm_current[2];# acute duration is 12 weeks
  parm_current[3]=0; parm_former[3] = parm_current[3];# duration to return to susceptible after treated - no treats for burden model
  parm_current[4]=0; parm_former[4] = parm_current[4];# SVR4 to DC
  parm_current[5]=0; parm_former[5] = parm_current[5];# SVR4 to HCC
  parm_current[6] = -log(1- 0.116 ); parm_former[6] = -log(1-0.106); # F0 to F1 current then former
  parm_current[7] = -log(1- 0.085 ); parm_former[7] = -log(1- 0.074); # F1 to F2 current then former
  parm_current[8] = -log(1-0.085); parm_former[8] = -log(1-0.106); # F2 to F3 current then former
  parm_current[9] = -log(1-0.130); parm_former[9] = -log(1-0.105); # F3 to F4 current then former
  parm_current[10] = -log(1-0.037); parm_former[10] = parm_current[10]; # F4 to DC current then former
  parm_current[11] = -log(1-0.01); parm_former[11] = parm_current[11]; # F4 to HCC current then former
  parm_current[12] = -log(1-0.068); parm_former[12] = parm_current[12]; # DC to HCC
  parm_current[13] = -log(1-0.033); parm_former[13] = parm_current[13]; # DC to LT
  parm_current[15] = -log(1-0.1); parm_former[15] = parm_current[15]; # HCC to LT
  parm_current[14] = -log(1-0.138) ; parm_former[14] = parm_current[14]; # DC to death 
  parm_current[16] = -log(1-0.605); parm_former[16] = parm_current[16]; # HCC death
  parm_current[17] = -log(1-0.169); parm_former[17] = parm_current[17]; # LT to death year 1
  parm_current[18] = -log(1-0.034); parm_former[18] = parm_current[18]; # LT to death year 2
  
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
  Mdash[12,11]=parm_current[10]+parm_current[4];Mdash[12,12] = -parm_current[12]-parm_current[13]-parm_current[14];
  Mdash[13,11]=parm_current[11]+parm_current[5];Mdash[13,12]=parm_current[12];Mdash[13,13]=-parm_current[15]-parm_current[16];
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
  M[12,11]=parm_former[10]+parm_former[4];M[12,12] = -parm_former[12]-parm_former[13]-parm_former[14];
  M[13,11]=parm_former[11]+parm_former[5];M[13,12]=parm_former[12];M[13,13]=-parm_former[15]-parm_former[16];
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
  
  curr_mort_pwid=c(0.96 ,0.96 ,1.12 ,0.18 ,0.22 ,0.53 ,1.38 ,4.28 ,14.96)/1000; # mortality per year for PWID
  curr_mort_former=c(0.044 ,0.051 ,0.062 ,0.1 ,0.222 ,0.534 ,1.376 ,4.282 ,14.956 )/1000; # mortality per year for PWID
  
  
  mort_current = t(matrix(rep(curr_mort_pwid,20),ncol=20))
  mort_former = t(matrix(rep(curr_mort_former,20),ncol=20))
  extra_parms_nams=c('piv','relapse','nu'); # infection rate (piv instead of pi), relapse to IDU, 1/duration of injecting span
  
  extra_parms_vals=c(r6,-log(1-r5),1/r4) # was 5.6%, nu was 1/17
  rout=c(r6,r5,r4)
  
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
  
  death_rate_dc=  -log(1-0.138) 
  death_rate_hcc= -log(1-0.605) 
  death_rate_lt1= -log(1-0.169) 
  death_rate_lt2= -log(1-0.034) 
  
  param_vals=c(Mvec,Mdashvec,extra_parms_vals,
               phiminusvals1,phiminusvals2,phiminusvals3,phiminusvals4,phiminusvals5,
               phiplussvals1,phiplussvals2,phiplussvals3,phiplussvals4,phiplussvals5,
               Magevec,
               curr_mort_pwid,curr_mort_former,
               death_rate_dc,death_rate_hcc,death_rate_lt1,death_rate_lt2)
  
  return(param_vals)
  
  
}


read_and_plot=function(runy,ploty,j=0){
  if (runy==1){
    Z=readRDS("HPCruns/run1/allmat.rds")
    labx="All former" # These are columns
    laby="All current" # These are rows
    if (ploty=="M"){
      X=Z$M - 2000
      colbarlaby = "Mortality"
      titly = "Year to reach WHO mortality target"
    } else if (ploty=="I"){
      X=Z$I - 2000
      colbarlaby = "Incidence"
      titly = "Year to reach WHO incidence target"
    } else if (ploty=="B"){
      X=Z$B - 2000
      colbarlaby = "Both"
      titly = "Year to have reached both WHO target"
    } else if (ploty=="C"){
      X=Z$C
      colbarlaby = "ICER"
      titly = "Cost effective ratio as $ per QALY"
    } else if (ploty=="A"){
      X=Z$A
      colbarlaby = "Average"
      titly = "Average DAA per 1,000 PWID"
    }  else if (ploty=="TT"){
      X=Z$TT
      colbarlaby = "Total"
      titly = "Total PWID form 2022-2040"  
    }
  }
  
  if (runy==2){
    Z=readRDS(paste0("HPCruns/run2/outmat_",j,".rds"))
    labx="PWID F0-F3" # These are columns
    laby="PWID F4-LT" # These are rows
    if (ploty=="M"){
      X=Z$M - 2000
      colbarlaby = "Mortality"
      titly = paste0("Year to reach WHO mortality target,NSP coverage = ",j,"%")
    } else if (ploty=="I"){
      X=Z$I - 2000
      colbarlaby = "Incidence"
      titly = paste0("Year to reach WHO incidence target,NSP coverage = ",j,"%")
    } else if (ploty=="B"){
      X=Z$B - 2000
      colbarlaby = "Both"
      titly = paste0("Year to have reached both WHO target,NSP coverage = ",j,"%")
    } else if (ploty=="C"){
      X=Z$C
      colbarlaby = "ICER"
      titly = paste0("Cost effective ratio as $ per QALY,NSP coverage = ",j,"%")
    }
  }
    
    if (runy==3){
      Z=readRDS("HPCruns/run3/allmat.rds")
      labx="PWID F0-F3" # These are columns
      laby="PWID F4-LT" # These are rows
      if (ploty=="M"){
        X=Z$M - 2000
        colbarlaby = "Mortality"
        titly = "Year to reach WHO mortality target"
      } else if (ploty=="I"){
        X=Z$I - 2000
        colbarlaby = "Incidence"
        titly = "Year to reach WHO incidence target"
      } else if (ploty=="B"){
        X=Z$B - 2000
        colbarlaby = "Both"
        titly = "Year to have reached both WHO target"
      } else if (ploty=="C"){
        X=Z$C
        colbarlaby = "ICER"
        titly = "Cost effective ratio as $ per QALY"
      } else if (ploty=="A"){
        X=Z$A
        colbarlaby = "Average"
        titly = "Average DAA per 1,000 PWID"
      }  else if (ploty=="TT"){
          X=Z$TT
          colbarlaby = "Total"
          titly = "Total PWID form 2022-2040"  
      }
    }
  if (runy==5){
    X=readRDS("HPCruns/run5/out_prop_dist.rds")
    x=1:100
    y=X[[ploty]]
    if (ploty=="M"){
      labx="reach per 1,000 PWID"
      laby = "Year (2000 &)"
      y=y-2000
      titly = "Year WHO Mortality target reached"
    }else if (ploty=="I"){
      labx="reach per 1,000 PWID"
      laby = "Year (2000 &)"
      y=y-2000
      titly = "Year WHO Incidence target reached"
    }else if (ploty=="B"){
      labx="reach per 1,000 PWID"
      laby = "Year (2000 &)"
      y=y-2000
      titly = "Year WHO Mortality and Incidence target reached"
    }else if (ploty=="C"){
      labx="reach per 1,000 PWID"
      laby = "ICER($)"
      titly = "ICER ($ per QALY)"
    }else if (ploty=="A"){
      labx="reach per 1,000 PWID"
      laby = "average treatments per 1,000 PWID"
      titly = "Average # treatments per 1,000 PWID from 2022 to 2040"
    }else if (ploty=="TT"){
      labx="reach per 1,000 PWID"
      laby = "total # of treatments from 2022 to 2040"
      titly = "Total # treatments from 2022 to 2040"
    }
  }
      if (runy==4){
        Z=readRDS("HPCruns/run4/allmat.rds")
        labx="PWID F0-F3" # These are columns
        laby="PWID F4-LT" # These are rows
        if (ploty=="M"){
          X=Z$M - 2000
          colbarlaby = "Mortality"
          titly = "Year to reach WHO mortality target"
        } else if (ploty=="I"){
          X=Z$I - 2000
          colbarlaby = "Incidence"
          titly = "Year to reach WHO incidence target"
        } else if (ploty=="B"){
          X=Z$B - 2000
          colbarlaby = "Both"
          titly = "Year to have reached both WHO target"
        } else if (ploty=="C"){
          X=Z$C
          colbarlaby = "ICER"
          titly = "Cost effective ratio as $ per QALY"
        } else if (ploty=="A"){
          X=Z$A
          colbarlaby = "Average"
          titly = "Average DAA per 1,000 PWID"
        }  else if (ploty=="TT"){
          X=Z$TT
          colbarlaby = "Total"
          titly = "Total PWID form 2022-2040"  
        }
    }
  
  if (runy==5){
    df = data.frame(x=x,y=y)
    # miny=min(pretty_breaks()(c(0, df$y)))
    # maxy=max(pretty_breaks()(c(0, df$y)))
    miny=min(pretty_breaks()(df$y))
    maxy=max(pretty_breaks()(df$y))
    vecy=pretty_breaks()(df$y)
    p=ggplot(data=df, aes(x=x, y=y)) + geom_line(color="red",size=2) +
      xlab(labx) + ylab(laby) + ggtitle(titly)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand=c(0,0),limits=c(miny,maxy),breaks=vecy)+
      scale_x_continuous(expand=c(0,0),limits=c(1,100),breaks=c(1,seq(5,100,5)))+
      theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    return(p)
  }else{
    p=plotoutmat(X,labx,laby,titly,colbarlaby) # note X is transposed in this function
  }
  return(p)
}

plotoutmat = function(Z,labx,laby,titly,legt,x=1:100,y=1:100){
  library(ggplot2)
  library(hrbrthemes)
  library(plotly)
  library(reshape2)
  library(scales)
  
  Z=t(Z)
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
    ggtitle(titly)+
    labs(fill = legt) +
    #scale_fill_continuous(labels=comma)+
    theme(legend.key.height = unit(3, "cm"))+ # legend.position = "bottom", 
    scale_fill_viridis_c(option = "magma",labels=comma,na.value = "transparent")
  return(p)
}

registerInputHandler("x.child", function(x, ...) {
  fromJSON(toJSON(x, auto_unbox = TRUE, null = "null"), 
           simplifyDataFrame = FALSE)
}, force = TRUE)


callback <<- JS(
  "var expandColumn = table.column(0).data()[0] === 'plus-sign' ? 0 : 1;",
  "table.column(expandColumn).nodes().to$().css({cursor: 'pointer'});",
  "",
  "// send selected columns of the main table to Shiny",
  "var tbl = table.table().node();",
  "var tblId = $(tbl).closest('.datatables').attr('id');",
  "var selector = 'td:not(:nth-child(' + (expandColumn+1) + '))';",
  "table.on('click', selector, function(){",
  "  setTimeout(function(){",
  "    var indexes = table.rows({selected:true}).indexes();",
  "    var indices = Array(indexes.length);",
  "    for(var i = 0; i < indices.length; ++i){",
  "      indices[i] = indexes[i];",
  "    }",
  "    Shiny.setInputValue(tblId + '_rows_selected', indices);",
  "  },0);",
  "});",
  "",
  "// make the table header of the nested table",
  "var format = function(d, childId){",
  "  if(d != null){",
  "    var html = '<table class=\"compact hover\" id=\"' + ", 
  "                childId + '\"><thead><tr>';",
  "    for(var key in d[d.length-1][0]){",
  "      html += '<th>' + key + '</th>';",
  "    }",
  "    html += '</tr></thead></table>'",
  "    return html;",
  "  } else {",
  "    return '';",
  "  }",
  "};",
  "",
  "// row callback to style the rows background colors of the child tables",
  "var rowCallback = function(row, dat, displayNum, index){",
  "  if($(row).hasClass('odd')){",
  "    $(row).css('background-color', 'papayawhip');",
  "    $(row).hover(function(){",
  "      $(this).css('background-color', '#E6FF99');",
  "    }, function() {",
  "      $(this).css('background-color', 'papayawhip');",
  "    });",
  "  } else {",
  "    $(row).css('background-color', 'lemonchiffon');",
  "    $(row).hover(function(){",
  "      $(this).css('background-color', '#DDFF75');",
  "    }, function() {",
  "      $(this).css('background-color', 'lemonchiffon');",
  "    });",
  "  }",
  "};",
  "",
  "// header callback to style the header of the child tables",
  "var headerCallback = function(thead, data, start, end, display){",
  "  $('th', thead).css({",
  "    'border-top': '3px solid indigo',", 
  "    'color': 'indigo',",
  "    'background-color': '#fadadd'",
  "  });",
  "};",
  "",
  "// make the child table",
  "var format_datatable = function(d, childId){",
  "  var dataset = [];",
  "  var n = d.length - 1;",
  "  for(var i = 0; i < d[n].length; i++){",
  "    var datarow = $.map(d[n][i], function(value, index){",
  "      return [value];",
  "    });",
  "    dataset.push(datarow);",
  "  }",
  "  var id = 'table#' + childId;",
  "  var subtable = $(id).DataTable({",
  "             'data': dataset,",
  "             'autoWidth': true,",
  "             'deferRender': true,",
  "             'info': false,",
  "             'lengthChange': false,",
  "             'ordering': d[n].length > 1,",
  "             'order': [],",
  "             'paging': false,",
  "             'scrollX': false,",
  "             'scrollY': false,",
  "             'searching': false,",
  "             'sortClasses': false,",
  "             'rowCallback': rowCallback,",
  "             'headerCallback': headerCallback,",
  "             'select': {style: 'multi'},",
  "             'columnDefs': [{targets: '_all', className: 'dt-right'}]", # was center
  "           });",
  "};",
  "",
  "// send selected rows of the children tables to shiny server",
  "var nrows = table.rows().count();",
  "var nullinfo = Array(nrows);",
  "for(var i = 0; i < nrows; ++i){",
  "  nullinfo[i] = {row: i, selected: null};",
  "}",
  "Shiny.setInputValue(tblId + '_children:x.child', nullinfo);",
  "var sendToR = function(){",
  "  var info = [];",
  "  setTimeout(function(){",
  "    for(var i = 0; i < nrows; ++i){",
  "      var childId = 'child-' + i;",
  "      var childtbl = $('#'+childId).DataTable();",
  "      var indexes = childtbl.rows({selected:true}).indexes();",
  "      var indices;",
  "      if(indexes.length > 0){",
  "        indices = Array(indexes.length);",
  "        for(var j = 0; j < indices.length; ++j){",
  "          indices[j] = indexes[j];",
  "        }",
  "      } else {",
  "        indices = null;",
  "      }",
  "      info.push({row: i, selected: indices});",
  "    }",
  "    Shiny.setInputValue(tblId + '_children:x.child', info);",
  "  }, 0);",
  "}",
  "$('body').on('click', '[id^=child-] td', sendToR);",
  "",
  "// click event to show/hide the child tables",
  "table.on('click', 'td.details-control', function () {",
  "  var cell = table.cell(this);",
  "      row = table.row($(this).closest('tr'));",
  "  if(row.child.isShown()){",
  "    row.child.hide();",
  "    cell.data('expand');",
  "    sendToR();",
  "  } else {",
  "    var childId = 'child-' + row.index();",
  "    row.child(format(row.data(), childId)).show();",
  "    row.child.show();",
  "    cell.data('collapse-down');",
  "    format_datatable(row.data(), childId);",
  "  }",
  "});")
## render function, to display the glyphicons ####
render <<- c(
  "function(data, type, row, meta){",
  "  if(type === 'display'){",
  "    return '<span style=\\\"color:black; font-size:18px\\\">' + ",
  "       '<i class=\\\"glyphicon glyphicon-' + data + '\\\"></i></span>';",
  "  } else {",
  "    return data;",
  "  }",
  "}"
)

# new type of plot
plot_cases_deaths=function(x_data,inci_data,mort_data){
  # Note these country names come from vaccine data
  cases=inci_data
  deaths=mort_data
  typeof = c(rep("HCV Incidence",length(cases)),rep("HCV Deaths",length(deaths)))
  Y=c(cases,-deaths)
  D=c(x_data,x_data)
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
    scale_fill_manual(paste0("Incident cases and deaths","        "), values = c("HCV Incidence" = "blue", "HCV Deaths" = "red"))+
    scale_y_continuous(breaks=prettysmall,label=prettybig)+
    scale_x_continuous(breaks = unique(D),expand=c(0,0))+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "top",legend.justification='left')+
    theme(plot.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(p1)
}


# this is turbotrend

################################################################################
##Fast P-spline smoothing ... 
##
##
##
################################################################################
#interface to Paul's C implementation for fast summation
# binsum <- function(x, y = 0 * x + 1, n = max(as.integer(x)))
#   {
#     ## Sum values of y in bins given by x
#     
#     ## Create working arrays
#     a <- as.integer(x)
#     b <- rep(0.0, n + 1)
#     m <- as.integer(length(x))
#     ## Call C function
#     som <- .C("binsum",
#               a = as.vector(a, mode="integer"),
#               y = as.vector(y, mode="double"),
#               b = as.vector(b, mode="double"),
#               m = as.integer(m),
#               PACKAGE="TurboNorm")[[3]]
#     return(som[-1])
#   }

#the work horse based on P.H.C. Eilers' implementation
turbotrend <- function(x, y, w = rep(1, length(y)), n = 100, lambda=10^seq(-10, 10, length=1000), iter = 0, method=c("original", "demmler"))
{
  my.call <- match.call()	
  method <- match.arg(method)	
  
  maxLambda <- 10^12
  
  if(length(x) != length(y))
    stop("x and y are of different length!")
  
  if(length(x) != length(w))
    stop("weights must be of same length as data!")	
  
  ##order x and y by x
  ##x <- x[order(x)]
  ##y <- y[order(x)]
  ##xmin <- x[1]
  ##xmax <- x[length(x)]
  
  xmin <- min(x)
  xmax <- max(x)
  dx <- 1.0001 * (xmax - xmin) / n
  xt <- xmin + ((1:n) - 0.5) * dx
  xi <- floor((x - xmin) / dx + 1)
  
  ##Collect counts and sums (handle empty bins correctly)
  ##tel <- tapply(0 * xi + 1, xi, sum) #BTB
  tel <- tapply(w, xi, sum)  #BTWB
  p <- as.integer(names(tel))
  u <- rep(0, n)
  u[p] <- tel
  ##using Paul's C implementation
  #u <- binsum(xi, as.numeric(w))       #as binners.c expects "numeric"
  
  ##som <- tapply(y, xi, sum) #BTy
  som <- tapply(w*y, xi, sum) #BTWy
  v <- rep(0, n)
  ##
  v[p] <- som
  ##using Paul's C implementation
  #v <- binsum(xi, y*w)
  
  ## Build penalty and solve system
  D <- diff(diag(n), diff = 2)
  
  ## Perform generalized cross-validation in order to find the optimal lambda if not given
  if(length(lambda) > 1){				
    
    maxLambda <- lambda[length(lambda)]
    
    ##old slower search for optimal lambda
    ##gcv <- switch(method, 
    ##                 original = sapply(lambda, function(x) originalBasis(x, xi, y, u, v, D)$gcv),    
    ##                 demmler = sapply(lambda, function(x) DemmlerReinschBasis(x, xi, y, u, v, D)$gcv))			
    
    ##lambda <- lambda[which.min(gcv)]
    
    ##is this faster?
    GCV.org <- function(x, xi, y, u, v, D) originalBasis(x, xi, y, u, v, D)$gcv
    GCV.DR <- function(x, xi, y, u, v, D) DemmlerReinschBasis(x, xi, y, u, v, D)$gcv
    lambda <- switch(method, 
                     original = optimize(GCV.org, interval=range(lambda), xi, y, u, v, D)$minimum,
                     demmler = optimize(GCV.DR, interval=range(lambda), xi, y, u, v, D)$minimum)
  }
  
  if(lambda <= 0 & lambda > maxLambda)
    warning("lambda <= 0 or > maxLambda!")
  
  solved <- switch(method, 
                   original = originalBasis(lambda, xi, y, u, v, D),
                   demmler = DemmlerReinschBasis(lambda, xi, y, u, v, D))              
  
  ##catch error
  if(inherits(solved, "try-error"))
    return(solved)
  
  ## Robustifying iterations 
  if(iter > 0) {
    P <- lambda * t(D) %*% D
    yhat <- solved$coeff[xi]
    for (it in 1:iter) {
      r <- y - yhat
      s <- median(abs(r))
      t <- r / (5 * s + 1e-4)
      wr <- (1 - t ^ 2) ^ 2
      wr[abs(t) > 1] <- 0
      u <- binsum(xi, wr)
      v <- binsum(xi, wr * y)
      W <- diag(u)
      z <- solve(W + P, v)
      yhat <- z[xi]
    }
  } 
  else {
    yhat <- solved$coeff[xi]
  }
  
  ## Return list with results
  object <- list(x = x, y = yhat, w=w, n=n, xtrend = xt, ytrend = solved$coeff, lambda=solved$lambda, iter=iter, gcv=solved$gcv, 
                 edf=solved$edf, method=method, call=my.call) 
  class(object) <- "turbotrend"
  object
}

################################################################################
##print method in a similar way as print.smoothPspline from pspline-package
##
##
##
################################################################################

print.turbotrend <- function(x, ...) {
  if(!is.null(cl <- x$call)) {
    cat("Call:\n")
    dput(cl)
  }
  cat("\nEffective degrees of freedom:", format(x$edf), "\n")
  cat("Number of bins:", format(x$n), "\n")
  cat("Penalty value:", format(x$lambda),  "\n") 
  cat("Number of robustifying iterations:", format(x$iter), "\n")              
  cat("GCV :", format(x$gcv), "\n")
  invisible(x)
}


################################################################################
##solve 
##
##
################################################################################
originalBasis <- function(lambda, xi, y, u, v, D)
{
  ##implementation without calculating the 'smoother' matrix explicitly because that involves nxn matrices i.s.o mxm
  tmp <- diag(u) + lambda * t(D) %*% D
  z <- try(solve(tmp, v))
  
  ##catch error
  if(inherits(z, "try-error"))
    return(z)
  
  ##cyclic permutation of matrices as it doesn't change the trace but makes the computation more efficient
  edf <- sum(diag(solve(tmp) %*% diag(u))) 
  ##gcv <- crossprod(y - z[xi])/(length(u) - edf)^2
  
  gcv <- sum((y - z[xi])^2)/(length(u) - edf)^2
  
  ##z0 <- solve(diag(u), v) #penalty = 0
  ##var0 <- sum((y - z0[xi])^2)/(length(u))^2 #residuals variances	
  ##aic <- sum((y - z[xi])^2)/var0 + 2*length(u)*log(sqrt(var0)) - 2*length(y)*log(2*pi)
  
  list(lambda=lambda, edf=edf, gcv=gcv, coeff=z)
}

################################################################################
##solve 
##
##
################################################################################
DemmlerReinschBasis <- function(lambda, xi, y, u, v, D)
{
  X <- chol(diag(u)) ## X^(-t)X^(-1) == diag(u)
  X <- diag(1/diag(X)) #inverse
  
  E <- eigen(X%*% t(D)%*%D%*%t(X), symmetric=TRUE)	
  
  T <- E$vectors ##T should be orthogonal and all.equal(T%*%diag(c)%*%t(T), X%*% t(D)%*%D%*%t(X))
  c <- E$values	
  
  z <- t(X)%*%T%*% solve(diag(1 + lambda*c), t(T)%*%X%*%v)
  edf <- sum(1/(1 + lambda*c))
  #gcv <- crossprod(y - z[xi])/(length(u) - edf)^2
  gcv <- sum((y - z[xi])^2)/(length(u) - edf)^2
  
  list(lambda=lambda, edf=edf, gcv=gcv, coeff=z)
}




