read_and_write=function(){
  # read the outputs and plots
  icer_mat=matrix(data=0,nrow = 100,ncol = 100)
  mort_mat=icer_mat
  inci_mat=icer_mat
  both_mat=icer_mat
  avy_mat=icer_mat
  tot_mat=icer_mat
  for ( i in 1 : 100){
    #X=readRDS(paste0("HPCruns/run1/out_",i,".rds"))  #rows are current, cols are former
    #X=readRDS(paste0("HPCruns/run3/out_",i,".rds"))  #rows are ad PWID, cols are mild PWID
    X=readRDS(paste0("HPCruns/run4/out_",i,".rds"))  #rows are current, cols are former
   
    mort_mat[i,]=X$M
    inci_mat[i,]=X$I
    icer_mat[i,]=X$C
    both_mat[i,]=X$B
    avy_mat[i,]=X$A
    tot_mat[i,]=X$TT
  }
  #saveRDS(list(M=mort_mat,I=inci_mat,C=icer_mat,B=both_mat,A=avy_mat,TT=tot_mat),"HPCruns/run1/allmat.rds")
  #saveRDS(list(M=mort_mat,I=inci_mat,C=icer_mat,B=both_mat,A=avy_mat,TT=tot_mat),"HPCruns/run3/allmat.rds")
  saveRDS(list(M=mort_mat,I=inci_mat,C=icer_mat,B=both_mat,A=avy_mat,TT=tot_mat),"HPCruns/run4/allmat.rds")
}
