output_ICER_text = function(base_cost,base_qaly,trt_cost,trt_qaly,daa_only,scen_number){
  library(flextable)
  ICER = (trt_cost - base_cost)/(trt_qaly-base_qaly)
  captiony=paste0("ICER(Scenario #",scen_number,") $ per QALY gained\n")
  if (daa_only=="Yes"){
    trtmessage = "DAA treatment"
    footnote=""
  }else{
    trtmessage = "DAA treatment with prevention"
    footnote="Prevention costs not included"
  }
  costs = c(base_cost,trt_cost)
  qalys = c(base_qaly,trt_qaly)
  icers = c(NA,round(ICER))
  df = data.frame(costs,qalys,icers)
  names(df)=c("Total Cost($)", "Total Qalys", "ICER($)" )
  
  # set_flextable_defaults(theme_fun = theme_vanilla,
  #                        padding.top = 12,
  #                        background.color = "#EFEFEF")
  df <- format(df,big.mark = ",")
  df$`Total Cost($)`=as.character(df$`Total Cost($)`)
  df$`Total Qalys`=as.character(df$`Total Qalys`)
  df$`ICER($)`=as.character(df$`ICER($)`)
  df[1,3]=""
  if (df[2,3]=="NaN"){
    df[2,3]="-"
  }
  outab = flextable(df)
  #set_flextable_defaults(big.mark = ",",padding.top = 12)
  outab=flextable::colformat_num(x=outab, na_str="",big.mark = ",")
  outab = set_caption(outab, caption = captiony)
  outab = add_footer_lines(outab, footnote)
  outab = color(outab,i=2,j=3, color = "red")
  outab = bold(outab , i=2,j=3, bold = TRUE)
  outab = align_text_col(outab , align = "right")
  outab = width(outab, width = 3)
  #set_table_properties(outab, width = 1, layout = "autofit")
  return(outab)
}
