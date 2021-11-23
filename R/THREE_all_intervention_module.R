THREE_all_intervention_module_ui <- function(id) {
  ns <- NS(id)
    dashboardPage(
    dashboardHeader(title = "All pop interventions",titleWidth = 320),
    dashboardSidebar(width = 320, shinyjs::useShinyjs(),    chooseSliderSkin("Flat", color = "blue"),
                     tags$head(
      tags$style(type="text/css", "#inliney label{ float:left; }" )
    ),     shinyjs::useShinyjs(),    chooseSliderSkin("Flat", color = "blue"),
                     div(style="display: inline-block;vertical-align:top;",tags$h5("Compartment model description")),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("THREE_int_detail2"), label="Show Model",size="xs")),
                     textOutput(ns('THREE_scenario_text')),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("THREE_intervention"), label="Intervention",size="xs")),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("THREE_run_sim_all"), label="Run simulation",size="xs")),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("THREE_reset_input2"), label="Reset",size="xs")),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("THREE_faqs"), label="FAQs",size="xs")),
    hidden(div(style="display: inline-block;vertical-align:top;",actionBttn(ns("THREE_play1"), label="Example 1",size="xs"))),
    hidden(div(style="display: inline-block;vertical-align:top;",actionBttn(ns("THREE_play2"), label="Example 2",size="xs"))),
    hidden(div(id="radioBtn",
               awesomeRadio(ns("rb_1"),status="danger",
                            label="Select population:",width='100%',
                            choices=list("Gen pop","PWID","Prison","Gen pop + PWID","Prison + PWID","Gen pop + Prison","Gen pop + PWID + Prison","Triptych"),
                            selected="Gen pop + PWID + Prison",inline=FALSE))), 
                            #tags$script("$('.radio-inline').addClass('col-md-3');$('.shiny-options-group').addClass('row');"))),
    #tags$head(tags$style(".radio-inline{margin-left:10px;padding-left: 20;}")),
    #tags$head(tags$style("#radioBtn{margin-left:10px;padding-left: 20;}")),
    
    hidden(div(id = "THREE_div_plotoutA",style="display: inline-block;vertical-align:top;width: 155px;",(selectInput(ns("THREE_plotoutA"), "plot WHO targets:",
                                                                                    c("Mortality" = "mort",
                                                                                      "Incidence" = "inci",
                                                                                      "Mortality#" = "mort_num",
                                                                                      "Incidence#" = "inci_num",
                                                                                      "#Inci & #Mort"="inci_mort_num"
                                                                                    ),
                                                                                    multiple = FALSE,size=5,selectize=FALSE,selected=character(0)
    )))),
    hidden(div(id="THREE_div_plotoutB",style="display: inline-block;vertical-align:top;width: 155px;",(selectInput(ns("THREE_plotoutB"), "plot epi outputs:",
                                                                                    c("Treatments" = "treat",
                                                                                      "Cases per treat" = "caseav",
                                                                                      "Point prevalence" = "point_prev",
                                                                                      "Chronic stages" = "stage",
                                                                                      "Cumulative" ="attack_pert",
                                                                                      "Prevention per year" ="prev_prev",
                                                                                      "Chronic #" ="chronic_num",
                                                                                      "R0(t)"="R0t",
                                                                                      "ICER"="ICER"
                                                                                      #"Chronic # long" ="chronic_num_long",
                                                                                      #"PWID #" ="pwid_num"
                                                                                      #"PWID # long" ="pwid_num_long"
                                                                                    ),
                                                                                    multiple = FALSE,size=9,selectize=FALSE,selected=character(0)
    ))))
    ),
 
    dashboardBody(shinyjs::useShinyjs(),tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '
    ))), 
    h5(id="THREE_wordy2",htmlOutput(ns("THREE_plot_simtext2"), container = span)),tags$style(HTML("#THREE_wordy2{color: red;}")),
    fluidRow(
      hidden(div(id = "THREE_div_plotsimmodel",column(width = 12,
             mainPanel(plotOutput(ns("THREE_plot_simmodel"),height="600px") %>% withSpinner() )))
      ),
             hidden(div(id = "THREE_div_plot_flex",column(width = 12,
             mainPanel(DTOutput(ns("THREE_plot_flex"),height="600px") )))
      )
             
    )
    )
  )
  
}

THREE_all_intervention_module <- function(input, output, session,scenario_number,parent_session) {
  
  output$THREE_plot_simmodel =  renderPlot(NULL) # so no spinner until button pushed
  output$THREE_scenario_text <- renderText({paste0("Scenario # ",scenario_number$scenario) })
  rv <- reactiveValues(pushed=0)
  new_rv <- reactiveValues(x=0)
  # xglob<<-default_vals()
  # print(xglob$fixed_coverage_three_GP)
  observeEvent(input$THREE_intervention,{
    rv$pushed <- 1
    new_rv$x <-1
    updateTabsetPanel(parent_session, "taby",selected = "2")
  })
  
  
  observeEvent(input$THREE_run_sim_all,{
    rtv_df=retvals(xglob)
    # rtv_df= read_yaml("/home/mrcuser/HCV_vX/HCV_prison/int_v3.YAML")
    #print(rtv_df)
    
    shinyjs::hide(id="THREE_div_plot_flex",asis=TRUE)
    updateSelectizeInput(session, "THREE_plotoutA", selected = character(0))
    updateSelectizeInput(session, "THREE_plotoutB", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "THREE_plotoutA")
    session$sendCustomMessage(type = "resetValue", message = "THREE_plotoutB")
    reset("rb_1")
    output$THREE_plot_simmodel =  renderPlot(NULL)
    output$THREE_plot_simtext2 = renderUI({HTML("Select required interventions and click run simulation")})
    shinyjs::hide(id="THREE_div_plotoutA",asis=TRUE)
    #shinyjs::hide(id="div_flexoutA",asis=TRUE)
    shinyjs::hide(id="THREE_div_plotoutB",asis=TRUE)
    #shinyjs::hide(id="div_plot_flex",asis=TRUE)
    shinyjs::hide(id="THREE_div_plotsimmodel",asis=TRUE)
    shinyjs::hide(id="radioBtn",asis=TRUE)
    output$THREEplot_flex=renderUI({NULL})

    # updateSelectizeInput(session, "THREE_plotoutA", selected = character(0))
    # updateSelectizeInput(session, "THREE_plotoutB", selected = character(0))
    # session$sendCustomMessage(type = "resetValue", message = "THREE_plotoutA")
    # session$sendCustomMessage(type = "resetValue", message = "THREE_plotoutB")
    # shinyjs::hide(id="THREE_div_plotoutA",asis=TRUE)
    # shinyjs::hide(id="THREE_div_plotoutB",asis=TRUE)
    # output$THREE_plot_simmodel =  renderPlot(NULL)
    # output$THREE_plot_flex=renderUI({NULL})
    # shinyjs::hide(id="THREE_div_plotsimmodel",asis=TRUE)
    # shinyjs::hide(id="THREE_div_plot_flex",asis=TRUE)
    # shinyjs::hide(id="radioBtn",asis=TRUE)
    # get the baseline with no figures
    q=testnewstartPWID_v4(rtv_df$discount_rate,0,1,0)

    # saveRDS(rtv_df,"~/Documents/DTOP/HCV_vX/acute_fit/acute_df.rds")
    XX<<-testnewstartPWID_v4_intervention(rtv_df,q$s,q$Xgenpop,q$Xpwid,q$Xprison,
                                          q$Lbasegenp,q$Lbasepwid,q$Lbasepris,
                                          0,1)
   
    shinyjs::show(id="radioBtn",asis=TRUE)
    shinyjs::show(id="THREE_div_plotoutA",asis=TRUE)
    shinyjs::show(id="THREE_div_plotoutB",asis=TRUE)
    output$THREE_plot_simtext2 = renderUI({HTML("Select required output")})

  })
  
  observeEvent(input$THREE_plotoutA,{
    shinyjs::hide(id="THREE_div_plot_flex",asis=TRUE)
    shinyjs::show(id="THREE_div_plotsimmodel",asis=TRUE)
    #print(input$THREE_plotoutA)
    #print(input$rb_1)
    output$THREE_plot_simmodel =  renderPlot(
      plot(XX[[control13plots(input$THREE_plotoutA,input$rb_1)]])
    )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("THREE_plot_simmodel"))  )
    updateSelectizeInput(session, "THREE_plotoutB", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "THREE_plotoutB") 
    output$THREE_plot_simtext2 = renderUI({HTML("Select required output")})
  })
  
  observeEvent(input$THREE_plotoutB,{
    if (input$THREE_plotoutB=="ICER"){
      shinyjs::show(id="THREE_div_plot_flex",asis=TRUE)
      shinyjs::hide(id="THREE_div_plotsimmodel",asis=TRUE)
# footnote and trtmessage defined in plot_output_icer_PWID_prison_gp as globals
 
      output[["THREE_plot_flex"]] <- renderDT({
        if (input$rb_1=="Triptych"){
          Ftab = rbind(XX[[105]],XX[[106]],XX[[107]])
          rownames(Ftab)=c("gen pop baseline","gen pop intervention","PWID baseline","PWID intervention",
                           "prison baseline","prison intervention")
          dum=get_title_footnote("Gen pop + PWID + Prison")
          footnote_local=dum$foot
          trtmessage_local=dum$trt
        } else{
          Ftab=XX[[controlicerplots(input$rb_1)]]
          dum=get_title_footnote(input$rb_1)
          footnote_local=dum$foot
          trtmessage_local=dum$trt          
        }
        output$THREE_plot_simtext2 = renderUI({HTML(trtmessage_local)})
        datatable(Ftab, callback = callback, escape = -2, 
                  extensions = "Select", selection = "none",
                  options = list(dom="t",
                                 select = list(style = "multi", selector = ".selectable"),
                                 autoWidth = FALSE,
                                 columnDefs = list(
                                   list(className = "selectable dt-center", 
                                        targets = c(0, 2:ncol(Ftab))),
                                   list(visible = FALSE, targets = ncol(Ftab)),
                                   list(orderable = FALSE, className = 'details-control', 
                                        width = "10px", render = JS(render), targets = 1),
                                   list(className = "dt-center", targets = "_all")
                                 )
                  ),caption = tags$caption(
                    style="caption-side: bottom; text-align: right; margin: 8px 0;",
                    footnote_local
                  )
        ) %>% formatStyle('ICER($)',color="red") %>% formatStyle(1:3,`text-align` = 'right')
      }, server = FALSE)
    } else
      {
        shinyjs::hide(id="THREE_div_plot_flex",asis=TRUE)
        shinyjs::show(id="THREE_div_plotsimmodel",asis=TRUE)
        # if (length(intersect(input$THREE_plotoutB,c("treat","caseav","attack_pert","prev_prev")))>0){
        #   output$THREE_plot_simmodel =  renderPlot(NULL) # null figure
        #   output$THREE_plot_simtext2 = renderUI({HTML("TREATMENT output - not yet implemented - baseline only ")})
        # }else{
      
        output$THREE_plot_simmodel =  renderPlot(
          plot(XX[[control13plots(input$THREE_plotoutB,input$rb_1)]])
        )
        ns =session$ns
        output$ui =renderUI(plotOutput(ns("THREE_plot_simmodel"))  ) 
        output$THREE_plot_simtext2 = renderUI({HTML("Select required output")})
    }

    updateSelectizeInput(session, "THREE_plotoutA", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "THREE_plotoutA")
    # output$THREE_plot_simtext2 = renderUI({HTML("Select required output")})
    # }

  })

  observeEvent(input$THREE_reset_input2,{
    shinyjs::hide(id="THREE_div_plot_flex",asis=TRUE)
    updateSelectizeInput(session, "THREE_plotoutA", selected = character(0))
    updateSelectizeInput(session, "THREE_plotoutB", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "THREE_plotoutA")
    session$sendCustomMessage(type = "resetValue", message = "THREE_plotoutB")
    reset("rb_1")
    output$THREE_plot_simmodel =  renderPlot(NULL)
    output$THREE_plot_simtext2 = renderUI({HTML("Select required interventions and click run simulation")})
    shinyjs::hide(id="THREE_div_plotoutA",asis=TRUE)
    #shinyjs::hide(id="div_flexoutA",asis=TRUE)
    shinyjs::hide(id="THREE_div_plotoutB",asis=TRUE)
    #shinyjs::hide(id="div_plot_flex",asis=TRUE)
    shinyjs::hide(id="THREE_div_plotsimmodel",asis=TRUE)
    shinyjs::hide(id="radioBtn",asis=TRUE)
    output$THREEplot_flex=renderUI({NULL})
    #callModule(intervention_panel_module,id="int3_model",0)
    rv$pushed <- 0
    new_rv$x <-0
    #shinyjs::reset("int3_model-early_gen_three")
    #updateSliderInput(parent_session,'early_gen_three',value = 0)
    #shinyjs::hide(id="plotof")
    #output$plot_simmodel =  renderPlot(NULL)
  })  
  
  observeEvent(input$THREE_int_detail2, {
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","model_structure.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  observeEvent(input$THREE_play1, {
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="m",
                  # fluidRow( useShinyjs(),
                            # column(12,
                                   tags$iframe(src=paste0("picy/","video1.webm"),style=" width:1100px; height: 150vh;border:none; margin:0; padding:0; overflow:hidden; z-index:999999;",scrolling=TRUE,id = 'myIframe'),
                                   tags$script(HTML("iFrameResize({ log: true }, '#myIframe')"))))
  })
  observeEvent(input$THREE_play2, {
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="m",
                  # fluidRow( useShinyjs(),
                            # column(12,
                                   tags$iframe(src=paste0("picy/","video1.webm"),style=" width:1100px; height: 100vh;border:none; margin:0; padding:0; overflow:hidden; z-index:999999;",scrolling=TRUE,id = 'myIframe'),
                                   # tags$iframe(src=paste0("picy/","video2.webm"),frameborder="0" ,style="overflow:hidden;height:100%;width:100%" ,height="100%" ,width="100%",scrolling=TRUE,id = 'myIframe'),
                                   tags$script(HTML("iFrameResize({ log: true }, '#myIframe')"))))
  })
  
  observeEvent(input$reach_butt, {
    ns =session$ns
    
    showModal(modalDialog(
      size="l",
      HTML('<img src="reach.PNG">')))
  })
  
  observeEvent(input$prev_butt, {
    ns =session$ns
    
    showModal(modalDialog(
      size="l",
      HTML('<img src="nsp_coverage.PNG">')))
  })
  
  observeEvent(input$THREE_faqs, {
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","PWID_HCV_faqs.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  
  # observeEvent(input$faqs, {
  #   ns =session$ns
  #   
  #   showModal(modalDialog(
  #     size="l",
  #     HTML('<img src="faqs.PNG">')))
  # })
  
  output$THREE_plot_simtext2 = renderUI({HTML("Select required interventions and click run simulation")})
  # dum =reactive({new_rv$x})
  # print(dum)
  New_rv<<-new_rv
  return(rv)
}

control8plots=function(plottype,plotid){
  # This returns the correct number plot form teh selection of 8
  # plotype is the inic ,mort etc
  # plot id is G,P,J,GP,JP,GJ,GPJ,threeplots etc
  # stage is 1: 8
  # mort is 9:16
  # mortnum is 17:24
  # inci is 25:32
  # inci_num is 33:40
  # both is 41:48
  # attack rate is 49:56
  # chronic_number is 57:64
  # R0t is 65:72
  # Below are input names
  plottypes=c("stage","mort","mort_num","inci","inci_num","inci_mort_num","point_prev","chronic_num","R0t")
  # Below are numbering according to long list above from genplots
  plotnums=c(1,2,3,4,5,6,7,8)
  choices=c("Gen pop","PWID","Prison","Gen pop + PWID","Prison + PWID","Gen pop + Prison","Gen pop + PWID + Prison","Triptych")
  i=which(choices==plotid)
  j=which(plottypes==plottype)
  return((j-1)*8+i)
}

get_title_footnote=function(plotid){
  choices=c("Gen pop","PWID","Prison","Gen pop + PWID","Prison + PWID","Gen pop + Prison","Gen pop + PWID + Prison","Triptych")
  i=which(choices==plotid)
  if (i==1){
    foot=footnote_1
    trt=trtmessage_1
  } else if (i==2){
    foot=footnote_2
    trt=trtmessage_2
  }else if (i==3){
    foot=footnote_3
    trt=trtmessage_3
  }else if (i==4){
    foot=footnote_4
    trt=trtmessage_4
  }else if (i==5){
    foot=footnote_5
    trt=trtmessage_5
  }else if (i==6){
    foot=footnote_6
    trt=trtmessage_6
  }else if (i==7){
    foot=footnote_7
    trt=trtmessage_7
  }
  return(list(foot=foot,trt=trt))
}

control13plots=function(plottype,plotid){
  # This returns the correct number plot form teh selection of 8
  # plotype is the inic ,mort etc
  # plot id is G,P,J,GP,JP,GJ,GPJ,threeplots etc
  # stage is 1: 8
  # mort is 9:16
  # mortnum is 17:24
  # inci is 25:32
  # inci_num is 33:40
  # both is 41:48
  # attack rate is 49:56
  # chronic_number is 57:64
  # R0t is 65:72
  # cum treat is 73:80
  # tot treats is 81:88
  # casepert is 89:96
  # prevc 97:104
  # Below are input names
  plottypes=c("stage","mort","mort_num","inci","inci_num","inci_mort_num","point_prev","chronic_num","R0t",
              "attack_pert","treat","caseav","prev_prev")
  # Below are numbering according to long list above from genplots
  choices=c("Gen pop","PWID","Prison","Gen pop + PWID","Prison + PWID","Gen pop + Prison","Gen pop + PWID + Prison","Triptych")
  i=which(choices==plotid)
  j=which(plottypes==plottype)
  z=(j-1)*8+i
  # Warning: Error in [[: attempt to select less than one element in get1index
  if (length((j-1)*8+i)==0){
    z=1 
  }
  return(z)
}

controlicerplots=function(plotid){
  # This returns the correct number plot form teh selection of 8
  # plotype is the inic ,mort etc
  # plot id is G,P,J,GP,JP,GJ,GPJ,threeplots etc
  # stage is 1: 8
  # mort is 9:16
  # mortnum is 17:24
  # inci is 25:32
  # inci_num is 33:40
  # both is 41:48
  # attack rate is 49:56
  # chronic_number is 57:64
  # R0t is 65:72
  # cum treat is 73:80
  # tot treats is 81:88
  # casepert is 89:96
  # prevc 97:104
  # Below are input names
 
  # Below are numbering according to long list above from genplots
  choices=c("Gen pop","PWID","Prison","Gen pop + PWID","Prison + PWID","Gen pop + Prison","Gen pop + PWID + Prison","Triptych")
  i=which(choices==plotid)
  if (i==8){
    i=7 # for now
  }
  return(104+i) # 104 depends on the other options
  
}
default_vals=function(){
  default=c(12,0,0,0,6,0,
            12,0,0,0,0,0,0,0,6,0,
            12,0,0,0,0,0,0,0,6,0)
  default_df = data.frame(matrix(default,nrow=1,ncol=26))
  names(default_df)= c("DAA_duration_gen_three_GP","early_gen_three_GP","advan_gen_three_GP","screening_cov_three_GP","diag_time_gen_three_GP","fixed_coverage_three_GP",
               "DAA_duration_three_PWID","early_former_three_PWID","advan_former_three_PWID","early_current_three_PWID","advan_current_three_PWID",
               "NSP_cov_three_PWID","OST_cov_three_PWID","NSP_OST_cov_three_PWID","diag_time_three_PWID",
               "fixed_coverage_three_PWID",
               "DAA_duration_three_PRIS","early_former_three_PRIS","advan_former_three_PRIS","early_current_three_PRIS","advan_current_three_PRIS",
               "NSP_cov_three_PRIS","OST_cov_three_PRIS","NSP_OST_cov_three_PRIS","diag_time_three_PRIS",
               "fixed_coverage_three_PRIS")  
  default_df$fixed_coverage_three_GP="Yes"
  default_df$fixed_coverage_three_PWID="Yes"
  default_df$fixed_coverage_three_PRIS="Yes"
  # print("here")
  return(default_df)
}

retvals = function(xglob){

  dum=c(
  xglob$DAA_duration_gen_three_GP(),
  xglob$early_gen_three_GP(),
  xglob$advan_gen_three_GP(),
  xglob$screening_cov_three_GP(),
  xglob$diag_time_gen_three_GP(),
  xglob$fixed_coverage_three_GP(),
  xglob$DAA_duration_three_PWID(),
  xglob$early_former_three_PWID(),
  xglob$advan_former_three_PWID(),
  xglob$early_current_three_PWID(),
  xglob$advan_current_three_PWID(),
  xglob$NSP_cov_three_PWID(),
  xglob$OST_cov_three_PWID(),
  xglob$NSP_OST_cov_three_PWID(),
  xglob$diag_time_three_PWID(),
  xglob$fixed_coverage_three_PWID(),
  xglob$DAA_duration_three_PRIS(),
  xglob$early_former_three_PRIS(),
  xglob$advan_former_three_PRIS(),
  xglob$early_current_three_PRIS(),
  xglob$advan_current_three_PRIS(),
  xglob$NSP_cov_three_PRIS(),
  xglob$OST_cov_three_PRIS(),
  xglob$NSP_OST_cov_three_PRIS(),
  xglob$diag_time_three_PRIS(),
  xglob$fixed_coverage_three_PRIS(),
  xglob$discount_rate())

  # duration
  convert_to_nums=c(1,7,17)
  for ( i in 1 : 3){
    if (dum[convert_to_nums[i]]=="weeks12"){
      dum[convert_to_nums[i]]="12"
    }else{
      dum[convert_to_nums[i]]="8"
    }
  }
  # fixed coverge
  convert_to_IO=c(6,16,26)
  for ( i in 1 : 3){
    if (dum[convert_to_IO[i]]=="Yes"){
      dum[convert_to_IO[i]]="0" # to be consistent with earlier versions 0 means ignore the coverage
    }else{
      dum[convert_to_IO[i]]="1"
    }
  }
  dum = as.numeric(dum)
  change_to_rate_from_prop1000 = c(2,3,8,9,10,11,18,19,20,21)
  change_to_rate_from_prop100 = c(4,12,13,14,22,23,24)
  change_to_rate_from_time = c(1,5,7,15,17,25)
  dum[change_to_rate_from_prop1000] = -log(1-dum[change_to_rate_from_prop1000]/1000)
  dum[change_to_rate_from_prop100] = -log(1-dum[change_to_rate_from_prop100]/100)
  trt_duration = change_to_rate_from_time[c(1,3,5)] # weeks
  diag_duration = change_to_rate_from_time[c(2,4,6)] # months
  dum[trt_duration]=52/dum[trt_duration]
  # NO this is assumed to be time - it is converted to rate later
  # BUT it must be converted to years
  dum[diag_duration]=dum[diag_duration]/12
  df = data.frame(matrix(dum,nrow=1,ncol=27))
  names(df)= c("DAA_duration_gen_three_GP","early_gen_three_GP","advan_gen_three_GP","screening_cov_three_GP","diag_time_gen_three_GP","fixed_coverage_three_GP",
                  "DAA_duration_three_PWID","early_former_three_PWID","advan_former_three_PWID","early_current_three_PWID","advan_current_three_PWID",
                  "NSP_cov_three_PWID","OST_cov_three_PWID","NSP_OST_cov_three_PWID","diag_time_three_PWID",
                  "fixed_coverage_three_PWID",
                  "DAA_duration_three_PRIS","early_former_three_PRIS","advan_former_three_PRIS","early_current_three_PRIS","advan_current_three_PRIS",
                  "NSP_cov_three_PRIS","OST_cov_three_PRIS","NSP_OST_cov_three_PRIS","diag_time_three_PRIS",
                  "fixed_coverage_three_PRIS","discount_rate")
  # print(df)
  # write_yaml(df,"/home/mrcuser/HCV_vV/HCV_prison/int.YAML")
return(df)
}

