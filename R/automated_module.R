automated_module_ui <- function(id) {
  ns <- NS(id)
  
  # tagList(
  dashboardPage(
    dashboardHeader(title = "choose output",titleWidth = 320),
    dashboardSidebar(width = 320, shinyjs::useShinyjs(),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("output_description"), label="Types of output",size="xs")),
                     tags$hr(),
                     div(id = "div_1_1",style="display: inline-block;vertical-align:top;",tags$h5("DAA treat only (8 weeks)")),
                     div(id = "div_1_2",style="display: inline-block;vertical-align:top;",tags$h5(" : All PWID vs. All former")),
                     div(id = "div_1_3",style="display: inline-block;vertical-align:top;",tags$h5("gen pop not INCLUDED")),
                     div(id = "div_1_4",style="display: inline-block;vertical-align:top;",actionBttn(ns("display_outbut1"), label="Display Output",size="xs"),style="float:right"),
                     tags$hr(id = "div_1_5"),
                     div(id = "div_2_1",style="display: inline-block;vertical-align:top;",tags$h5("DAA treat only (8 weeks)")),
                     div(id = "div_2_2",style="display: inline-block;vertical-align:top;",tags$h5(" : PWID F0-F3 vs. PWID F4-LT")),
                     div(id = "div_2_3",style="display: inline-block;vertical-align:top;",tags$h5("gen pop not INCLUDED")),
                     div(id = "div_2_4",style="display: inline-block;vertical-align:top;",actionBttn(ns("display_outbut2"), label="Display Output",size="xs"),style="float:right"),
                     tags$hr(id = "div_2_5"),
                     div(id = "div_3_1",style="display: inline-block;vertical-align:top;",tags$h5("DAA treat only (8 weeks)")),
                     div(id = "div_3_2",style="display: inline-block;vertical-align:top;",tags$h5(" : PWID F0-F3 vs. PWID F4-LT")),
                     div(id = "div_3_3",style="display: inline-block;vertical-align:top;",tags$h5("gen pop INCLUDED")),
                     div(id = "div_3_4",style="display: inline-block;vertical-align:top;",actionBttn(ns("display_outbut3"), label="Display Output",size="xs"),style="float:right"),
                     tags$hr(id = "div_3_5"),
                     div(id = "div_5_1",style="display: inline-block;vertical-align:top;",tags$h5("DAA treat only (8 weeks)")),
                     div(id = "div_5_2",style="display: inline-block;vertical-align:top;",tags$h5(" : allocation of x/1000 PWID")),
                     div(id = "div_5_3",style="display: inline-block;vertical-align:top;",tags$h5("gen pop not INCLUDED")),
                     div(id = "div_5_4",style="display: inline-block;vertical-align:top;",actionBttn(ns("display_outbut5"), label="Display Output",size="xs"),style="float:right"),
                     tags$hr(id = "div_5_5"),
                     div(id = "div_4_1",style="display: inline-block;vertical-align:top;",tags$h5("DAA treat (8 weeks) with NSP")),
                     div(id = "div_4_2",style="display: inline-block;vertical-align:top;",tags$h5(" : F0-F3 vs.F4-LT")),
                     div(id = "div_4_3",style="display: inline-block;vertical-align:top;",tags$h5("gen pop not INCLUDED")),
                     div(id = "div_4_4",style="display: inline-block;vertical-align:top;",actionBttn(ns("display_outbut4"), label="Display Output",size="xs"),style="float:right"),
                     hidden(tags$hr(id = "div_4_5")),
                     hidden(div(id = "div_A",style="display: inline-block;vertical-align:top;",actionBttn(ns("daa_M"), label="Year to mortality",size="xs",block=TRUE,color="danger"))),
                     hidden(div(id = "div_B",style="display: inline-block;vertical-align:top;",actionBttn(ns("daa_I"), label="Year to incidence",size="xs",block=TRUE,color="danger"))),
                     hidden(div(id = "div_C",style="display: inline-block;vertical-align:top;",actionBttn(ns("daa_B"), label="Latest year to both",size="xs",block=TRUE,color="danger"))),
                     hidden(div(id = "div_D",style="display: inline-block;vertical-align:top;",actionBttn(ns("daa_C"), label="   ICER   ",size="xs",block=TRUE,color="danger"))),
                     hidden(div(id = "div_E",style="display: inline-block;vertical-align:top;",actionBttn(ns("daa_T"), label="Total treats",size="xs",block=TRUE,color="danger"))),
                     hidden(div(id = "div_F",style="display: inline-block;vertical-align:top;",actionBttn(ns("daa_A"), label="Av. treats/1000 PWID",size="xs",block=TRUE,color="danger"))),
                     hidden(div(id = "radio_div",radioButtons(ns("select_output"),"Select output for plotting", c("Mort" = "M", "Inci" = "I","Both"="B","ICER"="C"), inline=TRUE))),
                     hidden(div(id = "NSP_div",sliderInput(ns("NSP_cov"), div(style = "font-size:10px","NSP coverage"),
                                 min = 1, max = 50,
                                 value = 10, step = 1,animate=animationOptions(interval = 500)))),#(
                     hidden(div(id="plot_NSP",actionBttn(ns("plot_NSP_butt"), label="PLOT",size="xs",color="danger"))),
                     tags$hr(),
                     actionBttn(ns("reset_input"), label="Reset",size="xs")
    ),
    dashboardBody(shinyjs::useShinyjs(),tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '
    ))), 
    h5(id="wordy",htmlOutput(ns("plot_text"), container = span)),tags$style(HTML("#wordy{color: red;}")),
    fluidRow(
      column(width = 12,
             hidden(div(id="down_data",downloadButton(ns("downloaddata"), "Download the data"))),
             mainPanel(plotOutput(ns("plot_model"),height="800px") %>% withSpinner() ),
      )
    )
    )
  )
  
}

automated_module <- function(input, output, session) {
  runy_num<<-99
  butty<<-"nothing"
  output$plot_model =  renderPlot(NULL) # so no spinner until button pushed
                   
  observeEvent(input$model_fit1, {
    output$plot_text <- renderUI({HTML(paste0(""))})
    ns =session$ns
    output$ui =renderUI(   plotOutput(ns("plot_model"))  )
  })

  observeEvent(input$select_output,{
    # null the figure and reset the slider
    reset("NSP_cov")
    shinyjs::hide(id="down_data",asis=TRUE)
    output$plot_model =  renderPlot(NULL)
    output$plot_text = renderUI({HTML("Two dimensional heatmaps will be displayed here")})
    #print(paste0(input$select_output," and ", input$NSP_cov, " and ",runy_num))
  })
   # output$plot_model <- renderPlot({
   #   print(paste0(input$select_output," and ", input$NSP_cov, " and ",runy_num))
   #   if (runy_num==2){
   #    if (is.null(input$select_output)==FALSE & is.null(input$NSP_cov)==FALSE){
   #        p=read_and_plot(2,input$select_output,input$NSP_cov)
   #        plot(p)
   #        output$ui =renderUI(plotOutput(ns("plot_model"))  )
   #        output$plot_text = renderUI({HTML("")})
   #    }
   #   }
   # })
  observeEvent(input$plot_NSP_butt, {
    if (runy_num==2){
      if (is.null(input$select_output)==FALSE & is.null(input$NSP_cov)==FALSE){
              p=read_and_plot(2,input$select_output,input$NSP_cov)
              output$plot_model =  renderPlot(
                plot(p)
              )
              ns =session$ns
              output$ui =renderUI(plotOutput(ns("plot_model"))  )
              output$plot_text = renderUI({HTML("")})
              shinyjs::show(id="down_data",asis=TRUE)
      }
    }
  })
 
  output$plot_text = renderUI({HTML("Two dimensional heatmaps will be displayed here")})
  #output$plot_text <- renderText({"The model is fit to the epideic data, by varying 3 (fit 1) or 5 (fit 2) parameters"})
  observeEvent(input$display_outbut1, {
    shinyjs::show(id="div_A",asis=TRUE)
    shinyjs::show(id="div_B",asis=TRUE)
    shinyjs::show(id="div_C",asis=TRUE)
    shinyjs::show(id="div_D",asis=TRUE)
    shinyjs::show(id="div_E",asis=TRUE)    
    shinyjs::show(id="div_F",asis=TRUE)   
    
    shinyjs::hide(id="div_2_1",asis=TRUE)
    shinyjs::hide(id="div_2_2",asis=TRUE)
    shinyjs::hide(id="div_2_3",asis=TRUE)
    shinyjs::hide(id="div_2_4",asis=TRUE)
    shinyjs::hide(id="div_2_5",asis=TRUE)
    shinyjs::hide(id="div_3_1",asis=TRUE)    
    shinyjs::hide(id="div_3_2",asis=TRUE)  
    shinyjs::hide(id="div_3_3",asis=TRUE)
    shinyjs::hide(id="div_3_4",asis=TRUE)
    shinyjs::hide(id="div_3_5",asis=TRUE)
    shinyjs::hide(id="div_5_1",asis=TRUE)    
    shinyjs::hide(id="div_5_2",asis=TRUE)  
    shinyjs::hide(id="div_5_3",asis=TRUE)
    shinyjs::hide(id="div_5_4",asis=TRUE)
    shinyjs::hide(id="div_5_5",asis=TRUE)
    shinyjs::hide(id="div_4_1",asis=TRUE)
    shinyjs::hide(id="div_4_2",asis=TRUE)
    shinyjs::hide(id="div_4_3",asis=TRUE)    
    shinyjs::hide(id="div_4_4",asis=TRUE)  
    shinyjs::hide(id="div_4_5",asis=TRUE)
    shinyjs::hide(id="div_1_4",asis=TRUE)
    runy_num<<-1
  })
  
  observeEvent(input$display_outbut2, {
    shinyjs::show(id="div_A",asis=TRUE)
    shinyjs::show(id="div_B",asis=TRUE)
    shinyjs::show(id="div_C",asis=TRUE)
    shinyjs::show(id="div_D",asis=TRUE)
    shinyjs::show(id="div_E",asis=TRUE)    
    shinyjs::show(id="div_F",asis=TRUE)   
    
    shinyjs::hide(id="div_1_1",asis=TRUE)
    shinyjs::hide(id="div_1_2",asis=TRUE)
    shinyjs::hide(id="div_1_3",asis=TRUE)
    shinyjs::hide(id="div_1_4",asis=TRUE)
    shinyjs::hide(id="div_1_5",asis=TRUE)
    shinyjs::hide(id="div_3_1",asis=TRUE)    
    shinyjs::hide(id="div_3_2",asis=TRUE)  
    shinyjs::hide(id="div_3_3",asis=TRUE)
    shinyjs::hide(id="div_3_4",asis=TRUE)
    shinyjs::hide(id="div_3_5",asis=TRUE)
    shinyjs::hide(id="div_5_1",asis=TRUE)    
    shinyjs::hide(id="div_5_2",asis=TRUE)  
    shinyjs::hide(id="div_5_3",asis=TRUE)
    shinyjs::hide(id="div_5_4",asis=TRUE)
    shinyjs::hide(id="div_5_5",asis=TRUE)    
    shinyjs::hide(id="div_4_1",asis=TRUE)
    shinyjs::hide(id="div_4_2",asis=TRUE)
    shinyjs::hide(id="div_4_3",asis=TRUE)    
    shinyjs::hide(id="div_4_4",asis=TRUE)  
    shinyjs::hide(id="div_4_5",asis=TRUE)
    shinyjs::hide(id="div_2_4",asis=TRUE)
    runy_num<<-3
  })
  
  observeEvent(input$display_outbut3, {
    shinyjs::show(id="div_A",asis=TRUE)
    shinyjs::show(id="div_B",asis=TRUE)
    shinyjs::show(id="div_C",asis=TRUE)
    shinyjs::show(id="div_D",asis=TRUE)
    shinyjs::show(id="div_E",asis=TRUE)    
    shinyjs::show(id="div_F",asis=TRUE)   
    
    shinyjs::hide(id="div_1_1",asis=TRUE)
    shinyjs::hide(id="div_1_2",asis=TRUE)
    shinyjs::hide(id="div_1_3",asis=TRUE)
    shinyjs::hide(id="div_1_4",asis=TRUE)
    shinyjs::hide(id="div_1_5",asis=TRUE)
    shinyjs::hide(id="div_2_1",asis=TRUE)    
    shinyjs::hide(id="div_2_2",asis=TRUE)  
    shinyjs::hide(id="div_2_3",asis=TRUE)
    shinyjs::hide(id="div_2_4",asis=TRUE)
    shinyjs::hide(id="div_2_5",asis=TRUE)
    shinyjs::hide(id="div_5_1",asis=TRUE)    
    shinyjs::hide(id="div_5_2",asis=TRUE)  
    shinyjs::hide(id="div_5_3",asis=TRUE)
    shinyjs::hide(id="div_5_4",asis=TRUE)
    shinyjs::hide(id="div_5_5",asis=TRUE)
    shinyjs::hide(id="div_4_1",asis=TRUE)
    shinyjs::hide(id="div_4_2",asis=TRUE)
    shinyjs::hide(id="div_4_3",asis=TRUE)    
    shinyjs::hide(id="div_4_4",asis=TRUE)  
    shinyjs::hide(id="div_4_5",asis=TRUE)
    shinyjs::hide(id="div_3_4",asis=TRUE)
    runy_num<<-4
  })
  
  observeEvent(input$display_outbut5, {
    shinyjs::show(id="div_A",asis=TRUE)
    shinyjs::show(id="div_B",asis=TRUE)
    shinyjs::show(id="div_C",asis=TRUE)
    shinyjs::show(id="div_D",asis=TRUE)
    shinyjs::show(id="div_E",asis=TRUE)    
    shinyjs::show(id="div_F",asis=TRUE)   
    
    shinyjs::hide(id="div_1_1",asis=TRUE)
    shinyjs::hide(id="div_1_2",asis=TRUE)
    shinyjs::hide(id="div_1_3",asis=TRUE)
    shinyjs::hide(id="div_1_4",asis=TRUE)
    shinyjs::hide(id="div_1_5",asis=TRUE)
    shinyjs::hide(id="div_2_1",asis=TRUE)    
    shinyjs::hide(id="div_2_2",asis=TRUE)  
    shinyjs::hide(id="div_2_3",asis=TRUE)
    shinyjs::hide(id="div_2_4",asis=TRUE)
    shinyjs::hide(id="div_2_5",asis=TRUE)
    shinyjs::hide(id="div_5_1",asis=TRUE)    
    shinyjs::hide(id="div_5_2",asis=TRUE)  
    shinyjs::hide(id="div_5_3",asis=TRUE)
    shinyjs::hide(id="div_5_4",asis=TRUE)
    shinyjs::hide(id="div_5_5",asis=TRUE)
    shinyjs::hide(id="div_4_1",asis=TRUE)
    shinyjs::hide(id="div_4_2",asis=TRUE)
    shinyjs::hide(id="div_4_3",asis=TRUE)    
    shinyjs::hide(id="div_4_4",asis=TRUE)  
    shinyjs::hide(id="div_4_5",asis=TRUE)
    shinyjs::hide(id="div_3_4",asis=TRUE)
    runy_num<<-5
  })
  
  observeEvent(input$display_outbut4, {
    ns =session$ns
    shinyjs::hide(id="div_A",asis=TRUE)
    shinyjs::hide(id="div_B",asis=TRUE)
    shinyjs::hide(id="div_C",asis=TRUE)
    shinyjs::hide(id="div_D",asis=TRUE)
    shinyjs::hide(id="div_E",asis=TRUE)    
    shinyjs::hide(id="div_F",asis=TRUE)   
    shinyjs::show(id="radio_div",asis=TRUE) 
    shinyjs::show(id="NSP_div",asis=TRUE) 
    shinyjs::show(id="plot_NSP",asis=TRUE) 
    shinyjs::hide(id="div_1_1",asis=TRUE)
    shinyjs::hide(id="div_1_2",asis=TRUE)
    shinyjs::hide(id="div_1_3",asis=TRUE)
    shinyjs::hide(id="div_1_4",asis=TRUE)
    shinyjs::hide(id="div_1_5",asis=TRUE)
    shinyjs::hide(id="div_2_1",asis=TRUE)    
    shinyjs::hide(id="div_2_2",asis=TRUE)  
    shinyjs::hide(id="div_2_3",asis=TRUE)
    shinyjs::hide(id="div_2_4",asis=TRUE)
    shinyjs::hide(id="div_2_5",asis=TRUE)
    shinyjs::hide(id="div_3_1",asis=TRUE)
    shinyjs::hide(id="div_3_2",asis=TRUE)
    shinyjs::hide(id="div_3_3",asis=TRUE)    
    shinyjs::hide(id="div_3_4",asis=TRUE)  
    shinyjs::hide(id="div_3_5",asis=TRUE)
    shinyjs::hide(id="div_5_1",asis=TRUE)    
    shinyjs::hide(id="div_5_2",asis=TRUE)  
    shinyjs::hide(id="div_5_3",asis=TRUE)
    shinyjs::hide(id="div_5_4",asis=TRUE)
    shinyjs::hide(id="div_5_5",asis=TRUE)    
    shinyjs::hide(id="div_4_4",asis=TRUE)
    shinyjs::show(id="div_4_5",asis=TRUE)
    runy_num<<-2
  })
  
  output$downloaddata <- downloadHandler(
    filename = function() {
      t=""
      if (runy_num ==1 | runy_num ==3 | runy_num==4 | runy_num==5){
        if (butty=="M") t="Mortality"
        if (butty=="I") t="Incidence"
        if (butty=="B") t="Mort_Inci"
        if (butty=="C") t="ICER"
        if (butty=="TT") t="TotTreats"
        if (butty=="A") t="AvTreats"
      }
      if (runy_num ==2) {
        if (input$select_output=="M") t="Mortality"
        if (input$select_output=="I") t="Incidence"
        if (input$select_output=="B") t="Mort_Inci"
        if (input$select_output=="C") t="ICER"
      }
      if (runy_num ==1){
         fname=paste0("scenario_1_",t)
      }
      if (runy_num ==3){
        fname=paste0("scenario_2_",t)
      }
      if (runy_num ==4){
        fname=paste0("scenario_3_",t)
      }
      if (runy_num ==5){
        fname=paste0("scenario_5_",t)
      }      
      if (runy_num ==2){
        fname=paste0("scenario_4_",t,"_NSPcov_",input$NSP_cov)
      }
      paste(fname, '.csv', sep='')
    },
    content = function(file) {
      if (runy_num ==1 | runy_num==3 | runy_num==4 | runy_num==5){
          if (runy_num==1){
            Z=readRDS("HPCruns/run1/allmat.rds")
          }
          if (runy_num==3){
            Z=readRDS("HPCruns/run3/allmat.rds")
          }
          if (runy_num==4){
            Z=readRDS("HPCruns/run4/allmat.rds")
          }
          if (runy_num==5){
            Z=readRDS("HPCruns/run5/out_prop_dist.rds")
          }
          if (butty=="M"){
            X=Z$M
          }
          if (butty=="I"){
            X=Z$M
          }
          if (butty=="B"){
            X=Z$M
          }
          if (butty=="C"){
            X=Z$M
          }
          if (butty=="TT"){
            X=Z$M
          }
          if (butty=="A"){
            X=Z$M
          }
      }else{
          #input$select_output,input$NSP_cov
          Z=readRDS(paste0("HPCruns/run2/outmat_",input$NSP_cov,".rds"))
          if (input$select_output=="M"){
            X=Z$M
          }
          if (input$select_output=="I"){
            X=Z$M
          }
          if (input$select_output=="B"){
            X=Z$M
          }
          if (input$select_output=="C"){
            X=Z$M
          }
      }
      if (runy_num!=5){
        row.names(X)=1:100
        colnames(X)=1:100
        write.table(X,file=file,row.names=TRUE,col.names=NA)
      }else{
        write.table(X,file=file,row.names=FALSE,col.names=FALSE)
      }
      
      
    }
  )
  observeEvent(input$daa_M, {
    p=read_and_plot(runy_num,"M")
    output$plot_model =  renderPlot(
      plot(p)
    )
    ns =session$ns
    output$ui =renderUI(plotOutput(ns("plot_model"))  )
    output$plot_text = renderUI({HTML("")})
    shinyjs::show(id="down_data",asis=TRUE)
    butty<<-"M"
  })
  
  observeEvent(input$daa_C, {
    p=read_and_plot(runy_num,"C")
    output$plot_model =  renderPlot(
      plot(p)
    )
    ns =session$ns
    output$ui =renderUI(plotOutput(ns("plot_model"))  )
    output$plot_text = renderUI({HTML("")})
    shinyjs::show(id="down_data",asis=TRUE)
    butty<<-"C"
  })
  
  observeEvent(input$daa_I, {
    p=read_and_plot(runy_num,"I")
    output$plot_model =  renderPlot(
      plot(p)
    )
    ns =session$ns
    output$ui =renderUI(plotOutput(ns("plot_model"))  )
    output$plot_text = renderUI({HTML("")})
    shinyjs::show(id="down_data",asis=TRUE)
    butty<<-"I"
  })
  
  observeEvent(input$daa_B, {
    p=read_and_plot(runy_num,"B")
    output$plot_model =  renderPlot(
      plot(p)
    )
    ns =session$ns
    output$ui =renderUI(plotOutput(ns("plot_model"))  )
    output$plot_text = renderUI({HTML("")})
    shinyjs::show(id="down_data",asis=TRUE)
    butty<<-"B"
  })
  
  observeEvent(input$daa_T, {
    p=read_and_plot(runy_num,"TT")
    output$plot_model =  renderPlot(
      plot(p)
    )
    ns =session$ns
    output$ui =renderUI(plotOutput(ns("plot_model"))  )
    output$plot_text = renderUI({HTML("")})
    shinyjs::show(id="down_data",asis=TRUE)
    butty<<-"TT"
  })
  
  observeEvent(input$daa_A, {
    p=read_and_plot(runy_num,"A")
    output$plot_model =  renderPlot(
      plot(p)
    )
    ns =session$ns
    output$ui =renderUI(plotOutput(ns("plot_model"))  )
    output$plot_text = renderUI({HTML("")})
    shinyjs::show(id="down_data",asis=TRUE)
    butty<<-"A"
  })  
  


  observeEvent(input$output_description, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    
    showModal(modalDialog(
      size="l",
      HTML('<img src="automated.PNG">')))
  })
  
  observeEvent(input$reset_input,{
    ns =session$ns
    reset("NSP_cov")
    reset("select_ouput")
    #reset("plot_model")
    #reset("plot_text")
    shinyjs::hide(id="div_A",asis=TRUE)
    shinyjs::hide(id="div_B",asis=TRUE)
    shinyjs::hide(id="div_C",asis=TRUE)
    shinyjs::hide(id="div_D",asis=TRUE)
    shinyjs::hide(id="div_E",asis=TRUE)
    shinyjs::hide(id="div_F",asis=TRUE)
    shinyjs::hide(id="radio_div",asis=TRUE)
    shinyjs::hide(id="NSP_div",asis=TRUE)
    shinyjs::hide(id="plot_NSP",asis=TRUE) 
    shinyjs::hide(id="down_data",asis=TRUE)
    shinyjs::show(id="div_1_1",asis=TRUE)
    shinyjs::show(id="div_1_2",asis=TRUE)
    shinyjs::show(id="div_1_3",asis=TRUE)
    shinyjs::show(id="div_1_4",asis=TRUE)
    shinyjs::show(id="div_1_5",asis=TRUE)
    shinyjs::show(id="div_2_1",asis=TRUE)
    shinyjs::show(id="div_2_2",asis=TRUE)
    shinyjs::show(id="div_2_3",asis=TRUE)
    shinyjs::show(id="div_2_4",asis=TRUE)
    shinyjs::show(id="div_2_5",asis=TRUE)
    shinyjs::show(id="div_3_1",asis=TRUE)
    shinyjs::show(id="div_3_2",asis=TRUE)
    shinyjs::show(id="div_3_3",asis=TRUE)
    shinyjs::show(id="div_3_4",asis=TRUE)
    shinyjs::show(id="div_3_5",asis=TRUE)
    shinyjs::show(id="div_5_1",asis=TRUE)    
    shinyjs::show(id="div_5_2",asis=TRUE)  
    shinyjs::show(id="div_5_3",asis=TRUE)
    shinyjs::show(id="div_5_4",asis=TRUE)
    shinyjs::show(id="div_5_5",asis=TRUE)
    shinyjs::show(id="div_4_1",asis=TRUE)
    shinyjs::show(id="div_4_2",asis=TRUE)
    shinyjs::show(id="div_4_3",asis=TRUE)
    shinyjs::show(id="div_4_4",asis=TRUE)
    shinyjs::hide(id="div_4_5",asis=TRUE)
    output$plot_model =  renderPlot(NULL)
    output$plot_text = renderUI({HTML("Two dimensional heatmaps will be displayed here")})
    runy_num<<-99
    butty<<-"nothing"
  })
  #return(input)
}
