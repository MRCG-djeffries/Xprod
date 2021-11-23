fit_to_data_module_ui <- function(id) {
  ns <- NS(id)
  
  # tagList(
  dashboardPage(
    dashboardHeader(title = "choose model",titleWidth = 320),
    dashboardSidebar(width = 320, shinyjs::useShinyjs(),
                     tags$style(paste0("#",ns("scenario"),"{width: 100px;}")),
                     #tags$style(HTML("#ns(scenario) {width: 100px;}")),
                     hidden(numericInput(ns("scenario"), "Select scenario (1 to 242):",
                                value = 122,min=1,max=242)),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("model_fit1"), label="Fit PWID/Gen pop model ignoring prison",size="xs")),
                     #div(style="display: inline-block;vertical-align:top;",actionBttn(ns("model_fit2"), label="Fit PWID model 2",size="xs")),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("genpop_fit_butt"), label="Fit PWID/Gen pop model including prison",size="xs")),
                     tags$hr(),
                     tags$h5("These are live model fits dynamically running the compartment model for each parameter set"),
                     tags$hr(),
                     # tags$h4(id="wordy2","Note the parameter sets were obtained by minimising the errors to the two fitting points on the epidemic data page (50% and 45%). This is an intenisve multi-variate optimization process and is not suitable for server sided applications. Additionally there can be
                     #         subtle differences in fit, depending which parameters are actually optimized for",
                     #         tags$style(HTML("#wordy2{color: yellow;}"))),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("opt_detail"), label="Optimization details",size="xs")),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("fit_detail"), label="PWID Fit details",size="xs")),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("genpop_fit"), label="Gen Pop fit details",size="xs")),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("prisonpop_fit"), label="Prison Pop fit details",size="xs")),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("scenario_fit"), label="Scenario fitting",size="xs")),
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
             mainPanel(hidden(downloadLink(ns("downloadPlot"), "Download Plot")),plotOutput(ns("plot_model"),height="900px") %>% withSpinner()),
      )
    )
    )
  )
  
}

fit_to_data_module <- function(input, output, session) {
  #values = reactiveValues(scen = 1)
  plotoutFD <- reactiveValues()
  fileoutFD <- reactiveValues()
  output$plot_model =  renderPlot(NULL) # so no spinner until button pushed
  # observeEvent(input$scenario, {
  #   values$scen = input$scenario
  # })                   
  observeEvent(input$model_fit1, {
    
    output$plot_model =  renderPlot(NULL)
    output$plot_text = renderUI({HTML("The model is fit to the epidemic data, by estimating starting values and varying calibration parameter")})
    shinyjs::hide("downloadPlot")
    # output$plot_text <- renderUI({HTML(paste0("Fit for 3 parameters: probability of transmission, 
    # F0 to F1 rate for current and former PWIDs.","<br>", 
    # "The fitted rates of F0 to F1 for current and former are 0.087 and 0.063","<br>",
    # "Compared to the literature estimated rates of 0.106 and 0.116","<br>",
    # "Note in the parameter database, there is no value for probability of transmission (estimated as 13.9%)","<br>",
    # "GENERALLY AT LEAST ONE PARAMETER IS USED TO CALIBRATE THE MODEL TO KNOWN EPIDEMIC DATA"
    # ))})
    output$plot_text <- renderUI({HTML(paste0(""))})
    #print(values$scen)
    #saveRDS(values$scen,"www/scenarioval.rds")
    X=testnewstartPWID_v4(3.5,1,0,1)
    #X=runmodel_v4(input$scenario)#(values$scen)
    output$plot_model =  renderPlot(grid.arrange(X$stage_G,X$stage_P,nrow = 2,heights = c(1, 1)))#,layout_matrix=rbind(c(1,1,2),c(3,4,5))))
    plotoutFD$stage_G = X$stage_G
    plotoutFD$stage_P=X$stage_P
    fileoutFD$title = "PWID_Gen_Pop_Model_Ignoring_Prison.png"
    ns =session$ns
    output$ui =renderUI(   plotOutput(ns("plot_model"))  )
    shinyjs::show("downloadPlot")
  })
  observeEvent(input$reset_input,{
    output$plot_model =  renderPlot(NULL)
    output$plot_text = renderUI({HTML("The model is fit to the epidemic data, by estimating starting values and varying calibration parameter")})
    shinyjs::hide("downloadPlot")
  })
 
  
  observeEvent(input$genpop_fit_butt, {
    
    output$plot_model =  renderPlot(NULL)
    output$plot_text = renderUI({HTML("The model is fit to the epidemic data, by estimating starting values and varying calibration parameter")})
    shinyjs::hide("downloadPlot")
    
    output$plot_text <- renderUI({HTML(paste0(""))})
    #X=run_gen_pop_v7(0,0,0,1,0,73,0,8,52/8)
    X=testnewstartPWID_v4(3.5,0,0,1)
    output$plot_model =  renderPlot(grid.arrange(X$stage_G,X$stage_P,X$stage_J,nrow = 3,heights = c(1, 1,1)))#,layout_matrix=rbind(c(1,1,2),c(3,4,5))))
    plotoutFD$stage_G = X$stage_G
    plotoutFD$stage_P=X$stage_P
    plotoutFD$stage_J=X$stage_J
    fileoutFD$title = "PWID_Gen_Pop_Jail_Model_Including_Prison.png"
    ns =session$ns
    output$ui =renderUI(plotOutput(ns("plot_model"))  )
    shinyjs::show("downloadPlot")
  })
  
  output$plot_text = renderUI({HTML("The model is fit to the epidemic data, by estimating starting values and varying calibration parameter")})
  #output$plot_text <- renderText({"The model is fit to the epideic data, by varying 3 (fit 1) or 5 (fit 2) parameters"})
  observeEvent(input$opt_detail, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    
    showModal(modalDialog(
      size="l",
      HTML('<img src="optofig.PNG">')))
   })
  
  observeEvent(input$fit_detail, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    
    showModal(modalDialog(
      size="l",
      HTML('<img src="pwid_fit.PNG">')))
  })
  
  observeEvent(input$genpop_fit, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    
    showModal(modalDialog(
      size="l",
      HTML('<img src="genpopfit.PNG">')))
  })
  
  observeEvent(input$prisonpop_fit, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    
    showModal(modalDialog(
      size="l",
      HTML('<img src="fitprisonpop.PNG">')))
  })
  
  observeEvent(input$scenario_fit, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    
    showModal(modalDialog(
      size="l",
      HTML('<img src="scenario.PNG">')))
  })
  
  output$downloadPlot <- downloadHandler(

    filename = function(){paste(fileoutFD$title, sep = '')}, # note even for fixed name this has to be a function
    
    content = function(file){
      
      png(file, width = 700, height = 1000)
      if (grepl("Ignoring",fileoutFD$title)==TRUE){
          plot(grid.arrange(plotoutFD$stage_G,plotoutFD$stage_P,nrow = 2,heights = c(1, 1)))
      }else{
          plot(grid.arrange(plotoutFD$stage_G,plotoutFD$stage_P,plotoutFD$stage_J,nrow = 3,heights = c(1, 1,1)))
      }
      dev.off()
    })
  
  
  
  
  
  return(input)
}
