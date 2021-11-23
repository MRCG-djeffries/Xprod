genpop_intervention_module_ui <- function(id) {
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(title = "Gen pop interventions",titleWidth = 320),
    dashboardSidebar(tags$style(HTML("
      .main-sidebar{
        width: 320px;
      }
    ")),    tags$head(
      tags$style(type="text/css", "#inliney label{ float:left; }" )
    ),     shinyjs::useShinyjs(),    chooseSliderSkin("Flat", color = "blue"),
    div(style="display: inline-block;vertical-align:top;",tags$h5("Gen pop intervention from 2022")),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("int_detail2"), label="Details",size="xs")),
    tags$div(style = "height:10px;",id = "inliney",radioButtons(ns("DAA_duration_gen"), label=div(style="margin-right: 10px;","DAA"),inline=TRUE,selected = "weeks12",c("8 weeks" = "weeks8","12 weeks" = "weeks12"))),
    tags$br(),
    div(style = "height:0px;",tags$hr()),
    tags$h5("Reach per 1,000 subjects"),
    (div(style="display: inline-block;vertical-align:top;width: 155px;",div(style = "height:80px;",
                                                                                  sliderInput(ns("early_gen"), div(style = "font-size:10px","F0,F1,F2"),
                                                                                              min = 0, max = 100,
                                                                                              value = 0, step = 1)))),
    (div(style="display: inline-block;vertical-align:top;width: 155px; ",div(style = "height:80px;",
                                                                                   sliderInput(ns("advan_gen"), div(style = "font-size:10px","F3,F4"),
                                                                                               min = 0, max = 100,
                                                                                               value = 0, step = 1)))),
    tags$br(),
    tags$hr(),
    tags$h5(id="screentag","Screening:"),
    div(id="div_screening_cov",style="display: inline-block;vertical-align:top;width: 155px;",(sliderInput(ns("screening_cov"), div(style = "font-size:10px","coverage(%)"),
                                                                                                         min = 0, max = 50,
                                                                                                         value = 0, step = 1))),
    div(id="div_diag_time_gen",style="display: inline-block;vertical-align:top;width: 155px;",(sliderInput(ns("diag_time_gen"), div(style = "font-size:10px","diagnosis(months)"),
                                                                                                       min = 1, max = 120,
                                                                                                       value = 6, step = 1))),
    tags$div(id = "inliney",radioButtons(ns("fixed_coverage"), "DAA treat only...",inline=TRUE,selected = "No",c("Yes" = "Yes","No" = "No"))),
    tags$br(),
    tags$hr(),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("run_sim_all"), label="Run simulation",size="xs")),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("reset_input2"), label="Reset",size="xs")),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("faqs"), label="FAQs",size="xs")),
    hidden(div(id = "div_plotoutA_gen",style="display: inline-block;vertical-align:top;width: 155px;",(selectInput(ns("plotoutA_gen"), "plot WHO targets:",
                                                                                                               c("Mortality" = "mort_GP",
                                                                                                                 "Incidence" = "inci_GP",
                                                                                                                 "Mortality#" = "mort_num_GP",
                                                                                                                 "Incidence#" = "inci_num_GP",
                                                                                                                 "#Inci & #Mort"="inci_mort_num_GP"
                                                                                                               ),
                                                                                                               multiple = FALSE,size=5,selectize=FALSE,selected=character(0)
    )))),
    hidden(div(id="div_plotoutB_gen",style="display: inline-block;vertical-align:top;width: 155px;",(selectInput(ns("plotoutB_gen"), "plot epi outputs:",
                                                                                                             c("Treatments" = "treat_GP",
                                                                                                               "Cases per treat" = "caseav_GP",
                                                                                                               "Attack rate" = "attack_GP",
                                                                                                               "Chronic stages" = "stage_GP",
                                                                                                               "Cumulative" ="attack_pert_GP",
                                                                                                               "Screened per year" ="prev_prev_GP",
                                                                                                               "Chronic #" ="chronic_num_GP"
                                                                                                        
                                                                                                             ),
                                                                                                             multiple = FALSE,size=7,selectize=FALSE,selected=character(0)
    ))))
    ),
    
    dashboardBody(shinyjs::useShinyjs(),tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '
    ))), 
    h5(id="wordy",htmlOutput(ns("plot_simtext"), container = span)),tags$style(HTML("#wordy{color: red;}")),
    fluidRow(
      column(width = 12,
             mainPanel(plotOutput(ns("plot_genmodel_GP"),height="600px") %>% withSpinner() )
      )
    )
    )
  )
  
}

genpop_intervention_module <- function(input, output, session) {
  
  output$plot_genmodel_GP =  renderPlot(NULL) # so no spinner until button pushed

  data3 = eventReactive(input$early_gen,{
    list(Pgenmild=-log(1-input$early_gen/1000))
  })
  data4 = eventReactive(input$advan_gen,{
    list(Pgenadva=-log(1-input$advan_gen/1000)) # p=1-exp(rate)
  })
  
  data6 = eventReactive(input$diag_time_gen,{
    list(Ditimedur=input$diag_time_gen/12)
  })
  observeEvent(input$fixed_coverage,{
    if (input$fixed_coverage=="Yes"){
      shinyjs::hide(id="div_diag_time_gen",asis=TRUE)
      shinyjs::hide(id="div_screening_cov",asis=TRUE)
      shinyjs::hide(id="screentag",asis=TRUE)
    }else{
      shinyjs::show(id="div_diag_time_gen",asis=TRUE)
      shinyjs::show(id="div_screening_cov",asis=TRUE)
      shinyjs::show(id="screentag",asis=TRUE)
    }
  })  
  observeEvent(input$run_sim_all,{
    
    if ((input$screening_cov)>100){
      # test that the coverage is 100% or less
      shinyalert(html=TRUE,
                 title = "Prevention coverage error",
                 text = paste0("<strong style='color: red;'>","Total coverage must be 100% or less","</strong>"),
                 type = "error",
                 showConfirmButton = TRUE,
                 confirmButtonCol = '#DD6B55',
                 confirmButtonText = 'OK'
      )
      # response is reactive so when changed in the alert it fires this event
    } else{
      updateSelectizeInput(session, "plotoutA_gen", selected = character(0))
      updateSelectizeInput(session, "plotoutB_gen", selected = character(0))
      session$sendCustomMessage(type = "resetValue", message = "plotoutA_gen")
      session$sendCustomMessage(type = "resetValue", message = "plotoutB_gen")
      output$plot_genmodel_GP =  renderPlot(NULL)
      if (input$fixed_coverage=="Yes"){
        diagnosis_time=NA
        fcov_val=1
      }else{
        diagnosis_time=data6()$Ditimedur
        fcov_val=0
      }
      
      X_GP<<-run_gen_pop_v7(data3()$Pgenmild,data4()$Pgenadva, -log(1-input$screening_cov/100),diagnosis_time,fcov_val,7,0,8,52/(if (input$DAA_duration_gen=="weeks8") 8 else 12))
      shinyjs::show(id="div_plotoutA_gen",asis=TRUE)
      shinyjs::show(id="div_plotoutB_gen",asis=TRUE)
      output$plot_simtext = renderUI({HTML("Select required output")})
    }
  })
  
  observeEvent(input$plotoutA_gen,{
    if (input$plotoutA_gen=="mort_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plotmort)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    if (input$plotoutA_gen=="inci_GP"){
      output$plot_genmodel_GP=  renderPlot(
        plot(X_GP$plotinci)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    if (input$plotoutA_gen=="mort_num_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plotmort_num)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    if (input$plotoutA_gen=="inci_num_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plotinci_num)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    if (input$plotoutA_gen=="inci_mort_num_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plot_both)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    updateSelectizeInput(session, "plotoutB_gen", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "plotoutB_gen")    
  })
  
  observeEvent(input$plotoutB_gen,{
    if (input$plotoutB_gen=="treat_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plottreats)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    if (input$plotoutB_gen=="caseav_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plotcasepertreat)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    if (input$plotoutB_gen=="attack_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plotattackrate)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    if (input$plotoutB_gen=="stage_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plotstages)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    if (input$plotoutB_gen=="attack_pert_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plotattackrate_per_treat)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    if (input$plotoutB_gen=="prev_prev_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plotprevent_count)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    if (input$plotoutB_gen=="chronic_num_GP"){
      output$plot_genmodel_GP =  renderPlot(
        plot(X_GP$plotchronic_num)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_genmodel_GP"))  )
    }
    updateSelectizeInput(session, "plotoutA_gen", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "plotoutA_gen")
  })
  observeEvent(input$reset_input2,{
    reset("early_gen")
    reset("advan_gen")
    reset("diag_time_gen")
    reset("fixed_coverage")
    reset("screening_cov")
    shinyjs::show(id="screentag",asis=TRUE)
    updateSelectizeInput(session, "plotoutA_gen", selected = character(0))
    updateSelectizeInput(session, "plotoutB_gen", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "plotoutA_gen")
    session$sendCustomMessage(type = "resetValue", message = "plotoutB_gen")
    output$plot_genmodel_GP =  renderPlot(NULL)
    output$plot_simtext = renderUI({HTML("Select required interventions and click run simulation")})
    shinyjs::hide(id="div_plotoutA_gen",asis=TRUE)
    shinyjs::hide(id="div_plotoutB_gen",asis=TRUE)
  })  
  
  observeEvent(input$int_detail2, {
    ns =session$ns
    
    showModal(modalDialog(
      size="l",
      HTML('<img src="genpopfit.PNG">')))
  })
  
  observeEvent(input$faqs, {
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","genpop_HCV_faqs.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  
  # observeEvent(input$faqs, {
  #   ns =session$ns
  #   
  #   showModal(modalDialog(
  #     size="l",
  #     HTML('<img src="faqs.PNG">')))
  # })
  
  output$plot_simtext = renderUI({HTML("Select required interventions and click run simulation")})
  
  return(input)
  
  
}
