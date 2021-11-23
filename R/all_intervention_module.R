all_intervention_module_ui <- function(id) {
  ns <- NS(id)
  
  # inlcudes
  # DAA 
  # screening
  # NSP
  # OST
  
  # tagList(
  #tags$style(HTML("
  #    .main-sidebar{
  #      width: 300px;
  #    }
  #  "))
    dashboardPage(
    dashboardHeader(title = "PWID interventions",titleWidth = 320),
    dashboardSidebar(width = 320, shinyjs::useShinyjs(),    chooseSliderSkin("Flat", color = "blue"),
    # dashboardSidebar(tags$style(HTML("
    #   .main-sidebar{
    #     width: 320px;
    #   }
    # ")),    
                     tags$head(
      tags$style(type="text/css", "#inliney label{ float:left; }" )
    ),     shinyjs::useShinyjs(),    chooseSliderSkin("Flat", color = "blue"),
                     div(style="display: inline-block;vertical-align:top;",tags$h5("DAA intervention from 2022")),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("int_detail2"), label="Intervention details",size="xs")),
                     textOutput(ns('scenario_text')),
                     tags$div(style = "height:10px;",id = "inliney",radioButtons(ns("DAA_duration"), label=div(style="margin-right: 10px;","DAA"),inline=TRUE,selected = "weeks12",c("8 weeks" = "weeks8","12 weeks" = "weeks12"))),
                     tags$br(),
                     tags$div(style = "height:10px;",id = "inliney",radioButtons(ns("includegenpop"), label=div(style="margin-right: 10px;","Include genpop (WHO target)"),inline=TRUE,selected = "No",c("Yes" = "Yes","No" = "No"))),
                     tags$br(),
                     div(style = "height:0px;",tags$hr()),
                     div(style="display: inline-block;vertical-align:top;",tags$h5("Reach per 1,000 PWID subjects")),
                     div(style="display: inline-block;vertical-align:top;",actionBttn(ns("reach_butt"), label="Reach details",size="xs")),
                     hidden(div(id="former_F0",style="display: inline-block;vertical-align:top;width: 155px;",div(style = "height:80px;",
                     sliderInput(ns("early_former"), div(style = "font-size:10px","former F0,F1,F2,F3"),
                                 min = 0, max = 100,
                                 value = 0, step = 1)))),
                     hidden(div(id="former_F4",style="display: inline-block;vertical-align:top;width: 155px; ",div(style = "height:80px;",
                     sliderInput(ns("advan_former"), div(style = "font-size:10px","former F4,HCC,DC,LT"),
                                 min = 0, max = 100,
                                 value = 0, step = 1)))),
                     div(style="display: inline-block;vertical-align:top;width: 155px;",div(style = "height:80px;",(sliderInput(ns("early_current"), div(style = "font-size:10px","current F0,F1,F2,F3"),
                                 min = 0, max = 100,
                                 value = 0, step = 1)))),
                     div(style="display: inline-block;vertical-align:top;width: 155px;",div(style = "height:80px;",(sliderInput(ns("advan_current"), div(style = "font-size:10px","current F4,HCC,DC,LT"),
                                 min = 0, max = 100,
                                 value = 0, step = 1)))),
                      tags$hr(),
    div(style="display: inline-block;vertical-align:top;",tags$h5(id="NSP_OSTtag","NSP and OST interventions:")),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("prev_butt"), label="Prevention",size="xs")),
    
    div(id="div_NSP_cov",style="display: inline-block;vertical-align:top;width: 155px;",(sliderInput(ns("NSP_cov"), div(style = "font-size:10px","NSP coverage(%)"),
                                                                                    min = 0, max = 50,
                                                                                    value = 0, step = 1))),
    div(id="div_OST_cov",style="display: inline-block;vertical-align:top;width: 155px;",(sliderInput(ns("OST_cov"), div(style = "font-size:10px","OST coverage(%)"),
                                                                                    min = 0, max = 50,
                                                                                    value = 0, step = 1))),
    div(id="div_NSP_OST_cov",style="display: inline-block;vertical-align:top;width: 155px;",(sliderInput(ns("NSP_OST_cov"), div(style = "font-size:10px","NSP & OST coverage(%)"),
                                                                                    min = 0, max = 50,
                                                                                    value = 0, step = 1))),
    div(id="div_diag_time",style="display: inline-block;vertical-align:top;width: 155px;",(sliderInput(ns("diag_time"), div(style = "font-size:10px","diagnosis(months)"),
                                                                                                       min = 1, max = 120,
                                                                                                       value = 6, step = 1))),
    tags$div(style = "height:10px;",id = "inliney",radioButtons(ns("fixed_coverage"), label=div(style="margin-right: 10px;","DAA treat only"),inline=TRUE,selected = "No",c("Yes" = "Yes","No" = "No"))),
    tags$br(),
    div(style = "height:0px;",tags$hr()),
    #div(style = "height:10px;",tags$h5("% pop growth from 2022 to 2090")),
    #div(style = "height:80px;",(sliderInput(ns("pwid_pop_growth"),label=div(style = "font-size:10px","% growth in PWID susceptible from 2022 to 2090"),min = 0, max = 30,value = 0, step = 1))),
    #tags$div(style = "height:10px;",id = "inliney",radioButtons(ns("pwid_pop_growth"), label=div(style="margin-right: 10px;","PWID pop growth"),inline=TRUE,selected = "Flat",c("Yes" = "Yes","Flat" = "Flat"))),
    #tags$br(),
    #tags$hr(),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("run_sim_all"), label="Run simulation",size="xs")),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("reset_input2"), label="Reset",size="xs")),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("faqs"), label="FAQs",size="xs")),
    hidden(div(id = "div_flexoutA",style="display: inline-block;vertical-align:top;",actionBttn(ns("icer"), label="ICER",size="xs"))),
    hidden(div(id = "div_plotoutA",style="display: inline-block;vertical-align:top;width: 155px;",(selectInput(ns("plotoutA"), "plot WHO targets:",
                                                                                    c("Mortality" = "mort",
                                                                                      "Incidence" = "inci",
                                                                                      "Mortality#" = "mort_num",
                                                                                      "Incidence#" = "inci_num",
                                                                                      "#Inci & #Mort"="inci_mort_num"
                                                                                    ),
                                                                                    multiple = FALSE,size=5,selectize=FALSE,selected=character(0)
    )))),
    hidden(div(id="div_plotoutB",style="display: inline-block;vertical-align:top;width: 155px;",(selectInput(ns("plotoutB"), "plot epi outputs:",
                                                                                    c("Treatments" = "treat",
                                                                                      "Cases per treat" = "caseav",
                                                                                      "Attack rate" = "attack",
                                                                                      "Chronic stages" = "stage",
                                                                                      "Cumulative" ="attack_pert",
                                                                                      "Prevention per year" ="prev_prev",
                                                                                      "Chronic #" ="chronic_num"
                                                                                      #"Chronic # long" ="chronic_num_long",
                                                                                      #"PWID #" ="pwid_num"
                                                                                      #"PWID # long" ="pwid_num_long"
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
    h5(id="wordy2",htmlOutput(ns("plot_simtext2"), container = span)),tags$style(HTML("#wordy2{color: red;}")),
    fluidRow(
      hidden(div(id = "div_plotsimmodel",column(width = 12,
             mainPanel(plotOutput(ns("plot_simmodel"),height="600px") %>% withSpinner() )))
      ),
      #hidden(div(id = "div_plot_flex",column(width = 12,
      #       mainPanel(uiOutput(ns("plot_flex"),height="600px") )))
             hidden(div(id = "div_plot_flex",column(width = 12,
             mainPanel(DTOutput(ns("plot_flex"),height="600px") )))
      )
             
    )
    )
  )
  
}

all_intervention_module <- function(input, output, session,scenario_number,genpop_input) {
  
  output$plot_simmodel =  renderPlot(NULL) # so no spinner until button pushed
  # show("plot_simmodel")
  # data =list(cess=-1,rela=-1)
  
  output$scenario_text <- renderText({paste0("Scenario # ",scenario_number$scenario) })
  data1 = eventReactive(input$early_former,{
    list(Pformermild=-log(1-input$early_former/1000))
  })
  data2 = eventReactive(input$advan_former,{
    list(Pformeradva=-log(1-input$advan_former/1000)) # p=1-exp(rate)
  })
  data3 = eventReactive(input$early_current,{
    list(Pcurrenmild=-log(1-input$early_current/1000))
  })
  data4 = eventReactive(input$advan_current,{
    list(Pcurrenadva=-log(1-input$advan_current/1000)) # p=1-exp(rate)
  })
  data6 = eventReactive(input$diag_time,{
    list(Ditimedur=input$diag_time/12)
  })
  observeEvent(input$fixed_coverage,{
    #print(paste0("here","_",input$fixed_coverage))
    if (input$fixed_coverage=="Yes"){
      shinyjs::hide(id="div_diag_time",asis=TRUE)
      shinyjs::hide(id="div_NSP_OST_cov",asis=TRUE)
      shinyjs::hide(id="div_OST_cov",asis=TRUE)
      shinyjs::hide(id="div_NSP_cov",asis=TRUE)
      shinyjs::hide(id="NSP_OSTtag",asis=TRUE)
      shinyjs::show(id="former_F0",asis=TRUE)
      shinyjs::show(id="former_F4",asis=TRUE)
    }else{
      shinyjs::show(id="div_diag_time",asis=TRUE)
      shinyjs::show(id="div_NSP_OST_cov",asis=TRUE)
      shinyjs::show(id="div_OST_cov",asis=TRUE)
      shinyjs::show(id="div_NSP_cov",asis=TRUE)
      shinyjs::show(id="NSP_OSTtag",asis=TRUE)
      shinyjs::hide(id="former_F0",asis=TRUE)
      shinyjs::hide(id="former_F4",asis=TRUE)
    }
  })  
  observeEvent(input$run_sim_all,{
    
    if ((input$NSP_cov+input$OST_cov+input$NSP_OST_cov)>100){
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
    updateSelectizeInput(session, "plotoutA", selected = character(0))
    updateSelectizeInput(session, "plotoutB", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "plotoutA")
    session$sendCustomMessage(type = "resetValue", message = "plotoutB")
    output$plot_simmodel =  renderPlot(NULL)
    output$plot_flex=renderUI({NULL})
    if (input$fixed_coverage=="Yes"){
        diagnosis_time=NA
      }else{
        diagnosis_time=data6()$Ditimedur
      }
    #print(data1()$Pformermild)
    if (input$includegenpop=="No"){
      mortvec=rep(0,36)
      incivec=rep(0,36)
    } else {
      if (genpop_input$fixed_coverage=="Yes"){
        gen_diagnosis_time=NA
        gen_fcov_val=1
      }else{
        gen_diagnosis_time=genpop_input$diag_time_gen/12
        gen_fcov_val=0
      }
      dum=run_gen_pop_v7(-log(1-genpop_input$early_gen/1000),-log(1-genpop_input$advan_gen/1000),-log(1-genpop_input$screening_cov/100),
                         gen_diagnosis_time,gen_fcov_val,
                         7,0,8,
                         52/(if (genpop_input$DAA_duration_gen=="weeks8") 8 else 12))
      mortvec=dum$plotmort_num_data
      incivec=dum$plotinci_num_data
    }
    X<<-runall_intervention_vnsp_ost_long_v3(data1()$Pformermild,data2()$Pformeradva,data3()$Pcurrenmild,data4()$Pcurrenadva,
                                     input$fixed_coverage,
                                     -log(1-input$NSP_cov/100),-log(1-input$OST_cov/100),-log(1-input$NSP_OST_cov/100),
                                     diagnosis_time,scenario_number$scenario,
                                     52/(if (input$DAA_duration=="weeks8") 8 else 12),
                                     mortvec,incivec) # note input$pwid_pop_growth not now used

    Ftab<<-output_ICER_text_v2(sum(X$COST),sum(X$QALYS),sum(X$Qdattreat$COST),sum(X$Qdattreat$QALYS),input$fixed_coverage,
                               X$stage_vals,X$Qdattreat$stage_vals,scenario_number$scenario) 
    # saveRDS(Ftab,"testFtab.RDS")
      shinyjs::show(id="div_plotoutA",asis=TRUE)
      shinyjs::show(id="div_flexoutA",asis=TRUE)
      shinyjs::show(id="div_plotoutB",asis=TRUE)
      shinyjs::show(id="div_but_genpop",asis=TRUE)
      output$plot_simtext2 = renderUI({HTML("Select required output")})
    }
  })
  
  observeEvent(input$plotoutA,{
    shinyjs::hide(id="div_plot_flex",asis=TRUE)
    shinyjs::show(id="div_plotsimmodel",asis=TRUE)
    if (input$plotoutA=="mort"){
      output$plot_simmodel =  renderPlot(
        plot(X$plotmort)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutA=="inci"){
      output$plot_simmodel =  renderPlot(
        plot(X$plotinci)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutA=="mort_num"){
      output$plot_simmodel =  renderPlot(
        plot(X$plotmort_num)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutA=="inci_num"){
      output$plot_simmodel =  renderPlot(
        plot(X$plotinci_num)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutA=="inci_mort_num"){
      output$plot_simmodel =  renderPlot(
        plot(X$plot_both)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    updateSelectizeInput(session, "plotoutB", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "plotoutB")    
  })
  
  observeEvent(input$plotoutB,{
    shinyjs::hide(id="div_plot_flex",asis=TRUE)
    shinyjs::show(id="div_plotsimmodel",asis=TRUE)
    if (input$plotoutB=="treat"){
      output$plot_simmodel =  renderPlot(
        plot(X$plottreats)
      )
      ns =session$ns
      #show(ns("plot_simmodel"))
      #hide(ns("plot_flex"))
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutB=="caseav"){
      output$plot_simmodel =  renderPlot(
        plot(X$plotcasepertreat)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutB=="attack"){
      output$plot_simmodel =  renderPlot(
        plot(X$attackrate)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutB=="stage"){
      output$plot_simmodel =  renderPlot(
        plot(X$stages)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutB=="attack_pert"){
      output$plot_simmodel =  renderPlot(
        plot(X$attackrate_per_treat)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutB=="prev_prev"){
      output$plot_simmodel =  renderPlot(
        plot(X$prevent_count)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutB=="chronic_num"){
      output$plot_simmodel =  renderPlot(
        plot(X$plotchronic_num)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutB=="chronic_num_long"){
      output$plot_simmodel =  renderPlot(
        plot(X$plotchronic_num_long)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutB=="pwid_num"){
      output$plot_simmodel =  renderPlot(
        plot(X$plotpwid_num)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    if (input$plotoutB=="pwid_num_long"){
      output$plot_simmodel =  renderPlot(
        plot(X$plotpwid_num_long)
      )
      ns =session$ns
      output$ui =renderUI(plotOutput(ns("plot_simmodel"))  )
    }
    updateSelectizeInput(session, "plotoutA", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "plotoutA")
  })
  observeEvent(input$icer,{
    ns =session$ns
    shinyjs::show(id="div_plot_flex",asis=TRUE)
    shinyjs::hide(id="div_plotsimmodel",asis=TRUE)
    footnote="Prevention costs not included"
    trtmessage = "DAA treatment with prevention"
    captiony=paste0("ICER (Scenario #",scenario_number$scenario,") as $ per QALY gained\n")
    if (input$fixed_coverage=="Yes"){
      footnote=""
      trtmessage = "DAA treatment"
    }
    trtmessage = paste0(trtmessage,". ",captiony)
    output[["plot_flex"]] <- renderDT({
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
                  footnote
                )
      ) %>% formatStyle('ICER($)',color="red") %>% formatStyle(1:3,`text-align` = 'right')

    }, server = FALSE)
    
    output$plot_simtext2 = renderUI({HTML(trtmessage)})
    #output$plot_flex <- renderUI({Ftab %>% htmltools_value()})
  })
  
  
  observeEvent(input$reset_input2,{
    reset("early_current")
    reset("advan_current")
    reset("diag_time")
    reset("fixed_coverage")
    reset("NSP_cov")
    reset("OST_cov")
    reset("NSP_OST_cov")
    reset("DAA_duration")
    reset("includegenpop")
    reset("pwid_pop_growth")
    shinyjs::show(id="NSP_OSTtag",asis=TRUE)
    updateSelectizeInput(session, "plotoutA", selected = character(0))
    updateSelectizeInput(session, "plotoutB", selected = character(0))
    session$sendCustomMessage(type = "resetValue", message = "plotoutA")
    session$sendCustomMessage(type = "resetValue", message = "plotoutB")
    output$plot_simmodel =  renderPlot(NULL)
    output$plot_simtext2 = renderUI({HTML("Select required interventions and click run simulation")})
    shinyjs::hide(id="div_plotoutA",asis=TRUE)
    shinyjs::hide(id="div_flexoutA",asis=TRUE)
    shinyjs::hide(id="div_plotoutB",asis=TRUE)
    shinyjs::hide(id="div_but_genpop",asis=TRUE)
    shinyjs::hide(id="div_plot_flex",asis=TRUE)
    shinyjs::hide(id="div_plotsimmodel",asis=TRUE)
    output$plot_flex=renderUI({NULL})
    #shinyjs::hide(id="plotof")
    #output$plot_simmodel =  renderPlot(NULL)
  })  
  
  observeEvent(input$int_detail2, {
    ns =session$ns
    
    showModal(modalDialog(
      size="l",
      HTML('<img src="treat.PNG">')))
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
  
  observeEvent(input$faqs, {
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
  
  output$plot_simtext2 = renderUI({HTML("Select required interventions and click run simulation")})
  
  
  
  
}
