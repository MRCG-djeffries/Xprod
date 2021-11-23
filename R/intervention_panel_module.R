
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

intervention_panel_module_ui<-function(id) {
 
  ns <- NS(id)

  bootstrapPage( 
    "", 
  navbarPageWithInputs( tags$style(
    ".navbar-nav li a {
        font-size: 20px;
        font-weight: bold;
      }
    "), 
    id = ns("available_tabs"),
    title=span("Enter the intervention parameters",style = "color: red" ),
             selected="tab_a",
      tabPanel(
        title = "PWID",
        value = "tab_a",
        tags$div(id="mydf",fluidPage(
          tags$style('#mydf .container-fluid {
                          background-color: #292b2c;color: #f7f7f7;
           }'),  
          div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("int_detail2_three_PWID"), label="PWID intervention details",size="xs")),
          div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("reset_gen_three_PWID"), label="Reset",size="xs")),
          div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("back_to_model_three_PWID"), label="Back to model",size="xs")),
          #textOutput(ns('scenario_text_three_PWID')),
        tags$div(style = "height:10px;",id = "inliney",radioButtons(ns("DAA_duration_three_PWID"), label=div(style="margin-right: 10px;","DAA"),inline=TRUE,selected = "weeks12",c("8 weeks" = "weeks8","12 weeks" = "weeks12"))),
        tags$br(),
        div(style = "height:0px;",tags$hr()),
        div(style="display: inline-block;vertical-align:top;margin-top: 10px;",tags$h5("Reach per 1,000 PWID subjects")),
        div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("reach_butt_three_PWID"), label="Reach details",size="xs")),
        tags$br(),
        hidden(div(id="former_F0_three_PWID",style="display: inline-block;vertical-align:top;",div(style = "height:100px;",
                                                                                                     sliderInput(ns("early_former_three_PWID"), div(style = "font-size:10px","former F0,F1,F2,F3"),
                                                                                                                 min = 0, max = 100,
                                                                                                                 value = 0, step = 1)))),
        hidden(div(id="former_F4_three_PWID",style="display: inline-block;vertical-align:top; ",div(style = "height:100px;",
                                                                                                      sliderInput(ns("advan_former_three_PWID"), div(style = "font-size:10px","former F4,HCC,DC,LT"),
                                                                                                                  min = 0, max = 100,
                                                                                                                  value = 0, step = 1)))),
        tags$br(),
        div(style="display: inline-block;vertical-align:top;",div(style = "height:100px;",(sliderInput(ns("early_current_three_PWID"), div(style = "font-size:10px","current F0,F1,F2,F3"),
                                                                                                                   min = 0, max = 100,
                                                                                                                   value = 0, step = 1)))),
        div(style="display: inline-block;vertical-align:top;",div(style = "height:100px;",(sliderInput(ns("advan_current_three_PWID"), div(style = "font-size:10px","current F4,HCC,DC,LT"),
                                                                                                                   min = 0, max = 100,
                                                                                                                   value = 0, step = 1)))),
        div(style = "height:0px;",tags$hr()),
        div(style="display: inline-block;vertical-align:top;margin-top: 10px;",tags$h5(id="NSP_OSTtag_three_PWID","NSP and OST interventions:")),
        div(id="div_prev_butt_three_PWID",style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("prev_butt_three_PWID"), label="Prevention",size="xs")),
        tags$br(),
        div(id="div_NSP_cov_three_PWID",style="display: inline-block;vertical-align:top;",(sliderInput(ns("NSP_cov_three_PWID"), div(style = "font-size:10px","NSP coverage(%)"),
                                                                                                         min = 0, max = 50,
                                                                                                         value = 0, step = 1))),
        div(id="div_OST_cov_three_PWID",style="display: inline-block;vertical-align:top;",(sliderInput(ns("OST_cov_three_PWID"), div(style = "font-size:10px","OST coverage(%)"),
                                                                                                         min = 0, max = 50,
                                                                                                         value = 0, step = 1))),
        tags$br(),
        div(id="div_NSP_OST_cov_three_PWID",style="display: inline-block;vertical-align:top;",(sliderInput(ns("NSP_OST_cov_three_PWID"), div(style = "font-size:10px","NSP & OST coverage(%)"),
                                                                                                             min = 0, max = 50,
                                                                                                             value = 0, step = 1))),
        div(id="div_diag_time_three_PWID",style="display: inline-block;vertical-align:top;",(sliderInput(ns("diag_time_three_PWID"), div(style = "font-size:10px","diagnosis(months)"),
                                                                                                           min = 1, max = 120,
                                                                                                           value = 6, step = 1))),
        tags$div(style = "height:10px;",id = "inliney",radioButtons(ns("fixed_coverage_three_PWID"), label=div(style="margin-right: 10px;","DAA treat only"),inline=TRUE,selected = "No",c("Yes" = "Yes","No" = "No"))),
        tags$br(),
        div(style = "height:0px;",tags$hr())
        ))
      ),
      # returns are (10)
      # "DAA_duration_three_PWID"
      # "early_former_three_PWID","advan_former_three_PWID","early_current_three_PWID","advan_current_three_PWID"
      # "NSP_cov_three_PWID","OST_cov_three_PWID","NSP_OST_cov_three_PWID","diag_time_three_PWID"
      #"fixed_coverage_three_pwid_PWID"
    
      tabPanel(
        title = "Prison",
        value = "tab_b",
        tags$div(id="mydf",fluidPage(
          tags$style('#mydf .container-fluid {
                          background-color: #292b2c;color: #f7f7f7;
           }'),  

          div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("int_detail2_three_PRIS"), label="Prison intervention details",size="xs")),
          div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("reset_gen_three_PRIS"), label="Reset",size="xs")),
          div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("back_to_model_three_PRIS"), label="Back to model",size="xs")),
  
           #textOutput(ns('scenario_text_three_PRIS')),
          tags$div(style = "height:10px;",id = "inliney",radioButtons(ns("DAA_duration_three_PRIS"), label=div(style="margin-right: 10px;","DAA"),inline=TRUE,selected = "weeks12",c("8 weeks" = "weeks8","12 weeks" = "weeks12"))),
          tags$br(),
          div(style = "height:0px;",tags$hr()),
          div(style="display: inline-block;vertical-align:top;margin-top: 10px;",tags$h5("Reach per 1,000 PWID subjects")),
          div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("reach_butt_three_PRIS"), label="Reach details",size="xs")),
          tags$br(),
          hidden(div(id="former_F0_three_PRIS",style="display: inline-block;vertical-align:top;",div(style = "height:100px;",
                                                                                                     sliderInput(ns("early_former_three_PRIS"), div(style = "font-size:10px","former F0,F1,F2,F3"),
                                                                                                                 min = 0, max = 100,
                                                                                                                 value = 0, step = 1)))),
          hidden(div(id="former_F4_three_PRIS",style="display: inline-block;vertical-align:top; ",div(style = "height:100px;",
                                                                                                      sliderInput(ns("advan_former_three_PRIS"), div(style = "font-size:10px","former F4,HCC,DC,LT"),
                                                                                                                  min = 0, max = 100,
                                                                                                                  value = 0, step = 1)))),
          tags$br(),
          div(style="display: inline-block;vertical-align:top;",div(style = "height:100px;",(sliderInput(ns("early_current_three_PRIS"), div(style = "font-size:10px","current F0,F1,F2,F3"),
                                                                                                         min = 0, max = 100,
                                                                                                         value = 0, step = 1)))),
          div(style="display: inline-block;vertical-align:top;",div(style = "height:100px;",(sliderInput(ns("advan_current_three_PRIS"), div(style = "font-size:10px","current F4,HCC,DC,LT"),
                                                                                                         min = 0, max = 100,
                                                                                                         value = 0, step = 1)))),
          div(style = "height:0px;",tags$hr()),
          div(style="display: inline-block;vertical-align:top;margin-top: 10px;",tags$h5(id="NSP_OSTtag_three_PRIS","NSP and OST interventions:")),
          div(id="div_prev_butt_three_PRIS",style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("prev_butt_three_PRIS"), label="Prevention",size="xs")),
          tags$br(),
          div(id="div_NSP_cov_three_PRIS",style="display: inline-block;vertical-align:top;",(sliderInput(ns("NSP_cov_three_PRIS"), div(style = "font-size:10px","NSP coverage(%)"),
                                                                                                         min = 0, max = 50,
                                                                                                         value = 0, step = 1))),
          div(id="div_OST_cov_three_PRIS",style="display: inline-block;vertical-align:top;",(sliderInput(ns("OST_cov_three_PRIS"), div(style = "font-size:10px","OST coverage(%)"),
                                                                                                         min = 0, max = 50,
                                                                                                         value = 0, step = 1))),
          tags$br(),
          div(id="div_NSP_OST_cov_three_PRIS",style="display: inline-block;vertical-align:top;",(sliderInput(ns("NSP_OST_cov_three_PRIS"), div(style = "font-size:10px","NSP & OST coverage(%)"),
                                                                                                             min = 0, max = 50,
                                                                                                             value = 0, step = 1))),
          div(id="div_diag_time_three_PRIS",style="display: inline-block;vertical-align:top;",(sliderInput(ns("diag_time_three_PRIS"), div(style = "font-size:10px","diagnosis(months)"),
                                                                                                           min = 1, max = 120,
                                                                                                           value = 6, step = 1))),
          tags$div(style = "height:10px;",id = "inliney",radioButtons(ns("fixed_coverage_three_PRIS"), label=div(style="margin-right: 10px;","DAA treat only"),inline=TRUE,selected = "No",c("Yes" = "Yes","No" = "No"))),
          tags$br(),
          div(style = "height:0px;",tags$hr())
        ))
      ),
      tabPanel(
        title = "General population",
        value = "tab_c",
        tags$div(id="mydf",fluidPage(
          
     #      tags$style(HTML("
     # #mydf body {
     #    background-color: black;
     #    color: white;
     #  }
     #  #mydf .shiny-input-container {
     #    color: #474747;
     #  }")),
 # https://stackoverflow.com/questions/46830173/tagsstyle-specific-modaldialog-element-in-shiny
          # bootstrap 4 colors
          #https://colorswall.com/palette/3/
      tags$style('#mydf .container-fluid {
                          background-color: #292b2c;color: #f7f7f7;
           }'),         
          
          # tags$style('#mydf .container-fluid {
          #                    background-color: #007BA7;
          #     }'),
        div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("int_detail2_three_GP"), label="Gen pop intervention details",size="xs")),
        div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("reset_gen_three_GP"), label="Reset",size="xs")),
        div(style="display: inline-block;vertical-align:top;margin-top: 10px;",actionBttn(ns("back_to_model_three_GP"), label="Back to model",size="xs")),
      
        tags$div(style = "height:10px;",id = "inliney",radioButtons(ns("DAA_duration_gen_three_GP"), label=div(style="margin-right: 10px;","DAA"),inline=TRUE,selected = "weeks12",c("8 weeks" = "weeks8","12 weeks" = "weeks12"))),
        tags$br(),
        div(style = "height:0px;",tags$hr()),
        tags$h5("Reach per 1,000 subjects"),
      
      #width: 255px;
        (div(style="display: inline-block;vertical-align:top;",div(style = "height:100px;",
                                                                                sliderInput(ns("early_gen_three_GP"), div(style = "font-size:10px","F0,F1,F2"),
                                                                                            min = 0, max = 100,
                                                                                            value = 0, step = 1)))),
        (div(style="display: inline-block;vertical-align:top; ",div(style = "height:100px;",
                                                                                 sliderInput(ns("advan_gen_three_GP"), div(style = "font-size:10px","F3,F4"),
                                                                                             min = 0, max = 100,
                                                                                             value = 0, step = 1)))),
        tags$br(),
        tags$hr(),
        tags$h5(id="screentag_three_GP","Screening:"),
        div(id="div_screening_cov_three_GP",style="display: inline-block;vertical-align:top;",div(style = "height:100px;",(sliderInput(ns("screening_cov_three_GP"), div(style = "font-size:10px","coverage(%)"),
                                                                                                               min = 0, max = 50,
                                                                                                               value = 0, step = 1)))),
        div(id="div_diag_time_gen_three_GP",style="display: inline-block;vertical-align:top;",div(style = "height:100px;",(sliderInput(ns("diag_time_gen_three_GP"), div(style = "font-size:10px","diagnosis(months)"),
                                                                                                               min = 1, max = 120,
                                                                                                               value = 6, step = 1)))),
        tags$div(id = "inliney",radioButtons(ns("fixed_coverage_three_GP"), label=div(style="margin-right: 10px;","DAA treat only"),inline=TRUE,selected = "No",c("Yes" = "Yes","No" = "No"))),
        tags$br(),
        tags$hr()
        )
          ) 
      ) ,
 inputs = sliderInput(ns("discount_rate"), div(style = "font-size:10px","Choose discount rate(%)"),
                      min = 0, max = 5,
                      value = 3.5, step = 0.5)# put any action buttons here
      # returns are (6)
      # "DAA_duration_gen_three_GP"
      # "early_gen_three_GP","advan_gen_three_GP"
      # "screening_cov_three_GP","diag_time_gen_three_GP"
      # "fixed_coverage_three_GP"
    ))

} 

intervention_panel_module <- function(input, output, session,parent_session) {
  #print("called")
  #print(New_rv)
  observeEvent(input$int_detail2_three_GP, {

      showModal(modalDialog(
        size="l",
        HTML('<img src="genpopfit.PNG">')))

 
    #isolate(rv_pushed)
    #if (rv_pushed!=0){
    # showModal(modalDialog(
    #   size="l",
    #   HTML('<img src="genpopfit.PNG">')))
    #}
  })
  
  observeEvent(input$back_to_model_three_GP, {
    updateTabsetPanel(parent_session, "taby",selected = "All pop interventions")
  })
  observeEvent(input$back_to_model_three_PWID, {
    updateTabsetPanel(parent_session, "taby",selected = "All pop interventions")
  })
  observeEvent(input$back_to_model_three_PRIS, {
    updateTabsetPanel(parent_session, "taby",selected = "All pop interventions")
  })
 
  observeEvent(input$reset_gen_three_GP,{
    reset("early_gen_three_GP")
    reset("advan_gen_three_GP")
    reset("DAA_duration_gen_three_GP")
    reset("fixed_coverage_three_GP")
    reset("screening_cov_three_GP")
    reset("diag_time_gen_three_GP")
  })
  
  observeEvent(input$reset_gen_three_PWID,{
    obsof=c("DAA_duration_three_PWID","early_former_three_PWID","advan_former_three_PWID","early_current_three_PWID","advan_current_three_PWID",
            "NSP_cov_three_PWID","OST_cov_three_PWID","NSP_OST_cov_three_PWID","diag_time_three_PWID","fixed_coverage_three_PWID")
    for (i in 1 : 10){
      reset(obsof[i])
    }
    shinyjs::show(id="NSP_OSTtag_three_PWID",asis=TRUE)
    shinyjs::show(id="div_prev_butt_three_PWID",asis=TRUE)
  })
  
  observeEvent(input$reset_gen_three_PRIS,{
    obsof=c("DAA_duration_three_PRIS","early_former_three_PRIS","advan_former_three_PRIS","early_current_three_PRIS","advan_current_three_PRIS",
            "NSP_cov_three_PRIS","OST_cov_three_PRIS","NSP_OST_cov_three_PRIS","diag_time_three_PRIS","fixed_coverage_three_PRIS")
    for (i in 1 : 10){
      reset(obsof[i])
    }
    shinyjs::show(id="NSP_OSTtag_three_PRIS",asis=TRUE)
    shinyjs::show(id="div_prev_butt_three_PRIS",asis=TRUE)
  })
  

   observe({
   if (New_rv$x==0){ # This global fired form parent tab
     reset("early_gen_three_GP")
     reset("advan_gen_three_GP")
     reset("DAA_duration_gen_three_GP")
     reset("fixed_coverage_three_GP")
     reset("screening_cov_three_GP")
     reset("diag_time_gen_three_GP")
     reset("discount_rate")
     obsof=c("DAA_duration_three_PWID","early_former_three_PWID","advan_former_three_PWID","early_current_three_PWID","advan_current_three_PWID",
        "NSP_cov_three_PWID","OST_cov_three_PWID","NSP_OST_cov_three_PWID","diag_time_three_PWID","fixed_coverage_three_PWID")
     for (i in 1 : 10){
       reset(obsof[i])
     }
     shinyjs::show(id="NSP_OSTtag_three_PWID",asis=TRUE)
     shinyjs::show(id="div_prev_butt_three_PWID",asis=TRUE)
     obsof=gsub("PWID","PRIS",obsof)
     for (i in 1 : 10){
       reset(obsof[i])
     }
     shinyjs::show(id="NSP_OSTtag_three_PRIS",asis=TRUE)
     shinyjs::show(id="div_prev_butt_three_PRIS",asis=TRUE)
   }
   })
   
   observeEvent(input$fixed_coverage_three_GP,{
     if (input$fixed_coverage_three_GP=="Yes"){
       shinyjs::hide(id="div_diag_time_gen_three_GP",asis=TRUE)
       shinyjs::hide(id="div_screening_cov_three_GP",asis=TRUE)
       shinyjs::hide(id="screentag_three_GP",asis=TRUE)
     }else{
       shinyjs::show(id="div_diag_time_gen_three_GP",asis=TRUE)
       shinyjs::show(id="div_screening_cov_three_GP",asis=TRUE)
       shinyjs::show(id="screentag_three_GP",asis=TRUE)
     }
   })  
   
   observeEvent(input$fixed_coverage_three_PWID,{
     if (input$fixed_coverage_three_PWID=="Yes"){
       shinyjs::hide(id="div_diag_time_three_PWID",asis=TRUE)
       shinyjs::hide(id="div_NSP_OST_cov_three_PWID",asis=TRUE)
       shinyjs::hide(id="div_OST_cov_three_PWID",asis=TRUE)
       shinyjs::hide(id="div_NSP_cov_three_PWID",asis=TRUE)
       shinyjs::hide(id="NSP_OSTtag_three_PWID",asis=TRUE)
       shinyjs::hide(id="div_prev_butt_three_PWID",asis=TRUE)
       shinyjs::show(id="former_F0_three_PWID",asis=TRUE)
       shinyjs::show(id="former_F4_three_PWID",asis=TRUE)
     }else{
       shinyjs::show(id="div_diag_time_three_PWID",asis=TRUE)
       shinyjs::show(id="div_NSP_OST_cov_three_PWID",asis=TRUE)
       shinyjs::show(id="div_OST_cov_three_PWID",asis=TRUE)
       shinyjs::show(id="div_NSP_cov_three_PWID",asis=TRUE)
       shinyjs::show(id="NSP_OSTtag_three_PWID",asis=TRUE)
       shinyjs::show(id="div_prev_butt_three_PWID",asis=TRUE)       
       shinyjs::hide(id="former_F0_three_PWID",asis=TRUE)
       shinyjs::hide(id="former_F4_three_PWID",asis=TRUE)
     }
   })  
   
   observeEvent(input$fixed_coverage_three_PRIS,{
     if (input$fixed_coverage_three_PRIS=="Yes"){
       shinyjs::hide(id="div_diag_time_three_PRIS",asis=TRUE)
       shinyjs::hide(id="div_NSP_OST_cov_three_PRIS",asis=TRUE)
       shinyjs::hide(id="div_OST_cov_three_PRIS",asis=TRUE)
       shinyjs::hide(id="div_NSP_cov_three_PRIS",asis=TRUE)
       shinyjs::hide(id="NSP_OSTtag_three_PRIS",asis=TRUE)
       shinyjs::hide(id="div_prev_butt_three_PRIS",asis=TRUE)
       shinyjs::show(id="former_F0_three_PRIS",asis=TRUE)
       shinyjs::show(id="former_F4_three_PRIS",asis=TRUE)
     }else{
       shinyjs::show(id="div_diag_time_three_PRIS",asis=TRUE)
       shinyjs::show(id="div_NSP_OST_cov_three_PRIS",asis=TRUE)
       shinyjs::show(id="div_OST_cov_three_PRIS",asis=TRUE)
       shinyjs::show(id="div_NSP_cov_three_PRIS",asis=TRUE)
       shinyjs::show(id="NSP_OSTtag_three_PRIS",asis=TRUE)
       shinyjs::show(id="div_prev_butt_three_PRIS",asis=TRUE)       
       shinyjs::hide(id="former_F0_three_PRIS",asis=TRUE)
       shinyjs::hide(id="former_F4_three_PRIS",asis=TRUE)
     }
   })  
   
   observeEvent(input$reach_butt_three_GP, {
     ns =session$ns
     showModal(modalDialog(
       size="l",
       HTML('<img src="reach.PNG">')))
   })
   
   observeEvent(input$reach_butt_three_PRIS, {
     ns =session$ns
     showModal(modalDialog(
       size="l",
       HTML('<img src="reach.PNG">')))
   })
   
   observeEvent(input$reach_butt_three_PWID, {
     ns =session$ns
     showModal(modalDialog(
       size="l",
       HTML('<img src="reach.PNG">')))
   })
   
   observeEvent(input$prev_butt_three_PWID, {
     ns =session$ns
     
     showModal(modalDialog(
       size="l",
       HTML('<img src="nsp_coverage.PNG">')))
   })
   
   observeEvent(input$prev_butt_three_PRIS, {
     ns =session$ns
     
     showModal(modalDialog(
       size="l",
       HTML('<img src="nsp_coverage.PNG">')))
   })
   
  DAA_duration_gen_three_GP = reactive({input$DAA_duration_gen_three_GP})  
  early_gen_three_GP = reactive({input$early_gen_three_GP})
  advan_gen_three_GP = reactive({input$advan_gen_three_GP})
  screening_cov_three_GP = reactive({input$screening_cov_three_GP})
  diag_time_gen_three_GP = reactive({input$diag_time_gen_three_GP})
  fixed_coverage_three_GP = reactive({input$fixed_coverage_three_GP})
  
  DAA_duration_three_PWID = reactive({input$DAA_duration_three_PWID})  
  early_former_three_PWID = reactive({input$early_former_three_PWID})  
  advan_former_three_PWID = reactive({input$advan_former_three_PWID})  
  early_current_three_PWID = reactive({input$early_current_three_PWID})  
  advan_current_three_PWID = reactive({input$advan_current_three_PWID})  
  NSP_cov_three_PWID = reactive({input$NSP_cov_three_PWID})  
  OST_cov_three_PWID = reactive({input$OST_cov_three_PWID})  
  NSP_OST_cov_three_PWID = reactive({input$NSP_OST_cov_three_PWID})  
  diag_time_three_PWID = reactive({input$diag_time_three_PWID})  
  fixed_coverage_three_PWID = reactive({input$fixed_coverage_three_PWID}) 
  
  DAA_duration_three_PRIS = reactive({input$DAA_duration_three_PRIS})  
  early_former_three_PRIS = reactive({input$early_former_three_PRIS})  
  advan_former_three_PRIS = reactive({input$advan_former_three_PRIS})  
  early_current_three_PRIS = reactive({input$early_current_three_PRIS})  
  advan_current_three_PRIS = reactive({input$advan_current_three_PRIS})  
  NSP_cov_three_PRIS = reactive({input$NSP_cov_three_PRIS})  
  OST_cov_three_PRIS = reactive({input$OST_cov_three_PRIS})  
  NSP_OST_cov_three_PRIS = reactive({input$NSP_OST_cov_three_PRIS})  
  diag_time_three_PRIS = reactive({input$diag_time_three_PRIS})  
  fixed_coverage_three_PRIS = reactive({input$fixed_coverage_three_PRIS}) 
  discount_rate = reactive({input$discount_rate}) 
  # "early_former_three_PWID","advan_former_three_PWID","early_current_three_PWID","advan_current_three_PWID"
  # "NSP_cov_three_PWID","OST_cov_three_PWID","NSP_OST_cov_three_PWID","diag_time_three_PWID"
  #"fixed_coverage_three_PWID"
  
  
  
  xglob<<-mget( c("DAA_duration_gen_three_GP","early_gen_three_GP","advan_gen_three_GP","screening_cov_three_GP","diag_time_gen_three_GP","fixed_coverage_three_GP",
                  "DAA_duration_three_PWID","early_former_three_PWID","advan_former_three_PWID","early_current_three_PWID","advan_current_three_PWID",
                       "NSP_cov_three_PWID","OST_cov_three_PWID","NSP_OST_cov_three_PWID","diag_time_three_PWID",
                       "fixed_coverage_three_PWID",
                  "DAA_duration_three_PRIS","early_former_three_PRIS","advan_former_three_PRIS","early_current_three_PRIS","advan_current_three_PRIS",
                       "NSP_cov_three_PRIS","OST_cov_three_PRIS","NSP_OST_cov_three_PRIS","diag_time_three_PRIS",
                       "fixed_coverage_three_PRIS",
                       "discount_rate"))
  #print(xglob)
  #xglob<<-reactive({input$early_gen_three})
  #print(x())
  #return(reactive({input$early_gen_three}))
}

