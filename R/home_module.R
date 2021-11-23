home_module_ui <-function(id){
  ns = NS(id)

  ui=fluidPage(tags$head(tags$style(
    HTML('#title {
           color: red;
           font-size: 40px;
           font-style: bold;
          }'))),
    
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title=div(img(src="Medicus-Economics-lg.png"), "HCV elimination model",style = "color: #FF0000"), windowTitle="HCV elimiation model"
        )
    ),
    

                        jumbotron("",h4("The aim of the HCV elimination model is to allow users to investigate the relative value of different routes towards the 
                                  WHO Hepatitis C incidence and mortality elimination targets. Based on a previously published and validated disease transmission model that has been cited over 150 times in the peer-reviewed 
                                  literature [", actionLink(inputId = ns("link_scott"), label = "Scott"),"]. The model calls upon the most recent literature on trends in both HCV prevalence across multiple strata 
                                  of society and risky behaviors known to drive Hep C infection such as injection drug use, and estimates time to elimination 
                                  based on treatment reach and access and take-up of treatment options in a combined population of a general, PWID and prison sub-populations.
                                  It also can be used to estimate the combinations of these inputs for a particular elimination goal."),
                                  button=TRUE,buttonLabel="Model"),
                        fluidRow(
                          column(8, panel_div(class_type = "primary", panel_title = "Directions",
                          content = h5("For combined gen pop/PWID/prison populations: ",actionLink(inputId = ns("link_threetab"), label = "Three sub-populations model"),
                                       HTML(strrep(br(), 1)),
                                       "For DAA and NSP/OST intervention: ",actionLink(inputId = ns("link_pwidtab"), label = "Intervention PWID")," and ",actionLink(inputId = ns("link_gentab"), label = "Intervention gen pop"),
                                       HTML(strrep(br(), 1)),
                                       "For precomputed scenario outputs:",actionLink(inputId = ns("link_autotab"),label = "Automated output"),
                                       HTML(strrep(br(), 1)),
                                       "For model parameters and refernces:",actionLink(inputId = ns("link_paramtab"),label = "Model parameters"),
                                       HTML(strrep(br(), 1)),
                                       "For model fitting and validation:",actionLink(inputId = ns("link_valtab"),label = "Epidemic data"),
                                       HTML(strrep(br(), 1)),
                                       "For baseline models:",actionLink(inputId = ns("link_basetab"),label = "Fit model")
                                       ))),
                          column(4, panel_div("success", "Application Contact",content = "warren.stevens@medicuseconomics.com"))
                  
                        ),  
                        fluidRow(
                          column(6, panel_div("info", "Version Control", "Version 2, November 11, 2021")),
                          column(6, panel_div("danger", "Security and License", "Copyright 2021")),
                           bsModal("modalExample", "", "tabBut", size = "large" ,
                                   #p("Additional text and widgets can be added in these modal boxes. Video plays in chrome browser"),
                                   img(width = "600", height = "650", src = "Compartment.PNG")
                           )
                        )
)


  
  
 
}

home_module <- function(input, output, session,parent_session) {
  observeEvent(input$link_scott, {
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","Scott.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  
  observeEvent(input$link_threetab, {
    updateTabsetPanel(parent_session, "taby",selected = "All pop interventions")
  })
  
  observeEvent(input$link_pwidtab, {
    #print("here")
    updateTabsetPanel(parent_session, "taby",selected = "Intervention PWID")
  })
  
  observeEvent(input$link_gentab, {
    #print("here")
    updateTabsetPanel(parent_session, "taby",selected = "Intervention gen pop")
  })
  
  observeEvent(input$link_paramtab, {
    #print("here")
    updateTabsetPanel(parent_session, "taby",selected = "Model parameters")
  })
  
  observeEvent(input$link_valtab, {
    #print("here")
    updateTabsetPanel(parent_session, "taby",selected = "Epidemic data")
  })
  
  observeEvent(input$link_basetab, {
    #print("here")
    updateTabsetPanel(parent_session, "taby",selected = "Fit model")
  })
  
  observeEvent(input$link_autotab, {
    #print("here")
    updateTabsetPanel(parent_session, "taby",selected = "Automated output")
  })
  
  
}
