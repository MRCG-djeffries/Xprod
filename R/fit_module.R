fit_module_ui <-function(id){
  ns = NS(id)
  
  ui <- fluidPage(
    tags$h5(id="head4","1. Total population"),
    tags$style(HTML("#head4{color: red;}")),
    tags$ul(
      div(style="display: inline-block;vertical-align:top;",tags$li("In 2016, 3.5 million people were chronically infected with HCV")), 
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("eldin_ref_pop"), label="Ref-Edlin",size="xs")),
     ),
    tags$h5(id="head4","2. Current PWID"),
    tags$style(HTML("#head4{color: red;}")),
    tags$ul(
      div(style="display: inline-block;vertical-align:top;",tags$li("(38.1% to 68%) of current PWID are infected with HCV in 2016")), 
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("degenhardt_ref_pop"), label="Ref-Degenhardt",size="xs")),
      ),
    tags$h5(id="headscale","3. Scaling to a population (assuming a growing PWID population) "),
    tags$style(HTML("#headscale{color: red;}")),
    tags$ul(
      div(style="display: inline-block;vertical-align:top;",tags$li("In 2007 there were 2,248,500 PWID (current)")),
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("degenhardt_ref_pop1"), label="Ref-Degenhardt",size="xs")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("Allow PWID (current) population to grow with two model fitting parameters")),
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("fraser_ref_pop"), label="Ref-Fraser",size="xs")),
         ),
    tags$h5(id="headscale","4. Fit multiplier parameter for force of infection to model increasing incidence of HCV"),
    tags$style(HTML("#headscale{color: red;}")),
    tags$ul(
      tags$li("Flat prior to 2009"),
      div(style="display: inline-block;vertical-align:top;",tags$li("3 to 4 fold increase in incidence between 2010 and 2018")),
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("cdc_ref_pop"), label="Ref-CDC",size="xs")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("Assume increasing incidence and flattening in 2030 and steady incidence until 2040")),
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("fraser_ref_pop1"), label="Ref-Fraser",size="xs")),
    ),
    tags$h5(id="head1","5. Fit epidemic model parameters for best fit to 2 to 4. Allow the following model parameters to vary"),
    tags$style(HTML("#head1{color: red;}")),
    tags$ul(
    div(style="display: inline-block;vertical-align:top;",tags$li("Calibration multiplier for force of infection")),
    div(style="display: inline-block;vertical-align:top;",actionBttn(ns("scott_ref_pop"), label="Ref-Scott",size="xs")),
    br(),
    div(style="display: inline-block;vertical-align:top;",tags$li("Relapse (former to current) rate and Injecting career length (current to former)")),
    ),
    tags$h5(id="head6","6. Split of HCV case for PWID (current and former) and general population"),
    tags$style(HTML("#head6{color: red;}")),
    tags$ul(
      div(style="display: inline-block;vertical-align:top;",tags$li("Fit scenarios around a 60%/20% split")), 
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("tatar_ref"), label="Ref-Tatar",size="xs")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("Adjust the split such that current PWID account for (74%  to 82%) of incident cases")), 
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("stone_ref"), label="Ref-Stone",size="xs")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("MSM and prison populations are not included in this model")), 
    ),
    tags$h5(id="head7","7. Number of former PWID"),
    tags$style(HTML("#head7{color: red;}")),
    tags$ul(
      tags$li("Former PWID population with HCV  = PWID% (point 6) * 3.5million - fitted pop in 2016 (point 3) * fitted prevalence in point 2"), 
    ),
    tags$h5(id="head8","8. Baby boomer effect (contaminated blood) and endemic trend for general population"),
    tags$style(HTML("#head8{color: red;}")),
    tags$ul(
      div(style="display: inline-block;vertical-align:top;",tags$li("Subjects born in 1945 to 1964 infected from 1970 to 1990")), 
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("mmwr_ref"), label="Ref-MMWR",size="xs")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("Subjects in these age groups have 5 times HCV prevalence compared to other general population age groups")), 
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("alavi_ref"), label="Ref-Alavi",size="xs")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("Chronic HCV exposure reduces life expectancy on average by 5 years")), 
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("arias_ref"), label="Ref-Arias",size="xs")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("Converting life expectancy reduction to mortality rates")), 
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("doc_ref"), label="Life Expectancy",size="xs")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("Assume that in the general population HCV is endemic, but not an infectious disease")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("So infection is for example from long term blood dialysis, needle sticks, sharps, or mucosal exposures rather than person-to-person exposure (in PWID)")),
    ),
    tags$h5(id="head9","9. Fitting prison population"),
    tags$style(HTML("#head9{color: red;}")),
    tags$ul(
      div(style="display: inline-block;vertical-align:top;",tags$li("Extract the PWID (format and current) population for 2015")), 
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("Scale the population such that % of incarcerated with HCV who are PWID is 50% with the remainder in the former population")), 
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("dolan_ref"), label="Ref-Dolan",size="xs")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("Assume 20% (this has to be consistent point 6(i) above) of 3,500,000 HCV positive subjects in 2015 are incarcerated")), 
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("Scale the starting prison population such that the total population is 2,173,800 in 2015")), 
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("doc_cpus"), label="US gov prison data",size="xs")),
      br(),
      div(style="display: inline-block;vertical-align:top;",tags$li("This gives an incarcerated HCV population percentage of 32%, consistent with")),
      div(style="display: inline-block;vertical-align:top;",actionBttn(ns("doc_cdc"), label="CDC report",size="xs")),    ),
    br(),
    actionBttn(ns("validation_doc"), label="Validation of fits",size="xs")
    
  )
}

fit_module <- function(input, output, session) {

  output$plot_incidence<-renderPlot({
    plot_inci_trendUS_v2()
})

  observeEvent(input$dolan_ref, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","Dolan.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  observeEvent(input$doc_cpus, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","Cpus.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  observeEvent(input$doc_cdc, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","cdc.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })

  observeEvent(input$doc_ref, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","age_calcs.png"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  
  observeEvent(input$arias_ref, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","arias.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  
  observeEvent(input$alavi_ref, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","alavi.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  
  observeEvent(input$mmwr_ref, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","mmwr.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  
  observeEvent(input$stone_ref, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","stone.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })

  observeEvent(input$tatar_ref, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","tatar.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
  })
  
  observeEvent(input$eldin_ref_pop, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","Edlin.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
})
  
  observeEvent(input$degenhardt_ref_pop, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","degenhardt.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
    
  })
  
  observeEvent(input$degenhardt_ref_pop1, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","degenhardt.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
    
  })
  
  observeEvent(input$fraser_ref_pop, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","fraser.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
    
  })
  observeEvent(input$fraser_ref_pop1, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","fraser.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
    
  })
  
  observeEvent(input$cdc_ref_pop, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","HepSurveillanceRpt_2018.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
    
  })
  observeEvent(input$cdc_ref_pop1, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","HepSurveillanceRpt_2018.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
    
  })
  
  observeEvent(input$scott_ref_pop, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","Scott.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
    
  })
  
  observeEvent(input$validation_doc, {
    ns =session$ns
    #addResourcePath("picy", "~/HCV/www")
    addResourcePath("picy", paste0(getwd(),"/www"))
    showModal(
      modalDialog(size="l",
                  fluidRow( useShinyjs(),
                            column(12,
                                   tags$iframe(src=paste0("picy/","validation_v2.pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
                            ))))
    
  })
  
 
}
