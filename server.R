server <- function(input, output, session) {

  # user session$userData to store user data that will be needed throughout
  # the Shiny application
  #session$userData$email <- 'tycho.brahe@tychobra.com'
  session$userData$db_trigger <- reactiveVal(0)
  callModule(home_module,id="home_page",parent_session=session) # note have to pass this session to get back for changing tabs
  callModule(parameters_module,id="param_table")
  callModule(fit_module,id="plot_incidence")
  scenario_number = callModule(fit_to_data_module,id="plot_model")
  genpop_input=callModule(genpop_intervention_module,id="genpop_model")
  callModule(all_intervention_module,id="sim_model",scenario_number,genpop_input)
  callModule(automated_module,id="out_data")
  rv=callModule(THREE_all_intervention_module,id="sim3_model",scenario_number,parent_session=session)
  callModule(intervention_panel_module,id="int3_model",parent_session=session)
  observe({
    if (rv$pushed==0){
      hideTab(inputId = "taby",target="2")
    }else{
      showTab(inputId = "taby",target="2")
    }
  })
  daty <- reactive ({NULL})


}
