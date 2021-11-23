library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinyFeedback)
library(ggplot2)
library(RSQLite)
library(shinyWidgets)
library(dbplyr)
library(shinydashboard)
library(shinyalert)
library(cowplot)
library(shinybusy)
library(scales)
library(purrr)
library(shinyBS)
library(shinyLP)
library(deSolve)
#library(TurboNorm)
library(yaml)
#library(ggpubr)
ui <- fluidPage( add_busy_bar(color = "#FF0000"),
  useShinyalert(),  
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                  Shiny.onInputChange(variableName, null);
                  });
                  "),
  # Application Title
  tabsetPanel(id="taby",type = "tabs",
  tabPanel("Home",home_module_ui("home_page")),
  tabPanel("Intervention gen pop",genpop_intervention_module_ui("genpop_model")),
  tabPanel("Intervention PWID",all_intervention_module_ui("sim_model")),
  tabPanel("Automated output",automated_module_ui("out_data")),
  tabPanel("Model parameters",parameters_module_ui("param_table")),
  tabPanel("Epidemic data",fit_module_ui("plot_incidence")),
  tabPanel("Fit model",fit_to_data_module_ui("plot_model")),
  tabPanel("All pop interventions",THREE_all_intervention_module_ui("sim3_model")),
  tabPanel("Intervention data",intervention_panel_module_ui("int3_model"),value=2)

)
)
      
