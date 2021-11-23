parameters_module_ui <- function(id) {
  ns <- NS(id)

  # tagList(
  dashboardPage(title="HCV model",
    dashboardHeader(title = "choose parameter set",titleWidth = 320),
    dashboardSidebar(width = 320, shinyjs::useShinyjs(),
                     selectInput(ns("category"), "Category:",
                                 c("Transition" = "Transition",
                                   "Liver related mortality" = "Liver related mortality",
                                   "Non liver related mortality" = "Non liver related mortality",
                                   "Drug use"="Drug use",
                                   "Annual costs"="Annual costs",
                                   "Health utilities"="Health utilities",
                                   "DAA data"="DAA data",
                                   "NSP-OST"="NSP-OST",
                                   "Post SVR"="Post SVR",
                                   "Prison pop"="Prison pop",
                                   "Pop flow"="Pop flow",
                                   "Prison mortality"="Prison mortality",
                                   "Prison infection"="Prison infection",
                                   "Prison intervention"="Prison intervention"
                                   ),
                                 multiple = TRUE,size=14,selectize=FALSE),
                     actionBttn(ns("reset_cat"), label="Reset",size="xs"),
                     tags$hr()
                   
    ),
    dashboardBody(shinyjs::useShinyjs(),tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '
    ))), 
    fluidRow(
      column(
        width = 12,
        title = "Parameters in model",
        DTOutput(ns("param_table")) %>%
          withSpinner(),
        tags$br(),
        tags$br()
      )
    ),
    tags$script(src = "dat_table_module.js"),
    tags$script(paste0("dat_table_module_js('", ns(''), "')"))
   )
  )
 
}

parameters_module <- function(input, output, session) {

 
  dats <- reactive({
    session$userData$db_trigger()

    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('parameter_data') %>%
        collect()  %>%
         arrange(Vnum)

    }, error = function(err) {

      print(err)
      showToast("error", "Database Connection Error")

    })
    #format(out$value, big.mark=',', scientific=FALSE) 
    
    out

  })
  
  dat_table_prep <- reactiveVal(NULL)
  observeEvent(dats(), {
    out <- dats()
    
    ids <- out$Vnum #out$id_
    
    actions <- purrr::map_chr(ids, function(id_) {
      paste0('<div class="btn-group" >
          <button class="btn btn-default btn-sm show_btn" data-toggle="tooltip" title="Show ref" id = ', id_, ' style="margin: 0"><i class="fa fa fa-eye"></i></button>
        </div>'
      )
    })
    
    # Set the Action Buttons row to the first column of the `mtcars` table
    out <- cbind(
      tibble(" " = actions),
      out
    )
    if (is.null(dat_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      dat_table_prep(out)

    } else {

      # manually hide the tooltip from the row so that it doesn't get stuck
      # when the row is deleted
      shinyjs::runjs("$('.btn-sm').tooltip('hide')")
      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(car_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
      
    }
      
  })
  
  car_table_proxy <- DT::dataTableProxy('param_table')
  

  observeEvent(input$reset_cat, {
    reset("category")
  })
 
  output$param_table <- renderDT({
    if (length(input$category)>0){
      #out=dats()%>% filter(Variable %in% input$category)
      # for ( i in 1 : length(input$category)){
      #     out =out %>% filter(Variable == input$category[i])
      # }
      req(dat_table_prep())
      out <- dat_table_prep()%>% filter(Variable %in% input$category)
    headerCallback = JS(
      "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').eq(1).css('color', 'white');
      $(thead).closest('thead').find('th').eq(2).css('color', 'white');
      $(thead).closest('thead').find('th').eq(3).css('color', 'red');
      $(thead).closest('thead').find('th').eq(5).css('color', 'white');
      $(thead).closest('thead').find('th').eq(6).css('color', 'red');
              }"
    )
    datatable(
      out,
      rownames = FALSE,
       colnames = c('Vnum', 'Variable', 'Stratum', 'Parameter', 'Units', 'Type', 'Value', 'Ref'),
      selection = "none",
      class = "compact stripe row-border nowrap",
     
      escape = -1,
      extensions = c("Buttons"),
      options = list(
        scrollX = TRUE,
        dom = 'Blftip',
        pageLength = 15,
        buttons = list(
          list(
            extend = "excel",
            text = "Download",
            title = paste0("parameters-", Sys.Date()),
            exportOptions = list(
              columns = 1:(length(out) - 1)
            )
          )
        ),
        columnDefs = list(
          list(targets = c(0,1,3,4,5,6), orderable = FALSE),
          list(className = 'dt-left', targets = 0:4),
          list(className = 'dt-center', targets = 6)
        ),
        headerCallback = JS(headerCallback),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff',' text-align': 'left;'});",
          "}")
      )
    )%>%
      formatStyle('Vnum', `text-align` = 'left')%>%
      formatStyle('Value', `text-align` = 'right',color = 'red',fontWeight = 'bold')%>%formatCurrency('Value', '')%>%
      formatStyle('Parameter', `text-align` = 'left',color = 'red',fontWeight = 'bold')
    }
    
  })

  file_to_show_row  <- eventReactive(input$dat_id_to_show, {
    #print(paste0("here ",input$dat_id_to_show))
    dats() %>% filter(Vnum== input$dat_id_to_show) %>% as.list()
    #print(paste0("here ",dum$Ref))
  })

  callModule(
    dat_show_module,
    "show_inv",
    modal_title = "Show ref",
    file_to_show_row=file_to_show_row,
    modal_trigger = reactive({input$dat_id_to_show})
  )
  




}
