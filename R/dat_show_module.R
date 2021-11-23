

dat_show_module <- function(input, output, session,  modal_title,file_to_show_row,modal_trigger) {
  ns <- session$ns
  #addResourcePath("picy", "~/HCV_vF/www")
  addResourcePath("picy", paste0(getwd(),"/www"))
  observeEvent(modal_trigger(), {
    #print("here mody")
    hold <- file_to_show_row()
   
    print(hold$Ref)
    showModal(
      modalDialog(size="l",
        fluidRow( useShinyjs(),
      column(12,
             tags$iframe(src=paste0("picy/",hold$Ref,".pdf"),width = "100%", height = "700px",scrolling=TRUE,id = 'myIframe'),
      ))))
 
  })
  
  
}
