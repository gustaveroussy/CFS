##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["sample_based_dotplot_main_parameters_UI"]] <- renderUI({
  req(values$data)
  tagList(
    selectInput("select_color_sample_based_dotplot", label = "Select color", 
                choices = list("Magma" = "A", "Inferno" = "B", "Plasma" = "C", "Viridis" = "D", "Cividis" = "E", "Rocket" = "F", "Mako" = "G", "Turbo" = "H", "Blues", "Reds","YlGnBu","YlOrRd"), 
                selected = "D")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["sample_based_dotplot_main_parameters_info"]], {
  showModal(
    modalDialog(
      sample_based_dotplot_main_parameters_info[["text"]],
      title = sample_based_dotplot_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
sample_based_dotplot_main_parameters_info <- list(
  title = "Main parameters for sample based dotplot",
  text = HTML("
    The elements in this panel allow you to compare what and how results are displayed across the whole tab.
    <ul>
    <li><b>Select color:</b> Choose the color scale for the intensity of the signal</li>
    </ul>
    "
  )
)