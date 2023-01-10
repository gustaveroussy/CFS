##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["ICA_top_IC_main_parameters_UI"]] <- renderUI({
  if(input$log_top_IC_heatmap == TRUE){res = log10(values$data@misc$top_gene_ICA)}else{res = values$data@misc$top_gene_ICA}
  res[is.nan(res)] <- 0
  tagList(
    selectInput("select_color_IC_top", label = "Select color", 
                choices = list("Viridis", "Blues", "Reds","YlGnBu","YlOrRd"), 
                selected = "Viridis"),
    sliderInput("slider_IC_top_range", label = "Color range", min = round(min(res), digits = 0), 
                max = round(max(res), digits = 0), value = c(round(min(res), digits = 0), round(max(res), digits = 0)))
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["ICA_top_IC_main_parameters_info"]], {
  showModal(
    modalDialog(
      ICA_top_IC_main_parameters_info[["text"]],
      title = ICA_top_IC_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
ICA_top_IC_main_parameters_info <- list(
  title = "Main parameters for total gene heatmap",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Select color:</b> Select here which color scale you want to use in the heatmap on the right.</li>
      <li><b>Color range:</b> Select the range of color to use.</li>
    </ul>
    "
  )
)