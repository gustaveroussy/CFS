##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["IC_gene_heatmap_slider_main_parameters_UI"]] <- renderUI({
  req(values$data)
  mm = min_max_gene_heatmap()
  if(mm$min != mm$max){
    tagList(
      sliderInput("slider_IC_gene_heatmap_range", label = "Color range", min = mm$min, 
                  max = mm$max, value = c(mm$min, mm$max)
      )
    )
  }
})

output[["IC_gene_heatmap_number_main_parameters_UI"]] <- renderUI({
  tagList(
    numericInput("select_number_IC_gene_heatmap", label = "Number of genes to display", value = 50, min = 2, max = NA, step = 1)
  )
})

output[["IC_gene_heatmap_color_main_parameters_UI"]] <- renderUI({
  tagList(
    selectInput("select_color_IC_gene_heatmap", label = "Select color", 
                choices = list("Viridis", "Blues", "Reds","YlGnBu","YlOrRd"), 
                selected = "Viridis")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_gene_heatmap_main_parameters_info"]], {
  showModal(
    modalDialog(
      IC_gene_heatmap_main_parameters_info[["text"]],
      title = IC_gene_heatmap_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_gene_heatmap_main_parameters_info <- list(
  title = "Main parameters for projection",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Color range:</b> Select the range of color to use.</li>
      <li><b>Number of genes to display:</b> Select the number of top genes from the IC to display.</li>
      <li><b>Select color:</b> Select here which color scale you want to use in the spatial plot on the right.</li>
    </ul>
    "
  )
)