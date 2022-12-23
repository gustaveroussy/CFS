##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["spot_gene_heatmap_slider_main_parameters_UI"]] <- renderUI({
  tagList(
    sliderInput("slider_spot_gene_heatmap_range", label = "Color range", min = round(min(Launch_analysis()@misc[[input$IC_choice]]$spot_top_genes_weight), digits = 0), 
                max = round(max(Launch_analysis()@misc[[input$IC_choice]]$spot_top_genes_weight), digits = 0), value = c(round(min(Launch_analysis()@misc[[input$IC_choice]]$spot_top_genes_weight), digits = 0), round(max(Launch_analysis()@misc[[input$IC_choice]]$spot_top_genes_weight), digits = 0))
    )
  )
})

output[["spot_gene_heatmap_color_main_parameters_UI"]] <- renderUI({
  tagList(
    selectInput("select_color_spot_gene_heatmap", label = "Select color", 
                choices = list("Viridis", "Blues", "Reds","YlGnBu","YlOrRd"), 
                selected = "Viridis")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["spot_gene_heatmap_main_parameters_info"]], {
  showModal(
    modalDialog(
      spot_gene_heatmap_main_parameters_info[["text"]],
      title = spot_gene_heatmap_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
spot_gene_heatmap_main_parameters_info <- list(
  title = "Main parameters for projection",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Color range:</b> Select the range of color to use.</li>
      <li><b>Select color:</b> Select here which color scale you want to use in the spatial plot on the right.</li>
    </ul>
    "
  )
)