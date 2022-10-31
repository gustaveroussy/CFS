##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["IC_gene_heatmap_slider_main_parameters_UI"]] <- renderUI({
  tagList(
    sliderInput("slider_IC_gene_heatmap_range", label = "Color range", min = round(min(Launch_analysis()@misc[[input$IC_choice]]$IC_top_genes_weight), digits = 0), 
                max = round(max(Launch_analysis()@misc[[input$IC_choice]]$IC_top_genes_weight), digits = 0), value = c(round(min(Launch_analysis()@misc[[input$IC_choice]]$IC_top_genes_weight), digits = 0), round(max(Launch_analysis()@misc[[input$IC_choice]]$IC_top_genes_weight), digits = 0))
    )
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
      <li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li>
      <li><b>Color cells by:</b> Select which variable, categorical or continuous, from the meta data should be used to color the cells.</li>
    </ul>
    "
  )
)