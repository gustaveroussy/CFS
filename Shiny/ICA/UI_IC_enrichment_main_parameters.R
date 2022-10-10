##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["IC_enrichment_main_parameters_UI"]] <- renderUI({
  tagList(
    selectInput(
      "IC_enrichment_IC_choice",
      label = "Choose IC related to enrichment",
      choices = names(Launch_analysis()@ica)[-1]
    ),
    selectInput(
      "IC_enrichment_database_choice",
      label = "Choose database related to enrichment",
      choices = names(Launch_analysis()@ica$IC_1$en)
    )
  )
})

output[["IC_enrichment_slider_main_parameters_UI"]] <- renderUI({
  tagList(
    sliderInput("slider_IC_enrichment_range", label = "Color range", min = round(min(Launch_analysis()@ica[[input$IC_enrichment_IC_choice]]$IC_top_genes_weight), digits = 0), 
                max = round(max(Launch_analysis()@ica[[input$IC_enrichment_IC_choice]]$IC_top_genes_weight), digits = 0), value = c(round(min(Launch_analysis()@ica[[input$IC_enrichment_IC_choice]]$IC_top_genes_weight), digits = 0), round(max(Launch_analysis()@ica[[input$IC_enrichment_IC_choice]]$IC_top_genes_weight), digits = 0))
    )
  )
})

output[["IC_enrichment_color_main_parameters_UI"]] <- renderUI({
  tagList(
    selectInput("select_color_IC_enrichment", label = "Select color", 
                choices = list("Viridis", "Blues", "Reds","YlGnBu","YlOrRd"), 
                selected = "Viridis")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_enrichment_main_parameters_info"]], {
  showModal(
    modalDialog(
      IC_enrichment_main_parameters_info[["text"]],
      title = IC_enrichment_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_enrichment_main_parameters_info <- list(
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