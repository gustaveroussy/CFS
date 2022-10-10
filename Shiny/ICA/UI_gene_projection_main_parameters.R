##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["gene_IC_main_parameters_UI"]] <- renderUI({
  tagList(
    selectInput(
      "gene_projection_IC_choice",
      label = "Choose IC related to genes to plot",
      choices = names(Launch_analysis()@ica)[-1]
    )
  )
})

output[["gene_choice_main_parameters_UI"]] <- renderUI({
  selectizeInput("gene_projection_gene_choice", label = "Choose gene to plot",
                 choices = Launch_analysis()@ica[[input$gene_projection_IC_choice]]$IC_top_genes[1:100],
                 selected = NULL,
                 multiple = TRUE,
                 options = NULL)
})

output[["gene_color_choice_main_parameters_UI"]] <- renderUI({
  selectInput("select_color_gene_projection", label = "Select color", 
              choices = list("Viridis", "Blues", "Reds","YlGnBu","YlOrRd"), 
              selected = "Viridis")
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["gene_projection_main_parameters_info"]], {
  showModal(
    modalDialog(
      gene_projection_main_parameters_info[["text"]],
      title = gene_projection_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
gene_projection_main_parameters_info <- list(
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