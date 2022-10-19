##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["Plot_main_parameters_UI"]] <- renderUI({
  tagList(
    selectInput("Plot_analysis_type", label = "Select method to use", 
                choices = list("UMAP"), 
                selected = "UMAP"),
    selectInput("Plot_display_type", label = "Select what to color", 
                choices = list("Clustering", "Ploïdie"), 
                selected = "Clustering"),
    selectizeInput("gene_projection_gene_choice", label = "Choose IC to plot",
                   choices = names(Launch_analysis()@misc)[-1],
                   selected = NULL,
                   multiple = TRUE,
                   options = NULL),
    numericInput("Plot_resolution", "Enter Plot resolution", 1.2,
      min = 0.1, max = 2, step = 0.1
    ),
    actionButton("start_plot", "Start plot"),
    downloadButton("download_RDS", "Download RDS")
    )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["Plot_main_parameters_info"]], {
  showModal(
    modalDialog(
      Plot_main_parameters_info[["text"]],
      title = Plot_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
Plot_main_parameters_info <- list(
  title = "Main parameters for total gene heatmap",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li>
      <li><b>Color cells by:</b> Select which variable, categorical or continuous, from the meta data should be used to color the cells.</li>
    </ul>
    "
  )
)