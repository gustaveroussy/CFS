##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["gene_choice_main_parameters_UI"]] <- renderUI({
  req(values$data)
  tagList(
    selectizeInput("gene_projection_gene_choice", label = "Choose gene to plot",
                   choices = names(sort(abs(values$data@reductions$ica@feature.loadings[,input$IC_choice]),decreasing = T)),
                   selected = NULL,
                   multiple = TRUE,
                   options = NULL),
    sliderInput("transparency_gene_projection", "Alpha",
                min = 0, max = 1,
                value = 1, step = 0.01)
  )
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
      <li><b>Choose gene to plot:</b> Select here which genes you want to use in the plot on the right</li>
      <li><b>Select color:</b> Select here which color scale you want to use in the plot on the right.</li>
    </ul>
    "
  )
)