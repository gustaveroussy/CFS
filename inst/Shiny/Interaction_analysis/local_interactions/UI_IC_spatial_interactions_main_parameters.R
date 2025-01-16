##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["plot_interactions_from_graph_main_parameters_UI"]] <- renderUI({
  req(values$data)
  tagList(
    selectInput("select_interaction_1", label = "Interaction 1", 
                choices = if(input$choose_distances_to_determine == "Genes") {rownames(GetAssayData(values$data))} else if(input$choose_distances_to_determine == "ica"){colnames(values$data@reductions$ica@cell.embeddings)}else{values$data@misc$reduction_names[[input$choose_distances_to_determine]]}
                ),
    selectInput("select_interaction_2", label = "Interaction 2", 
                choices = if(input$choose_distances_to_determine_2 == "Genes") {rownames(GetAssayData(values$data))} else if(input$choose_distances_to_determine_2 == "ica"){colnames(values$data@reductions$ica@cell.embeddings)}else{values$data@misc$reduction_names[[input$choose_distances_to_determine_2]]},
                ),
    radioButtons("transparency_interactions_choice", label = "Alpha type",
                 choices = list("Constant" = 1, "Scaling" = 2), 
                 selected = 1),
    sliderInput("transparency_interactions_range", "Alpha",
                min = 0, max = 1,
                value = 1, step = 0.01),
    numericInput("plot_interactions_size", "Spot size", 4, min = 0, max = NA),
    selectInput("select_color_interactions", label = "Select color", 
                choices = list("Magma" = "A", "Inferno" = "B", "Plasma" = "C", "Viridis" = "D", "Cividis" = "E", "Rocket" = "F", "Mako" = "G", "Turbo" = "H", "Blues", "Reds","YlGnBu","YlOrRd"), 
                selected = "D"),
    numericInput("plot_interactions_n_rando", "Number of permutations", 500, min = 1, max = NA),
    selectInput("select_correction_method_interactions", label = "Correction method", 
                choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), 
                selected = "bonferroni"),
    selectInput("select_alternative_interactions", label = "Statistic alternative", 
                choices = list("Greater" = "greater", "Lower" = "less"), 
                selected = "Greater"),
    actionButton("interaction_plot_start", "Start")
  )
})

##----------------------------------------------------------------------------##
## update when clicking an edge
##----------------------------------------------------------------------------##



##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
interactions_from_graph_main_parameters_info <- list(
  title = "Main parameters for interactions",
  text = HTML("
    Options for the main parameters of interactions
    <ul>
    <li><b>Interaction 1/2:</b> Choose the values to plot the interaction of</li>
    <li><b>Alpha type:</b>
      <ul>
        <li><b>Constant:</b> Alpha uniform through the sample</li>
        <li><b>Scaling:</b> Alpha scaled with signal intensity</li>
      </ul>
    </li>
    <li><b>Alpha:</b> Change scale of the transparency</li>
    <li><b>Spot size:</b> Change the size of spots</li>
    <li><b>Number of dimensions:</b> Choose the number of dimensions for the graph</li>
    <li><b>Vertices size:</b> Choose vertices size</li>
    <li><b>Number of permutations:</b> Number of permutations for the randomization test</li>
    <li><b>Correction method:</b> p-value correction method</li>
    <li><b>Statistic alternative:</b> Alternative of the test, greater: values significantly associated, Lower: values significantly dissociated</li>
    </ul>
    ")
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["interactions_from_graph_main_parameters_info"]], {
  showModal(
    modalDialog(
      interactions_from_graph_main_parameters_info[["text"]],
      title = interactions_from_graph_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
