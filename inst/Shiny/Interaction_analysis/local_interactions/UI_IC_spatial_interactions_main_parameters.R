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
                choices = list("Greater" = "greater", "Two sided" = "two.sided", "Lower" = "less"), 
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
  text = p("Options for the main parameters of interactions")
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