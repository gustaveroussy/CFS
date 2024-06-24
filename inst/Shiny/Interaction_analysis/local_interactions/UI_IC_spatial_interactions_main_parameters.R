##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["plot_interactions_from_graph_main_parameters_UI"]] <- renderUI({
  req(values$data)
  tagList(
    radioButtons("transparency_interactions_choice", label = "Alpha type",
                 choices = list("Constant" = 1, "Scaling" = 2), 
                 selected = 1),
    sliderInput("transparency_interactions_range", "Alpha",
                min = 0, max = 1,
                value = 1, step = 0.01),
    numericInput("plot_interactions_size", "Spot size", 4, min = 0, max = NA),
    selectInput("select_color_interactions", label = "Select color", 
                choices = list("Magma" = "A", "Inferno" = "B", "Plasma" = "C", "Viridis" = "D", "Cividis" = "E", "Rocket" = "F", "Mako" = "G", "Turbo" = "H", "Blues", "Reds","YlGnBu","YlOrRd"), 
                selected = "D")
  )
})

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