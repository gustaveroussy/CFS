##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Boxplot_distance_main_parameters_UI"]] <- renderUI({
  req(values$data)
  fluidRow(
    column(width = 12,
           selectInput(inputId = "boxplot_interaction_boxploint_type", label = "Type of point display", choices = c("all","outliers","suspectedoutliers","FALSE")),
           numericInput(inputId = "boxplot_interaction_z_score_filter",label = "Z-score filter",value = 3, min = 0)
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
Boxplot_distance_main_parameters_info <- list(
  title = "Main parameters for interaction boxplot",
  text = p("Options for the main parameters of interaction boxplot")
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["Boxplot_distance_main_parameters_info"]], {
  showModal(
    modalDialog(
      Boxplot_distance_main_parameters_info[["text"]],
      title = Boxplot_distance_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})