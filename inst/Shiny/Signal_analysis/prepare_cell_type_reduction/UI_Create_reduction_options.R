##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Output_or_message_cell_reduction"]] <- renderUI({
  fluidRow(
    column(width = 12,
      selectInput("choose_annotation_column_for_reduction",
       "Choose annotation column",
       names(values$annotation_for_output),
       selected = names(values$annotation_for_output)[1],
       multiple = FALSE,
       selectize = TRUE,
       width = NULL,
       size = NULL
      ),
      numericInput(
        "choose_filter_value_column_for_reduction",
        "Low pass filter",
        value = 0.025,
        min = 0,
        max = 1,
        step = 0.001,
        width = NULL
      ),
      actionButton("start_creating_reduction", "Start")
    )
  )
})
