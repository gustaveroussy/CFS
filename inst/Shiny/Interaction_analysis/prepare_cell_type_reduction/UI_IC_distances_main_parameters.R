##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["IC_distance_main_parameters"]] <- renderUI({
  fluidRow(
    column(width = 12,
      selectInput("choose_method_for_distances",
       "Choose method",
       c("Lee"),
       selected = "Lee",
       multiple = FALSE,
       selectize = TRUE,
       width = NULL,
       size = NULL
      ),
      actionButton("start_distance_ICs", "Start")
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_distance_main_parameters_info <- list(
  title = "Create cell annotation",
  text = p("Create a reduction for a specific annotation")
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_distance_main_parameters_info"]], {
  showModal(
    modalDialog(
      IC_distance_main_parameters_info[["text"]],
      title = IC_distance_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})