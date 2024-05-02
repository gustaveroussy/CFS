##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["IC_distance_main_parameters_UI"]] <- renderUI({
  req(values$data)
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
      selectInput("choose_sample_for_distances",
                  "Choose sample",
                  names(values$data@images),
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
      ),
      actionButton("start_distance_IC", "Start"),
      downloadButton("download_distance_tables", "Download graph table")
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_distance_main_parameters_info <- list(
  title = "Main parameters for distances",
  text = p("Options for the main parameters of distances")
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