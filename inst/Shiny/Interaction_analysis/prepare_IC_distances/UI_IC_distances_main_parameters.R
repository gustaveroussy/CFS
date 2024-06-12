##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["IC_distance_main_parameters_UI"]] <- renderUI({
  req(values$data)
  fluidRow(
    column(width = 12,
       selectInput("choose_distances_to_determine",
                   "Choose graph",
                   c("IC","Genes"),
                   selected = "IC",
                   multiple = FALSE,
                   selectize = TRUE,
                   width = NULL,
                   size = NULL
       ),
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
                  unique(sapply(strsplit(rownames(values$data@meta.data), "_"),"[[",1)),
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
      ),
      conditionalPanel(
        condition = "input.choose_distances_to_determine == 'Genes'",
        selectizeInput("choose_ic_for_genes_filter_for_distances", "Filter genes by ICs",
                       choices = rownames(values$Annotation)
                      )
      ),
      shinyWidgets::awesomeCheckbox(
        inputId = "use_positive_values_for_distances",
        label = "Use IC positive values",
        value = TRUE
      ),
      actionButton("start_distance_IC", "Start"),
      downloadButton("download_distance_tables", "Download graph table")
    )
  )
})

output[["IC_distance_main_parameters_UI_2"]] <- renderUI({
  req(values$data)
  fluidRow(
    column(width = 12,
           numericInput("Z_score_for_distances",
                        "Z-score filter",
                        3,
                        min = 0,
                        step = 0.01
           ),
           selectInput("choose_vertices_color_for_distances",
                       "Vertices annotation",
                       colnames(values$Annotation),
                       selected = "Type",
                       multiple = FALSE,
                       selectize = TRUE,
                       width = NULL,
                       size = NULL
           ),
           selectInput("choose_n_dim_for_distances",
                       "number of dimensions",
                       choices = list("2d" = 2,"3d" = 3)
           ),
           numericInput("choose_vertices_size_for_distances",
                        "Vertices size",
                        10,
                        min = 1,
                        step = 0.01
           ),
           numericInput("choose_edges_size_for_distances",
                        "Edges size",
                        2,
                        min = 0.01,
                        step = 0.01
           )
    )
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