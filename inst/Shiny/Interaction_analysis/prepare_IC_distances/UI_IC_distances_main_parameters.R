##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["IC_distance_main_parameters_UI_1"]] <- renderUI({
  selectInput("choose_method_for_distances",
              "Choose method",
              c("Lee","Dotproduct","Dotproduct Genes"),
              selected = "Lee",
              multiple = FALSE,
              selectize = TRUE,
              width = NULL,
              size = NULL
  )
})

output[["IC_distance_main_parameters_UI_2"]] <- renderUI({
  req(values$data)
  fluidRow(
           column(width = 6,
             selectInput("choose_distances_to_determine",
                   "Choose value 1",
                   c(names(values$data@reductions)[!(names(values$data@reductions) %in% c("umap", "pca"))],"Genes"),
                   multiple = FALSE,
                   selectize = TRUE,
                   width = NULL,
                   size = NULL
             )
            ),
           column(width = 6,
             selectInput("choose_distances_to_determine_2",
                         "Choose value 2",
                         c(names(values$data@reductions)[!(names(values$data@reductions) %in% c("umap", "pca"))],"Genes"),
                         selected = "IC",
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL
             )
          ),
    column(width = 12,
      selectInput("choose_sample_for_distances",
                  "Choose sample",
                  c(names(values$data@images),"All"),
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
      ),
      conditionalPanel(
        condition = "input.choose_distances_to_determine == 'Genes' | input.choose_distances_to_determine_2 == 'Genes'",
        selectInput("choose_ic_for_genes_filter_for_distances_ligand", "Filter ligands by ICs",
                       choices = rownames(values$Annotation), multiple = TRUE
                      ),
        selectInput("choose_ic_for_genes_filter_for_distances_receptor", "Filter receptors by ICs",
                    choices = rownames(values$Annotation), multiple = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.choose_distances_to_determine != 'Genes' | input.choose_distances_to_determine_2 != 'Genes'",
          shinyWidgets::awesomeCheckbox(
            inputId = "use_positive_values_for_distances",
            label = "Use positive values",
            value = TRUE
          )
      ),
      actionButton("start_distance_IC", "Start"),
      actionButton("start_distance_IC_batch", "Batch process"),
      downloadButton("download_distance_tables", "Download graph table"),
      shinyDirButton("download_distance_tables_batch", "Batch download", "Choose a directory",FALSE)
    )
  )
})

output[["IC_distance_main_parameters_UI_3"]] <- renderUI({
  req(values$data)
  fluidRow(
    column(width = 12,
           numericInput("Z_score_for_distances",
                        "Z-score filter",
                        3,
                        min = 0,
                        step = 0.01
           ),
           selectInput("choose_layout_for_distances",
                       "Layout",
                       list("Fruchterman Reingold" = "fr", "Davidson Harel" = "dh",
                            "graphopt" = "graphopt", "Kamada Kawai" = "kk",
                            "Multidimensional Scaling" = "mds", "Sugiyama" = "sugiyama"),
                       selected = "fr",
                       multiple = FALSE,
                       selectize = TRUE,
                       width = NULL,
                       size = NULL
           ),
           selectInput("choose_vertices_color_for_distances",
                       "Vertices Color",
                       colnames(values$Annotation),
                       selected = "Type",
                       multiple = FALSE,
                       selectize = TRUE,
                       width = NULL,
                       size = NULL
           ),
           selectInput("choose_vertices_annotation_for_distances",
                       "Vertices Annotation",
                       colnames(values$Annotation),
                       selected = "Annotation",
                       multiple = FALSE,
                       selectize = TRUE,
                       width = NULL,
                       size = NULL
           ),
           selectInput("choose_n_dim_for_distances",
                       "Number of dimensions",
                       choices = list("2d" = 2,"3d" = 3)
           ),
           numericInput("choose_vertices_size_for_distances",
                        "Vertices size",
                        40,
                        min = 1,
                        step = 0.01
           ),
           numericInput("choose_edges_size_for_distances",
                        "Edges size",
                        5,
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
  title = "Main parameters for graph",
  text = HTML("
    Main parameters for the IC distances tab.
    <ul>
    <li><b>Choose method:</b> Method to determine the distances between values</li>
    <li><b>Choose value 1/2:</b> Choose wich reduction to compare between one another</li>
    <li><b>Choose sample:</b> Choose which sample sans create the distance graph</li>
    <li><b>Use positive values:</b> Set negative values from the reduction table to 0</li>
    </ul>
    ")
)

interactions_from_graph_display_info <- list(
  title = "Display parameters for graph",
  text = HTML("
    Display parameters for the IC distances tab.
    <ul>
    <li><b>Z-score filter:</b> Filter edges by keeping every value above the given Z-score</li>
    <li><b>Layout:</b> Choose the layout of the graph</li>
    <li><b>Vertices Color:</b> Color vertices based on the chosen annotation</li>
    <li><b>Vertices Annotation:</b> Annotation to display upon hovering over vestices</li>
    <li><b>Number of dimensions:</b> Choose the number of dimensions for the graph</li>
    <li><b>Vertices size:</b> Choose vertices size</li>
    <li><b>Edges size:</b> Choose edges thickness</li>
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

observeEvent(input[["interactions_from_graph_display_info"]], {
  showModal(
    modalDialog(
      interactions_from_graph_display_info[["text"]],
      title = interactions_from_graph_display_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
