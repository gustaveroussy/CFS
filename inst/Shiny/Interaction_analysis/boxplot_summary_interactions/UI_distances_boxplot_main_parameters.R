##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Boxplot_distance_main_parameters_UI"]] <- renderUI({
  req(values$data)
  fluidRow(
    column(width = 12,
           selectInput(
             "boxplot_interaction_filter_type",
             "Type of filter",
             NULL,
             selected = NULL,
             multiple = FALSE,
             selectize = TRUE,
             width = NULL,
             size = NULL
           ),
           selectInput(
             "boxplot_interaction_filter_1",
             "Filter by",
             NULL,
             selected = NULL,
             multiple = TRUE,
             selectize = TRUE,
             width = NULL,
             size = NULL
           ),
           selectInput(
             "boxplot_interaction_filter_2",
             "Filter by",
             NULL,
             selected = NULL,
             multiple = TRUE,
             selectize = TRUE,
             width = NULL,
             size = NULL
           ),
           selectInput(inputId = "boxplot_interaction_boxploint_type", label = "Type of point display", choices = c("all","outliers","suspectedoutliers","FALSE")),
           numericInput(inputId = "boxplot_interaction_z_score_filter",label = "Z-score filter",value = 3, min = 0)
    )
  )
})

##----------------------------------------------------------------------------##
## update filter type
##----------------------------------------------------------------------------##

observeEvent(input$choose_distances_to_determine,{
  req(input$choose_distances_to_determine)
  
  if(input$choose_distances_to_determine == "IC"){
    updateSelectInput(session, "boxplot_interaction_filter_type", label = "Type of filter", choices = c("IC",colnames(values$Annotation)), selected = NULL)
  }else if(input$choose_distances_to_determine == "Genes"){
    updateSelectInput(session, "boxplot_interaction_filter_type", label = "Type of filter", choices = c("lr interactions"), selected = NULL)
  }
  
})

##----------------------------------------------------------------------------##
## update filter selection
##----------------------------------------------------------------------------##

observeEvent(input$boxplot_interaction_filter_type,{
  req(input$boxplot_interaction_filter_type)
  
  if(input$choose_distances_to_determine == "IC"){
    if(input$boxplot_interaction_filter_type == "IC"){
      filter_value_1 = colnames(values$data@reductions$ica@cell.embeddings)
    } else {
      filter_value_1 = unique(values$Annotation[,input$boxplot_interaction_filter_type])
    }
  }else if(input$choose_distances_to_determine == "Genes"){
    if(input$boxplot_interaction_filter_type == "lr interactions"){
      filter_value_1 = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$ligand_gene_symbol
    }
  }
  
  if(input$choose_distances_to_determine == "IC"){
    if(input$boxplot_interaction_filter_type == "IC"){
      filter_value_2 = colnames(values$data@reductions$ica@cell.embeddings)
    } else {
      filter_value_2 = unique(values$Annotation[,input$boxplot_interaction_filter_type])
    }
  }else if(input$choose_distances_to_determine == "Genes"){
    if(input$boxplot_interaction_filter_type == "lr interactions"){
      filter_value_2 = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$receptor_gene_symbol
    }
  }
  
  updateSelectInput(session, "boxplot_interaction_filter_1", label = "Filter by", choices = filter_value_1, selected = NULL)
  updateSelectInput(session, "boxplot_interaction_filter_2", label = "Filter by", choices = filter_value_2, selected = NULL)
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