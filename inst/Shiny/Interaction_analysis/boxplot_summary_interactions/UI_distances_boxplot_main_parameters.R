##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Boxplot_distance_main_parameters_UI"]] <- renderUI({
  req(values$data)
  fluidRow(
    column(width = 12,
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
## update filter selection
##----------------------------------------------------------------------------##

observe({
  if(!is.null(values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]])){
    tree_table = values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[input$choose_sample_for_distances]][[input$choose_method_for_distances]]
  } else if (!is.null(values$distances[[paste0(input$choose_distances_to_determine_2,"_",input$choose_distances_to_determine)]])){
    tree_table = values$distances[[paste0(input$choose_distances_to_determine_2,"_",input$choose_distances_to_determine)]][[input$choose_sample_for_distances]][[input$choose_method_for_distances]]
  } else {
    tree_table = NULL
  }

  req(tree_table)
  
  if(input$choose_distances_to_determine != "Genes" | input$choose_distances_to_determine_2 != "Genes"){
    if(input$choose_distances_to_determine == "Genes"){
        filter_value_1 = unique(read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$ligand_gene_symbol,read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$receptor_gene_symbol)
    } else if (input$choose_distances_to_determine != "Genes"){
      if(input$choose_distances_to_determine == "ica"){
        filter_value_1 = colnames(values$data@reductions$ica@cell.embeddings)
      } else {
        filter_value_1 = unique(values$Annotation[,input$choose_distances_to_determine])
      }
    }
    
    if(input$choose_distances_to_determine_2 == "Genes"){
      filter_value_2 = unique(read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$ligand_gene_symbol,read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$receptor_gene_symbol)
    } else if (input$choose_distances_to_determine_2 != "Genes"){
      if(input$choose_distances_to_determine_2 == "ica"){
        filter_value_2 = colnames(values$data@reductions$ica@cell.embeddings)
      } else {
        filter_value_2 = unique(values$Annotation[,input$choose_distances_to_determine_2])
      }
    }
  } else {
    filter_value_1 = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$ligand_gene_symbol
    filter_value_2 = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$receptor_gene_symbol
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