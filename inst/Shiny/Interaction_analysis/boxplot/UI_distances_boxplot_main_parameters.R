##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Boxplot_distance_main_parameters_UI"]] <- renderUI({
  req(values$data)
  fluidRow(
    column(width = 12,
           selectInput(inputId = "boxplot_interaction_boxplot",
                       label = "Type of display",
                       choices = c("All values", "Interaction analysis","Niche analysis")),
           selectInput(inputId = "boxplot_interaction_boxploint_type", label = "Type of point display", choices = c("all","outliers","suspectedoutliers","FALSE")),
           numericInput(inputId = "boxplot_interaction_z_score_filter",label = "Z-score filter",value = 3, min = 0),
           checkboxInput("boxplot_interaction_compare_with_randomized", label = "Compare with randomized", value = FALSE)
    )
  )
})

output[["Boxplot_distance_main_parameters_UI_2"]] <- renderUI({
  req(values$data)
  fluidRow(
    if(input$boxplot_interaction_boxplot == "All values"){
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
             )
      )
    } else if (input$boxplot_interaction_boxplot == "Interaction analysis") {
      column(width = 12,
             selectInput(
               "boxplot_interaction_selection_value",
               "Value",
               c("sumLee","meanLee"),
               selected = NULL,
               multiple = FALSE
             ),
             selectInput(
               "boxplot_interaction_selection_1",
               "Reduction choice 1",
               names(values$data@reductions)[!(names(values$data@reductions) %in% c("umap","tsne"))],
               selected = NULL,
               multiple = FALSE
             ),
             selectInput(
               "boxplot_interaction_1",
               "Seach with",
               NULL,
               selected = NULL,
               multiple = TRUE
             ),
             selectInput(
               "boxplot_interaction_selection_2",
               "Reduction choice 2",
               names(values$data@reductions)[!(names(values$data@reductions) %in% c("umap","tsne"))],
               selected = NULL,
               multiple = FALSE
             ),
             selectInput(
               "boxplot_interaction_2",
               "Distance with",
               NULL,
               selected = NULL,
               multiple = TRUE
             )
      )
    } else if (input$boxplot_interaction_boxplot == "Niche analysis") {
      column(width = 12,
             selectInput(
               "boxplot_niche_interaction_selection_1",
               "Seach with",
               colnames(values$data@reductions$ica@cell.embeddings),
               selected = colnames(values$data@reductions$ica@cell.embeddings)[1],
               multiple = FALSE
             ),
             selectInput(
               "boxplot_niche_interaction_selection_2",
               "Reduction choice 2",
               names(values$data@reductions)[!(names(values$data@reductions) %in% c("umap","ica","pca","tsne"))],
               selected = NULL,
               multiple = FALSE
             ),
             selectInput(
               "boxplot_niche_interaction_2",
               "Distance with",
               NULL,
               selected = NULL,
               multiple = TRUE
             )
      )
    }
  )
})

output[["Boxplot_distance_main_parameters_UI_3"]] <- renderUI({
  actionButton("start_distance_boxplot", "Start")
})

##----------------------------------------------------------------------------##
## update reduction selection
##----------------------------------------------------------------------------##

observeEvent(input$boxplot_interaction_selection_1,{
  updateSelectInput(session, "boxplot_interaction_1", choices = if(input$boxplot_interaction_selection_1 == "ica"){
      colnames(values$data@reductions[[input$boxplot_interaction_selection_1]]@cell.embeddings)
    } else {
      values$data@misc$reduction_names[[input$boxplot_interaction_selection_1]]
    }
  )
})

observeEvent(input$boxplot_interaction_selection_2,{
  updateSelectInput(session, "boxplot_interaction_2", choices = if(input$boxplot_interaction_selection_2 == "ica"){
    colnames(values$data@reductions[[input$boxplot_interaction_selection_2]]@cell.embeddings)
  } else {
    values$data@misc$reduction_names[[input$boxplot_interaction_selection_2]]
  }
  )
})

observeEvent(input$boxplot_niche_interaction_selection_2,{
  updateSelectInput(session, "boxplot_niche_interaction_2", choices = values$data@misc$reduction_names[[input$boxplot_niche_interaction_selection_2]]
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
      } else if (input$choose_distances_to_determine == "Metadata"){
        filter_value_1 = colnames(values$data@meta.data)
      } else {
        filter_value_1 = unique(values$Annotation[,input$choose_distances_to_determine])
      }
    }
    
    if(input$choose_distances_to_determine_2 == "Genes"){
      filter_value_2 = unique(read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$ligand_gene_symbol,read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$receptor_gene_symbol)
    } else if (input$choose_distances_to_determine_2 != "Genes"){
      if(input$choose_distances_to_determine_2 == "ica"){
        filter_value_2 = colnames(values$data@reductions$ica@cell.embeddings)
      } else if (input$choose_distances_to_determine_2 == "Metadata"){
        filter_value_2 = colnames(values$data@meta.data)
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
  text = HTML("
	Options for the main parameters of Boxplot distances, used to compare samples between them
		
	<li><b>Type of point display:</b>
		<ul>
			<li><b>all:</b> Display all points next to the blox plot</li>
			<li><b>outliers/suspected outliers:</b> Display boxplot outliers</li>
			<li><b>FALSE:</b> Does not display outiliers, boxplot will cover all points</li>
		</ul>
	</li>

	<li><b>Z-score filter:</b> Filter out all boxplot below the given Z-score</li>

	<li><b>Type of display:</b>
	  <ul>
		<li><b>All values:</b> </li>


		<li><b>Interaction analysis:</b> 
		  <ul>
			<li><b>Value:</b> interaction value to compare between samples</li>
			<li><b>Reduction choice 1/2:</b> Which reduction to compare between samples</li>
			<li><b>Search/Distance with:</b> Which reduction values to compare between samples</li>
		  </ul>
		</li>
		
		<li><b>Niche analysis:</b> 
		  <ul>
			<li><b>Reduction choice 2:</b> Which reduction to compare between samples</li>
			<li><b>Search/Distance with:</b> Which reduction values to compare between samples</li>
		  </ul>
		</li>
		  
		  
	  </ul>
	</li>
    ")
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
