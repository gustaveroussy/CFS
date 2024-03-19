##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

observeEvent(input$start_UMAP, {
  if(input$Plot_analysis_type == "tSNE"){
    type = NULL
    
    if (!is.null(input$Plot_display_IC_choice)) {
      if (!is.null(input$Plot_display_type_UMAP_choice)){
        if(length(input$Plot_display_type_UMAP_choice) != 1){
          for (n_cell_type in input$Plot_display_type_UMAP_choice) {
            if(is.null(type)) {
              type = values$annotation_for_output[[n_cell_type]]
            } else {
              type = append(type, values$annotation_for_output[[n_cell_type]])
            }
          }
          type = unique(type)
        } else {
          type = values$annotation_for_output[[input$Plot_display_type_UMAP_choice]]
        }
        
        type = type[type %in% rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"]]
        
        values$data= RunTSNE(values$data, reduction = "ica", dims = as.integer(gsub('[IC_]','',unique(c(type,input$Plot_display_IC_choice)))), perplexity = input$Plot_perplexity, reduction.name = input$reddim_named_by_user)
      } else {
        values$data= RunTSNE(values$data, reduction = "ica", dims = as.integer(gsub('[IC_]','',input$Plot_display_IC_choice)), perplexity = input$Plot_perplexity, reduction.name = input$reddim_named_by_user)
      }
    } else if (!is.null(input$Plot_display_type_UMAP_choice)){
      if(length(input$Plot_display_type_UMAP_choice) != 1){
        name = paste(input$Plot_display_type_UMAP_choice,collapse = ",")
        for (n_cell_type in input$Plot_display_type_UMAP_choice) {
          if(is.null(type)) {
            type = values$annotation_for_output[[n_cell_type]]
          } else {
            type = append(type, values$annotation_for_output[[n_cell_type]])
          }
        }
        type = unique(type)
      } else {
        type = values$annotation_for_output[[input$Plot_display_type_UMAP_choice]]
      }
      
      type = type[type %in% rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"]]
      
      values$data=RunTSNE(values$data, reduction = "ica", dims = as.integer(gsub('[IC_]','',type)), perplexity = input$Plot_perplexity, reduction.name = input$reddim_named_by_user)
    } else {
      if(is.null(values$data@reductions$tsne)){
        shinyalert("tSNE error", "No tSNE can be calculated", type = "error")
      }
    }
  }
  
})