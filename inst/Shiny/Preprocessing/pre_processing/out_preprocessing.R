##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

preprocessing_values <- reactiveValues(button_check = 1, preprocessing_text_display = "<b>Data not available.</b>")

observeEvent(input$preprocessing_normalisation_action_button, {
  req(values$data)
  withProgress(message = 'Normalization', value = 0, {
    
    incProgress(0.4, detail = "Filtering empty spots")
    
    i <- if(substr(packageVersion("Seurat"),1,1) == "5"){(Matrix::colSums(values$data@assays$Spatial$counts, na.rm=T) != 0)}else{(Matrix::colSums(values$data@assays$Spatial@counts, na.rm=T) != 0)}
    row_names_df_to_keep<-colnames(values$data@assays$Spatial)[i]
    values$data = values$data[, row_names_df_to_keep]
    
    incProgress(0.4, detail = "Normalizing")
    
    values$data = PrepNormData(data=values$data,organism=input$preprocessing_specie_select,variable_features=input$preprocessing_variable_features, min_cells = input$preprocessing_minimum_cell)

    incProgress(0.2, detail = "Done")
  })
})

observeEvent(input$preprocessing_reduction_action_button, {
  req(values$data)
  
  values$annotation_for_output = list()
  values$IC_names = NULL
  values$Stat = NULL
  values$Annotation = NULL
  values$marker_gene = NULL
  values$HD_image = NULL
  values$HD_image_2 = NULL
  
  withProgress(message = 'Reduction', value = 0, {
    incProgress(0.4, detail = "ICA")
    values$data=ICASpatial(data=values$data,nics=input$preprocessing_number_of_ICs,maxit=input$preprocessing_maxit,method=input$preprocessing_ICA_function, kurtosis = input$preprocessing_kurtosis, sd = input$preprocessing_sd)
    
    incProgress(0.3, detail = "Preparing table")
    
    
    values$data@misc$annotation = matrix(data = "", nrow = length(colnames(values$data@reductions$ica@cell.embeddings)), ncol = 3)
    rownames(values$data@misc$annotation) = colnames(values$data@reductions$ica@cell.embeddings)
    colnames(values$data@misc$annotation) = c('Use','Type','Annotation')
    values$data@misc$annotation[,'Use'] = as.character(values$data@misc$GeneAndStat$Kurtosis_ICs > input$preprocessing_kurtosis)
    
    values$IC_names = names(values$data@misc$GeneAndStat$Kurtosis_ICs)[as.logical(values$data@misc$GeneAndStat$Kurtosis_ICs > 3)]
    
    values$data@misc$annotation = as.matrix(values$data@misc$annotation)
    
    values$Stat = values$data@misc[["GeneAndStat"]]
    
    updateSelectizeInput(session, "Ic_list", label = "list of IC",
                         choices = values$IC_names,
                         selected = NULL)
    
    values$Annotation = values$data@misc$annotation
    
    incProgress(0.2, detail = "Preparing CFS")
    # Get All annotations and their associated ICs
    associate_signal_with_IC()
    
    incProgress(0.1, detail = "Done")
  })
})

observeEvent(input$preprocessing_enrichment_action_button, {
  req(values$data)
  withProgress(message = 'Enrichment', value = 0, {
    incProgress(0.5)
    if(!is.null(input$preprocessing_database)){
      values$data = Enrich_ICA(data=values$data,dbs=input$preprocessing_database, kurtosis = values$data@misc$GeneAndStat$kurtosis_value, enrich_positive_genes = input$preprocessing_enrichment_positive, overwrite = input$preprocessing_enrichment_overwrite)
    }
  })
})

observe({
    preprocessing_values$preprocessing_text_display = paste0("<b>Number of spots : </b>", dim(values$data)[2],
                                                             "<br><b>Number of features : </b>", dim(values$data)[1]
    )
})
