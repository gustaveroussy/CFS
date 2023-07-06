##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

preprocessing_values <- reactiveValues(button_check = 1, preprocessing_text_display = "<b>Data not available.</b>")

observeEvent(input$preprocessing_action_button, {
  req(values$data)
  if (input$preprocessing_action_button == preprocessing_values$button_check) {
    
    values$annotation_for_output = list()
    values$IC_names = NULL
    values$Stat = NULL
    values$Annotation = NULL
    values$UMAP = NULL
    values$marker_gene = NULL
    values$HD_image = NULL
    values$HD_image_2 = NULL
    
    i <- (Matrix::colSums(values$data@assays$Spatial@counts, na.rm=T) != 0)
    row_names_df_to_keep<-colnames(values$data@assays$Spatial)[i]
    values$data = values$data[, row_names_df_to_keep]
    
    withProgress(message = 'Pre-processing', value = 0, {
      incProgress(0.2, detail = "Normalize")
      values$data = PrepNormData(data=values$data,organism=input$preprocessing_specie_select,variable_features=input$preprocessing_variable_features)
      incProgress(0.4, detail = "Calculating ICs")
      values$data=ICASpatial(data=values$data,ncis=input$preprocessing_number_of_ICs,maxit=input$preprocessing_maxit,method=input$preprocessing_ICA_function, kurtosis = input$preprocessing_kurtosis, sd = input$preprocessing_sd)
      incProgress(0.3, detail = "Enriching")
      if(!is.null(input$preprocessing_database)){
        values$data = Enrich_ICA(data=values$data,dbs=input$preprocessing_database, kurtosis = values$data@misc$GeneAndStat$kurtosis_value)
      }
      incProgress(0.1, detail = "Done")
      
      if(is.null(values$data@misc$GeneAndStat$kurtosis_value)){
        values$IC_names = names(values$data@misc$GeneAndStat$Kurtosis_ICs)[values$data@misc$GeneAndStat$Kurtosis_ICs > 3]
      } else {
        values$IC_names = names(values$data@misc$GeneAndStat$Kurtosis_ICs)[values$data@misc$GeneAndStat$Kurtosis_ICs > values$data@misc$GeneAndStat$kurtosis_value]
      }
    
      row_names = values$IC_names
      values$data@misc$annotation = matrix(data = "", nrow = length(row_names), ncol = 3)
      rownames(values$data@misc$annotation) = row_names
      colnames(values$data@misc$annotation) = c('Use','Type','Annotation')
      values$data@misc$annotation[,'Use'] = TRUE
      
      values$data@misc$annotation = as.matrix(values$data@misc$annotation)
      
      values$Stat = values$data@misc[["GeneAndStat"]]
      
      updateSelectizeInput(session, "Ic_list", label = "list of IC",
                           choices = values$IC_names,
                           selected = NULL)
      
      values$Annotation = values$data@misc$annotation
      
      # Get All annotations and their associated ICs
      list_names_IC = unique(unlist(str_split(values$Annotation[,"Type"], pattern = ',', n = Inf, simplify = FALSE)))
      
      for (list_annotation in list_names_IC) {
        if(list_annotation != ""){
          list_annotation <- gsub("\\+", "\\\\+", list_annotation)
          result = values$Annotation[,'Use'] == TRUE & values$Annotation[,'Type'] == list_annotation
          values$annotation_for_output[[list_annotation]] = names(result[result])
        }
      }
      
      
    })
      preprocessing_values$button_check <- input$preprocessing_action_button + 1
  }
})

observe({
    preprocessing_values$preprocessing_text_display = paste0("<b>Number of spots : </b>", dim(values$data)[2],
                                                             "<br><b>Number of features : </b>", dim(values$data)[1]
    )
})
