##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

preprocessing_values <- reactiveValues(button_check = 1, preprocessing_text_display = "<b>Data not available.</b>")

observeEvent(input$preprocessing_action_button, {
  req(values$data)
  if (input$preprocessing_action_button == preprocessing_values$button_check) {
    withProgress(message = 'Pre-processing', value = 0, {
      incProgress(0.2, detail = "Normalize")
      values$data = PrepNormData(data=values$data,organism=input$preprocessing_specie_select,variable_features=input$preprocessing_variable_features)
      incProgress(0.4, detail = "Calculating ICs")
      values$data=ICASpatial(data=values$data,ncis=input$preprocessing_number_of_ICs,maxit=input$preprocessing_maxit,method=input$preprocessing_ICA_function,sd=input$preprocessing_kurtosis)
      incProgress(0.3, detail = "Enriching")
      
      values$data = Show_IC_and_Enrich(data=values$data,dbs=input$preprocessing_database)
      incProgress(0.1, detail = "Done")
    
      
      if (is.null(values$data@misc$annotation)){
        row_names = names(values$data@misc)[grep('IC_', names(values$data@misc))]
        values$data@misc$annotation = matrix(data = "", nrow = length(row_names), ncol = 3)
        rownames(values$data@misc$annotation) = row_names
        colnames(values$data@misc$annotation) = c('Use','Type','Annotation')
        values$data@misc$annotation[,'Use'] = TRUE
      }
      
      values$IC_names = rownames(values$data@misc$annotation)
      
      values$Stat = values$data@misc[["GeneAndStat"]]
      
      updateSelectizeInput(session, "Ic_list", label = "list of IC",
                           choices = values$IC_names,
                           selected = NULL)
      
      values$Annotation = values$data@misc$annotation
      
      # Get All annotations and their associated ICs
      list_names_IC = str_split(values$Annotation[,"Type"], pattern = ',', n = Inf, simplify = FALSE)
      
      for (i in 1:length(list_names_IC)) {
        list_annotation = list_names_IC[[i]]
        for (j in list_annotation) {
          if(is.null(values$annotation_for_output[[j]]) && j != ""){
            j <- gsub("\\+", "\\\\+", j)
            values$annotation_for_output[[j]] = na.omit(rownames(values$data@misc$annotation)[grep("TRUE", values$Annotation)[grep(j, values$Annotation)-length(values$Annotation[,'Use'])]])
          }
        }
      }
      
      
    })
      preprocessing_values$button_check <- input$preprocessing_action_button + 1
  }
})

observe({
  if(!is.null(values$data)){
    if(names(values$data@assays) %in% "SCT"){
      preprocessing_values$preprocessing_text_display = paste0("<b>Number of spots : </b>", dim(values$data@assays$SCT@scale.data)[2],
                                                               "<br><b>Number of features : </b>", dim(values$data@assays$SCT@scale.data)[1]
      )
    } else {
      preprocessing_values$preprocessing_text_display = paste0("<b>Number of spots : </b>", dim(values$data@assays$Spatial@counts)[2],
                                                               "<br><b>Number of features : </b>", dim(values$data@assays$Spatial@counts)[1]
      )
    }
  }
})
