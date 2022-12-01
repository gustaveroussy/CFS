output[["Annotation_table_UI"]] <- renderDT({
  DT = matrix(values$Annotation[input$IC_choice,], nrow = 1, ncol = 3, dimnames = list(c(input$IC_choice), c('Use','Type','Annotation')))
  datatable(DT, editable = list(target = 'cell', disable = list(columns = c(0))), class = 'cell-border stripe', colnames = c('IC' = 1))
})

observeEvent(input$Annotation_table_UI_cell_edit, {
  clmn <- input$Annotation_table_UI_cell_edit$col
  values$Annotation[input$IC_choice, clmn] <- input$Annotation_table_UI_cell_edit$value
  
  # Get All annotations and their associated ICs
  list_names_IC = str_split(values$Annotation[,"Type"], pattern = ',', n = Inf, simplify = FALSE)
  values$annotation_for_output = list()
  for (i in 1:length(values$IC_names)) {
    for (j in 1:length(list_names_IC[[i]])) {
      if(!is.null(values$annotation_for_output[[list_names_IC[[i]][j]]])){
        values$annotation_for_output[[list_names_IC[[i]][j]]] = append(values$annotation_for_output[[list_names_IC[[i]][j]]], c(values$IC_names[i]))
        values$annotation_for_output[[list_names_IC[[i]][j]]] = unique(values$annotation_for_output[[list_names_IC[[i]][j]]])
      } else {
        values$annotation_for_output[[list_names_IC[[i]][j]]] = c(values$IC_names[i])
      }
    }
  }
})