output[["Annotation_table_UI"]] <- renderDT({
  req(values$Annotation)
  DT = matrix(values$Annotation[input$IC_choice,], nrow = 1, ncol = 3, dimnames = list(c(input$IC_choice), c('Use','Type','Annotation')))
  datatable(DT, editable = list(target = 'cell', disable = list(columns = c(0))), class = 'cell-border stripe', colnames = c('IC' = 1))
})

observeEvent(input$Annotation_table_UI_cell_edit, {
  req(values$Annotation)
  clmn <- input$Annotation_table_UI_cell_edit$col
  values$Annotation[input$IC_choice, clmn] <- input$Annotation_table_UI_cell_edit$value
  
  # Get All annotations and their associated ICs
  values$annotation_for_output = list()

  # Get All annotations and their associated ICs
  list_names_IC = unique(unlist(str_split(values$Annotation[,"Type"], pattern = ',', n = Inf, simplify = FALSE)))

  for (list_annotation in list_names_IC) {
    if(list_annotation != ""){
      list_annotation <- gsub("\\+", "\\\\+", list_annotation)
      result = values$Annotation[,'Use'] == TRUE & values$Annotation[,'Type'] == list_annotation
      values$annotation_for_output[[list_annotation]] = names(result[result])
    }
  }

  annotation = values$Annotation
  
  save(annotation,
       file = paste0(Shiny.options[["shiny_root"]], "../tmp_data/annotation_table.RData")
  )
})