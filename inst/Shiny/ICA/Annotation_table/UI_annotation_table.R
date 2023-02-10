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
  list_names_IC = str_split(values$Annotation[,"Type"], pattern = ',', n = Inf, simplify = FALSE)
  
  values$annotation_for_output = list()
  
  for (i in 1:length(list_names_IC)) {
    list_annotation = list_names_IC[[i]]
    for (j in list_annotation) {
      if(is.null(values$annotation_for_output[[j]]) && j != ""){
        j <- gsub("\\+", "\\\\+", j)
        values$annotation_for_output[[j]] = na.omit(rownames(values$Annotation)[grep("TRUE", values$Annotation)[grep(j, values$Annotation)-length(values$Annotation[,'Use'])]])
      }
    }
  }
  
  annotation = values$Annotation
  
  save(annotation,
       file = paste0(Shiny.options[["shiny_root"]], "../tmp_data/annotation_table.RData")
       )
})