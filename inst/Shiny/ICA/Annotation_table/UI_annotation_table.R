output[["Annotation_table_UI"]] <- renderDT({
  req(values$Annotation)
  DT = matrix(values$Annotation[input$IC_choice,], nrow = 1, ncol = ncol(values$Annotation), dimnames = list(c(input$IC_choice), colnames(values$Annotation)))
  datatable(DT, editable = list(target = 'cell', disable = list(columns = c(0))), class = 'cell-border stripe', colnames = c('IC' = 1))
})

observeEvent(input$Annotation_table_UI_cell_edit, {
  req(values$Annotation)
  clmn <- input$Annotation_table_UI_cell_edit$col
  values$Annotation[input$IC_choice, clmn] <- input$Annotation_table_UI_cell_edit$value
  
  # Get All annotations and their associated ICs
  associate_signal_with_IC()

  annotation = values$Annotation
  
  save(annotation,
       file = paste0(Shiny.options[["shiny_root"]], "../tmp_data/annotation_table.RData")
  )
})