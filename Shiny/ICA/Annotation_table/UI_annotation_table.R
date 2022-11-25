output[["Annotation_table_UI"]] <- renderDT({
  DT = matrix(values$Annotation[input$IC_choice,], nrow = 1, ncol = 3, dimnames = list(c(input$IC_choice), c('Use','Type','Annotation')))
  datatable(DT, editable = list(target = 'cell', disable = list(columns = c(0))), class = 'cell-border stripe', colnames = c('IC' = 1))
})

observeEvent(input$Annotation_table_UI_cell_edit, {
  clmn <- input$Annotation_table_UI_cell_edit$col
  values$Annotation[input$IC_choice, clmn] <- input$Annotation_table_UI_cell_edit$value
})