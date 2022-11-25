table <- reactiveValues(annotation = NULL)

observeEvent(input[["input_file"]], {
  req(Launch_analysis())
  
  table$annotation <- Launch_analysis()@misc$annotation
})

output[["Annotation_table_UI"]] <- renderDT({
  DT = table$annotation
  datatable(DT, editable = TRUE)
})

observeEvent(input$Annotation_table_UI_cell_edit, {
  row  <- input$Annotation_table_UI_cell_edit$row
  clmn <- input$Annotation_table_UI_cell_edit$col
  table$annotation[row, clmn] <- input$Annotation_table_UI_cell_edit$value
  print(table$annotation)
})