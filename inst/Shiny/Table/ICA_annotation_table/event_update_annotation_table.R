##----------------------------------------------------------------------------##
## event update the table when edited
##----------------------------------------------------------------------------##
observeEvent(input$Table_or_message_cell_edit, {
  clmn <- input$Table_or_message_cell_edit$col
  rown <- input$Table_or_message_cell_edit$row
  values$Annotation[rown, clmn] <- input$Table_or_message_cell_edit$value
  # Get All annotations and their associated ICs
  
  associate_signal_with_IC()
  
  annotation = values$Annotation
  
  save(annotation,
       file = paste0(Shiny.options[["shiny_root"]], "/../tmp_data/annotation_table.RData")
  )
  
})