##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##
# make Annotation table
output[["Table_or_message"]] <- renderDT({
  DT = values$Annotation
  datatable(DT, editable = list(target = 'cell', disable = list(columns = c(0))), options = list(pageLength = 100), class = 'cell-border stripe', colnames = c('IC' = 1))
})

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