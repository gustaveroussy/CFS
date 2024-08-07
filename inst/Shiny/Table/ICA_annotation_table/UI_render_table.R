##----------------------------------------------------------------------------##
## event_display_the_table
##----------------------------------------------------------------------------##
# make Annotation table
output[["Table_or_message"]] <- renderDT({
  DT = values$Annotation
  datatable(DT, editable = list(target = 'cell', disable = list(columns = c(0))), options = list(pageLength = 100), class = 'cell-border stripe', colnames = c('IC' = 1))
})