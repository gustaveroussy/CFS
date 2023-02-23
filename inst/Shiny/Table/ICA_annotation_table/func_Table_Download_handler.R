##----------------------------------------------------------------------------##
## Handler for the download for RDS
##----------------------------------------------------------------------------##

output$download_table <- downloadHandler(
  filename = function() {
    paste("annotation", ".csv", sep = "")
  },
  content = function(file) {
    table = values$Annotation
    write.csv(table, file, row.names=TRUE)
  }
)