##----------------------------------------------------------------------------##
## Handler for the download for RDS
##----------------------------------------------------------------------------##

output$download_marker_table <- downloadHandler(
  filename = function() {
    paste("markers", ".csv", sep = "")
  },
  content = function(file) {
    table = values$marker_gene
    write.csv(table, file, row.names=TRUE)
  }
)