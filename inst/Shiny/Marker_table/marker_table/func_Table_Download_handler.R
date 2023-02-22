##----------------------------------------------------------------------------##
## Handler for the download for RDS
##----------------------------------------------------------------------------##

output$download_marker_table <- downloadHandler(
  filename = function() {
    paste("markers", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(Prepare_table_marker(table = values$marker_gene[[(as.integer(input$marker_cluster_choice)+1)]]),
              file, row.names=TRUE)
  }
)