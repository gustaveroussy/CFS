##----------------------------------------------------------------------------##
## Handler for the download for RDS
##----------------------------------------------------------------------------##

output$download_distance_tables <- downloadHandler(
  filename = function() {
    paste("annotation", ".csv", sep = "")
  },
  content = function(file) {
    table = values$distances[[input$choose_distances_to_determine]][[input$choose_sample_for_distances]][[input$choose_method_for_distances]][["df"]]
    write.csv(table, file, row.names=FALSE)
  }
)