##----------------------------------------------------------------------------##
## Handler for the download for RDS
##----------------------------------------------------------------------------##

output$download_RDS <- downloadHandler(
  filename = function() {
    paste("data", ".RDS", sep = "")
  },
  content = function(file) {
    data <- Clustering_UMAP()
    if (!is.null(input$Ic_list)) {
      data@misc$IC <- input$Ic_list
    } else if (!is.null(input$Plot_display_IC_choice)) {
      data@misc$IC <- input$Plot_display_IC_choice
    }
    saveRDS(data, file)
  }
)