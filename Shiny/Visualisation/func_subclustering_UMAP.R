##----------------------------------------------------------------------------##
## sub-clustering
##----------------------------------------------------------------------------##

selected_cells_plot <- reactive({
  return(plotly::event_data(c("plotly_selected"), source = "A"))
})

sub_UMAP_plot <- reactive({
  
  data <- Clustering_UMAP()
  
  data <- data[,(colnames(data@assays$Spatial) %in% selected_cells_plot()$customdata)]
  
  return(data)
})