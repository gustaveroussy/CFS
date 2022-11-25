##----------------------------------------------------------------------------##
## trajectory
##----------------------------------------------------------------------------##

current_plot_trajectory <- reactive({

  data <- values$data
  req(data@reductions[["umap"]])
  
  data <- Spatial_pseudotime(data,input$Plot_display_IC_choice)
  
  if (input$trajectory_dimension_type == "2D"){
    
    fig <- plot_ly(type = 'scatter',
                   mode='markers'
    )
    
    if (input$trajectory_color_by == "Clustering"){
      for (i in 0:length(summary(data@meta.data[["seurat_clusters"]]))-1){
        
        fig <- fig %>% add_trace(
            x = data@misc$dpt@dm@eigenvectors[which(data$seurat_clusters[which(data@meta.data$aneuploid == "aneuploid")] == i),c(1)],
            y = data@misc$dpt@dm@eigenvectors[which(data$seurat_clusters[which(data@meta.data$aneuploid == "aneuploid")] == i),c(2)],
            name = i,
            marker = list(
              color = palette()[i+1],
              size = 10
            ),
            showlegend = T,
            hovertemplate = paste0("DC1 : %{x}<br>",
                                   "DC2 : %{y}",
                                   "<extra></extra>")
          )
      }
    } else {
    }
  } else if (input$trajectory_dimension_type == "3D") {
    
    fig <- plot_ly(type = 'scatter3d', mode = "markers")
    
    if (input$trajectory_color_by == "Clustering"){
      for (i in 0:length(summary(data@meta.data[["seurat_clusters"]]))-1){
        
        fig <- fig %>% add_trace(
          x = data@misc$dpt@dm@eigenvectors[which(data$seurat_clusters[which(data@meta.data$aneuploid == "aneuploid")] == i),c(1)],
          y = data@misc$dpt@dm@eigenvectors[which(data$seurat_clusters[which(data@meta.data$aneuploid == "aneuploid")] == i),c(2)],
          z = data@misc$dpt@dm@eigenvectors[which(data$seurat_clusters[which(data@meta.data$aneuploid == "aneuploid")] == i),c(3)],
          name = i,
          marker = list(
            color = palette()[i+1],
            size = 2
          ),
          showlegend = T,
          hovertemplate = paste0("DC1 : %{x}<br>",
                                "DC2 : %{y}<br>",
                                "DC3 : %{z}",
                                "<extra></extra>")
        )
      }
      
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'DC1'), yaxis = list(title = 'DC2'), zaxis = list(title = 'DC3')))
      
    } else {
    }
  }
  return(fig)
})