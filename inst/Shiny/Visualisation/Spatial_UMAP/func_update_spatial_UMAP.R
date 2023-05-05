##----------------------------------------------------------------------------##
## Spatial clustering
##----------------------------------------------------------------------------##

current_plot_spatial <- reactive({
  
  data <- values$UMAP
  
  fig <- plot_ly(type = 'scatter',
                 mode='markers',
                 source = "C"
  )
  
  # if (!is.null(values$HD_image_2)){
  #   fig <- fig %>% add_trace(type="image", source = values$HD_image_2, hoverinfo = 'skip')
  # }
  if (!is.null(values$HD_image)) {
    fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
  } else {
    if(!is.null(values$low_image)){
      fig <- fig %>% add_trace(type="image", source = values$low_image, hoverinfo = 'skip')
    }
  }
  
  if (input$Plot_analysis_type == "UMAP"){
    if (input$Plot_display_type == "Clustering"){
      for (i in 0:length(summary(data@meta.data[["seurat_clusters"]]))-1){
        fig <- fig %>%
          add_trace(
            x = TissueCoordinates()[,"imagecol"][which(data@meta.data[["seurat_clusters"]]==i)],
            y = TissueCoordinates()[,"imagerow"][which(data@meta.data[["seurat_clusters"]]==i)],
            name = i,
            marker = list(
              color = palette()[i+1],
              size = 10
            ),
            showlegend = T,
            text = i,
            customdata = rownames(data@meta.data)[which(data@meta.data[["seurat_clusters"]]==i)],
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                  "Cluster : %{text}",
                                  "<extra></extra>")
          )
    } 
  } else if (input$Plot_display_type == "Ploïdie"){
      c = 1
      for (i in unique(data@meta.data[["aneuploid"]])){
        fig <- fig %>%
          add_trace(
            x = TissueCoordinates()[,"imagecol"][which(data@meta.data[["aneuploid"]]==i)],
            y = TissueCoordinates()[,"imagerow"][which(data@meta.data[["aneuploid"]]==i)],
            name = i,
            marker = list(
              color = palette()[c],
              size = 10
            ),
            showlegend = T,
            text = data@meta.data[["aneuploid"]][which(data@meta.data[["aneuploid"]]==i)],
            customdata = rownames(data@meta.data)[which(data@meta.data[["aneuploid"]]==i)],
            hovertemplate = paste("Cell : %{customdata}<br>",
                                  "Ploïdie : %{text}<br>",
                                  "<extra></extra>")
          )
        c = c + 1
      }
    }
  }
  
  fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                 yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                 autosize = TRUE
  )
  
  fig <- fig %>% event_register('plotly_click')
  fig <- fig %>% event_register('plotly_brushed')
  
  return(fig)
})

