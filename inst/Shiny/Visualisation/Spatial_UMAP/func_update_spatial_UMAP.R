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
    if (input$Plot_display_type == "seurat_clusters"){
      for (i in 0:length(summary(data@meta.data[["seurat_clusters"]]))-1){
        fig <- fig %>%
          add_trace(
            x = TissueCoordinates()[,"imagecol"][which(data@meta.data[["seurat_clusters"]]==i)],
            y = TissueCoordinates()[,"imagerow"][which(data@meta.data[["seurat_clusters"]]==i)],
            name = i,
            marker = list(
              color = palette()[i+1],
              size = input$Plot_scatter_size_spatial
            ),
            showlegend = T,
            text = i,
            customdata = rownames(data@meta.data)[which(data@meta.data[["seurat_clusters"]]==i)],
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                  "Cluster : %{text}",
                                  "<extra></extra>")
          )
    } 
  } else if (input$Plot_display_type == "gene") {
      
    fig <- fig %>%
      add_trace(
        x = TissueCoordinates()[,"imagecol"],
        y = TissueCoordinates()[,"imagerow"],
        marker = list(
          color = values$UMAP@assays$SCT@scale.data[input$gene_UMAP_choice,],
          colorscale = input$select_color_visualisation_projection,
          size = input$Plot_scatter_size_spatial
        ),
        showlegend = T,
        text = values$UMAP@assays$SCT@scale.data[input$gene_UMAP_choice,],
        customdata = rownames(values$UMAP@meta.data),
        hovertemplate = paste0("Cell : %{customdata}<br>",
                               "Value : %{text}",
                               "<extra></extra>")
      )
      
    } else {
    if(typeof(values$UMAP@meta.data[[input$Plot_display_type]]) == "double" | grepl('nCount_|nFeature_|percent_', input$Plot_display_type)){
      fig <- fig %>%
        add_trace(
          x = TissueCoordinates()[,"imagecol"],
          y = TissueCoordinates()[,"imagerow"],
          marker = list(
            color = values$UMAP@meta.data[[input$Plot_display_type]],
            colorscale = input$select_color_visualisation_projection,
            size = input$Plot_scatter_size_spatial
          ),
          showlegend = T,
          text = values$UMAP@meta.data[[input$Plot_display_type]],
          customdata = rownames(data@meta.data),
          hovertemplate = paste("Cell : %{customdata}<br>",
                                "Value : %{text}<br>",
                                "<extra></extra>")
        )
    } else {
        c = 1
        for (i in unique(data@meta.data[[input$Plot_display_type]])){
          fig <- fig %>%
            add_trace(
              x = TissueCoordinates()[,"imagecol"][which(data@meta.data[[input$Plot_display_type]]==i)],
              y = TissueCoordinates()[,"imagerow"][which(data@meta.data[[input$Plot_display_type]]==i)],
              name = i,
              marker = list(
                color = palette()[c],
                size = input$Plot_scatter_size_spatial
              ),
              showlegend = T,
              text = data@meta.data[[input$Plot_display_type]][which(data@meta.data[[input$Plot_display_type]]==i)],
              customdata = rownames(data@meta.data)[which(data@meta.data[[input$Plot_display_type]]==i)],
              hovertemplate = paste("Cell : %{customdata}<br>",
                                    "Ploïdie : %{text}<br>",
                                    "<extra></extra>")
            )
          c = c + 1
        }
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

