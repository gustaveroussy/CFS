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
      for (i in 1:length(summary(data@meta.data[["seurat_clusters"]]))-1){
        
        table = data@reductions$ica@cell.embeddings[which(data@meta.data[["seurat_clusters"]]==i),]
        list_cells_ICs = c()
        for(k in 1:length(rownames(table))){
          top_10_ICs = head(colnames(table)[order(table[rownames(table)[k], ],decreasing = TRUE)],10)
          final_vector = c('Cluster : ', i,'\nTop 10 ICs :\n')
          for (j in top_10_ICs){
            final_vector = c(final_vector,j,' : ',table[rownames(table)[k],j],'\n')
            final_vector = paste(final_vector,collapse = "")
          }
          list_cells_ICs = c(list_cells_ICs,final_vector)
        }
        
        r = length(as.vector(TissueCoordinates()[,"imagecol"][which(data@meta.data[["seurat_clusters"]]==i)]))
        
        datatable <- data.frame("x" = as.vector(TissueCoordinates()[,"imagecol"][which(data@meta.data[["seurat_clusters"]]==i)]),
                                "y" = as.vector(TissueCoordinates()[,"imagerow"][which(data@meta.data[["seurat_clusters"]]==i)]),
                                "cell_name" = as.vector(rownames(data@meta.data)[which(data@meta.data[["seurat_clusters"]]==i)]),
                                "t" = list_cells_ICs)
        
        fig <- fig %>%
          add_trace(data = datatable,
            x = ~x,
            y = ~y,
            name = i,
            marker = list(
              color = palette()[i+1],
              size = input$Plot_scatter_size_spatial
            ),
            showlegend = T,
            text = datatable$cell_name,#i,
            customdata = datatable$t,
            hovertemplate = paste0("Cell : %{text}<br>",
                                   "%{customdata}",
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
          size = input$Plot_scatter_size_spatial,
          showscale = T
        ),
        showlegend = T,
        text = values$UMAP@assays$SCT@scale.data[input$gene_UMAP_choice,],
        customdata = rownames(values$UMAP@meta.data),
        hovertemplate = paste0("Cell : %{customdata}<br>",
                               "Value : %{text}",
                               "<extra></extra>")
      )
    
    fig <- fig %>% layout(showlegend = F)
      
    }  else if (input$Plot_display_type == "IC") {
      
      fig <- fig %>%
        add_trace(
          x = TissueCoordinates()[,"imagecol"],
          y = TissueCoordinates()[,"imagerow"],
          marker = list(
            color = values$UMAP@reductions$ica@cell.embeddings[,input$IC_UMAP_choice],
            colorscale = input$select_color_visualisation_projection,
            size = input$Plot_scatter_size_spatial,
            showscale = T
          ),
          showlegend = T,
          text = values$UMAP@reductions$ica@cell.embeddings[,input$IC_UMAP_choice],
          customdata = rownames(values$UMAP@meta.data),
          hovertemplate = paste0("Cell : %{customdata}<br>",
                                 "Value : %{text}",
                                 "<extra></extra>")
        )
      
      fig <- fig %>% layout(showlegend = F)
      
    } else {
    if(typeof(values$UMAP@meta.data[[input$Plot_display_type]]) == "double" | grepl('nCount_|nFeature_|percent_', input$Plot_display_type)){
      fig <- fig %>%
        add_trace(
          x = TissueCoordinates()[,"imagecol"],
          y = TissueCoordinates()[,"imagerow"],
          marker = list(
            color = values$UMAP@meta.data[[input$Plot_display_type]],
            colorscale = input$select_color_visualisation_projection,
            size = input$Plot_scatter_size_spatial,
            showscale = T
          ),
          showlegend = T,
          text = values$UMAP@meta.data[[input$Plot_display_type]],
          customdata = rownames(data@meta.data),
          hovertemplate = paste("Cell : %{customdata}<br>",
                                "Value : %{text}<br>",
                                "<extra></extra>")
        )
      
      fig <- fig %>% layout(showlegend = F)
      
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

