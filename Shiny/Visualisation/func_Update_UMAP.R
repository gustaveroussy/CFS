##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

current_plot_umap <- reactive({
  
  values$data <- Clustering_UMAP()
  
  req(values$data@reductions[["umap"]])
  
  
  fig <- plot_ly(type = 'scatter',
                 mode='markers',
                 source = "A"
  )
  
  if (input$Plot_analysis_type == "UMAP"){
    if (input$Plot_display_type == "Clustering"){
      for (i in 0:length(summary(values$data@meta.data[["seurat_clusters"]]))-1){
        fig <- fig %>%
          add_trace(
            x = values$data[["umap"]]@cell.embeddings[which(values$data@meta.data[["seurat_clusters"]]==i),1],
            y = values$data[["umap"]]@cell.embeddings[which(values$data@meta.data[["seurat_clusters"]]==i),2],
            name = i,
            marker = list(
              color = palette()[i+1],
              size = 10
            ),
            showlegend = T,
            text = i,
            customdata = rownames(values$data@meta.data)[which(values$data@meta.data[["seurat_clusters"]]==i)],
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Cluster : %{text}",
                                   "<extra></extra>")
          )
      }
    } else if (input$Plot_display_type == "Ploïdie"){
      c = 1
      for (i in unique(values$data@meta.data[["aneuploid"]])){
        fig <- fig %>%
          add_trace(
            x = values$data[["umap"]]@cell.embeddings[which(values$data@meta.data[["aneuploid"]]==i),1],
            y = values$data[["umap"]]@cell.embeddings[which(values$data@meta.data[["aneuploid"]]==i),2],
            name = i,
            marker = list(
              color = palette()[c],
              size = 10
            ),
            showlegend = T,
            text = i,
            customdata = rownames(values$data@meta.data)[which(values$data@meta.data[["aneuploid"]]==i)],
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Cluster : %{text}",
                                   "<extra></extra>")
          )
        c = c+1
        }
      }
    
    fig <- fig %>% event_register('plotly_selected')
    
    } else if (input$Plot_analysis_type == "sub UMAP") { # En chantier : voir si on fait ça avec un traitement additionnel ou directement dans shiny
      
      values$data <- sub_UMAP_plot()
      
      table <- values$data[["umap"]]@cell.embeddings
      table <- table[(row.names(table) %in% selected_cells_plot()$customdata),]
      
      metadata <- values$data@meta.data
      metadata <- metadata[(row.names(metadata) %in% selected_cells_plot()$customdata),]
      
      if (input$Plot_display_type == "Clustering"){
        for (i in 0:length(summary(metadata[["seurat_clusters"]]))-1){
          fig <- fig %>%
            add_trace(
              x = table[which(metadata[["seurat_clusters"]]==i),1],
              y = table[which(metadata[["seurat_clusters"]]==i),2],
              name = i,
              marker = list(
                color = palette()[i+1],
                size = 10
              ),
              showlegend = T,
              text = i,
              customdata = rownames(metadata)[which(metadata[["seurat_clusters"]]==i)],
              hovertemplate = paste0("Cell : %{customdata}<br>",
                                     "Cluster : %{text}",
                                     "<extra></extra>")
            )
        }
      } else if (input$Plot_display_type == "Ploïdie") {
        c = 1
        for (i in unique(metadata[["aneuploid"]])){
          fig <- fig %>%
            add_trace(
              x = table[which(metadata[["aneuploid"]]==i),1],
              y = table[which(metadata[["aneuploid"]]==i),2],
              name = i,
              marker = list(
                color = palette()[c],
                size = 10
              ),
              showlegend = T,
              text = i,
              customdata = rownames(metadata)[which(metadata[["aneuploid"]]==i)],
              hovertemplate = paste0("Cell : %{customdata}<br>",
                                     "Cluster : %{text}",
                                     "<extra></extra>")
            )
          c = c+1
        }
      }
    }
  return(fig)
})