##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

current_plot_umap <- reactive({
  if (!is.null(input$Plot_display_IC_choice)) {
    if (!is.null(input$Plot_display_type_UMAP_choice)){
      if(length(input$Plot_display_type_UMAP_choice) != 1){
        for (n_cell_type in 1:length(input$Plot_display_type_UMAP_choice)) {
          if(n_cell_type == 1) {
            type = values$annotation_for_output[[n_cell_type]]
          } else {
            type = append(type, values$annotation_for_output[[n_cell_type]])
          }
        }
        type = unique(type)
      } else {
        type = values$annotation_for_output[[input$Plot_display_type_UMAP_choice]]
      }
      values$UMAP=Cluster_ICA(data=values$data,ICs=as.integer(gsub('[IC_]','',unique(c(type,input$Plot_display_IC_choice)))),res=input$Plot_resolution, spread = input$Plot_spread)
    } else {
      values$UMAP=Cluster_ICA(data=values$data,ICs=as.integer(gsub('[IC_]','',input$Plot_display_IC_choice)),res=input$Plot_resolution, spread = input$Plot_spread)
    }
  } else {
    if (!is.null(input$Plot_display_type_UMAP_choice)){
      if(length(input$Plot_display_type_UMAP_choice) != 1){
        name = paste(input$Plot_display_type_UMAP_choice,collapse = ",")
        for (n_cell_type in 1:length(input$Plot_display_type_UMAP_choice)) {
          if(n_cell_type == 1) {
            type = values$annotation_for_output[[n_cell_type]]
          } else {
            type = append(type, values$annotation_for_output[[n_cell_type]])
          }
        }
        type = unique(type)
      } else {
        type = values$annotation_for_output[[input$Plot_display_type_UMAP_choice]]
      }
      values$UMAP=Cluster_ICA(data=values$data,ICs=as.integer(gsub('[IC_]','',type)),res=input$Plot_resolution, spread = input$Plot_spread)
    } else {
      values$UMAP = values$data
    }
  }
  
#  if ("aneuploid" %in% colnames(values$UMAP@meta.data)) {
#    values$UMAP@meta.data$aneuploid <- as.character(values$UMAP@meta.data$aneuploid)
#    values$UMAP@meta.data$aneuploid[which(is.na(values$UMAP@meta.data$aneuploid))] = "unknown"
#    values$UMAP@meta.data$aneuploid <- as.factor(values$UMAP@meta.data$aneuploid)
#    
#    values$UMAP <- Spatial_pseudotime(values$UMAP)
#  }
  
  req(values$UMAP)

  fig <- plot_ly(type = 'scatter',
                 mode='markers',
                 source = "A"
  )
  
  if (input$Plot_analysis_type == "UMAP"){
    if (input$Plot_display_type == "Clustering"){
      for (i in 0:length(summary(values$UMAP@meta.data[["seurat_clusters"]]))-1){
        fig <- fig %>%
          add_trace(
            x = values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[["seurat_clusters"]]==i),1],
            y = values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[["seurat_clusters"]]==i),2],
            name = i,
            marker = list(
              color = palette()[i+1],
              size = 10
            ),
            showlegend = T,
            text = i,
            customdata = rownames(values$UMAP@meta.data)[which(values$UMAP@meta.data[["seurat_clusters"]]==i)],
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Cluster : %{text}",
                                   "<extra></extra>")
          )
      }
    } else if (input$Plot_display_type == "Ploïdie"){
      c = 1
      for (i in unique(values$UMAP@meta.data[["aneuploid"]])){
        fig <- fig %>%
          add_trace(
            x = values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[["aneuploid"]]==i),1],
            y = values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[["aneuploid"]]==i),2],
            name = i,
            marker = list(
              color = palette()[c],
              size = 10
            ),
            showlegend = T,
            text = i,
            customdata = rownames(values$UMAP@meta.data)[which(values$UMAP@meta.data[["aneuploid"]]==i)],
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Cluster : %{text}",
                                   "<extra></extra>")
          )
        c = c+1
        }
      }
    
    fig <- fig %>% event_register('plotly_selected')
    
    } else if (input$Plot_analysis_type == "sub UMAP") { # En chantier : voir si on fait ça avec un traitement additionnel ou directement dans shiny
      
      values$UMAP <- sub_UMAP_plot()
      
      table <- values$UMAP[["umap"]]@cell.embeddings
      table <- table[(row.names(table) %in% selected_cells_plot()$customdata),]
      
      metadata <- values$UMAP@meta.data
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