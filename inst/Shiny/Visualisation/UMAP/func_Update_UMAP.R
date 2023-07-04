##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

current_plot_umap <- reactive({
  type = NULL
  
  if (!is.null(input$Plot_display_IC_choice)) {
    if (!is.null(input$Plot_display_type_UMAP_choice)){
      if(length(input$Plot_display_type_UMAP_choice) != 1){
        for (n_cell_type in input$Plot_display_type_UMAP_choice) {
          if(is.null(type)) {
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
  } else if (!is.null(input$Plot_display_type_UMAP_choice)){
    if(length(input$Plot_display_type_UMAP_choice) != 1){
      name = paste(input$Plot_display_type_UMAP_choice,collapse = ",")
      for (n_cell_type in input$Plot_display_type_UMAP_choice) {
        if(is.null(type)) {
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
    if(is.null(values$UMAP)){
      shinyalert("UMAP error", "No UMAP can be calculated", type = "error")
    }
  }
  
  req(values$UMAP)
  req(values$UMAP@reductions$umap)

  fig <- plot_ly(type = 'scatter',
                 mode='markers',
                 source = "G"
  )
  
  if (input$Plot_analysis_type == "UMAP"){
    if (input$Plot_display_type == "seurat_clusters"){
      for (i in 1:length(summary(values$UMAP@meta.data[["seurat_clusters"]]))-1){
        
        table = values$UMAP@reductions$ica@cell.embeddings[which(values$UMAP@meta.data[["seurat_clusters"]]==i),]
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
        
        r = length(as.vector(values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[["seurat_clusters"]]==i),2]))
        
        datatable <- data.frame("x" = as.vector(values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[["seurat_clusters"]]==i),1]),
                                "y" = as.vector(values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[["seurat_clusters"]]==i),2]),
                                "cell_name" = rownames(values$UMAP@meta.data)[which(values$UMAP@meta.data[["seurat_clusters"]]==i)],
                                "t" = list_cells_ICs)
        
        fig <- fig %>%
          add_trace(data = datatable,
            x = ~x,
            y = ~y,
            name = i,
            marker = list(
              color = palette()[i+1],
              size = input$Plot_scatter_size_UMAP
            ),
            text = datatable$cell_name,
            customdata = datatable$t,
            showlegend = T,
            hovertemplate = paste0("Cell : %{text}<br>",
                                   "%{customdata}",
                                   "<extra></extra>")
          )
      }
    } else if (input$Plot_display_type == "gene") {
      
      fig <- fig %>%
        add_trace(
          x = values$UMAP[["umap"]]@cell.embeddings[,1],
          y = values$UMAP[["umap"]]@cell.embeddings[,2],
          marker = list(
            color = values$UMAP@assays$SCT@scale.data[input$gene_UMAP_choice,],
            colorscale = input$select_color_visualisation_projection,
            size = input$Plot_scatter_size_UMAP
          ),
          showlegend = T,
          text = values$UMAP@assays$SCT@scale.data[input$gene_UMAP_choice,],
          customdata = rownames(values$UMAP@meta.data),
          hovertemplate = paste0("Cell : %{customdata}<br>",
                                 "Value : %{text}",
                                 "<extra></extra>")
        )
      
    } else if (input$Plot_display_type == "IC") {
      
      fig <- fig %>%
        add_trace(
          x = values$UMAP[["umap"]]@cell.embeddings[,1],
          y = values$UMAP[["umap"]]@cell.embeddings[,2],
          marker = list(
            color = values$UMAP@reductions$ica@cell.embeddings[,input$IC_UMAP_choice],
            colorscale = input$select_color_visualisation_projection,
            size = input$Plot_scatter_size_UMAP
          ),
          showlegend = T,
          text = values$UMAP@reductions$ica@cell.embeddings[,input$IC_UMAP_choice],
          customdata = rownames(values$UMAP@meta.data),
          hovertemplate = paste0("Cell : %{customdata}<br>",
                                 "Value : %{text}",
                                 "<extra></extra>")
        )
      
    } else {
      if(typeof(values$UMAP@meta.data[[input$Plot_display_type]]) == "double" | grepl('nCount_|nFeature_|percent_', input$Plot_display_type)){
        fig <- fig %>%
          add_trace(
            x = values$UMAP[["umap"]]@cell.embeddings[,1],
            y = values$UMAP[["umap"]]@cell.embeddings[,2],
            marker = list(
              color = values$UMAP@meta.data[[input$Plot_display_type]],
              colorscale = input$select_color_visualisation_projection,
              size = input$Plot_scatter_size_UMAP
            ),
            showlegend = T,
            text = values$UMAP@meta.data[[input$Plot_display_type]],
            customdata = rownames(values$UMAP@meta.data),
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Value : %{text}",
                                   "<extra></extra>")
          )
      } else {
        c = 1
        for (i in unique(values$UMAP@meta.data[[input$Plot_display_type]])){
          fig <- fig %>%
            add_trace(
              x = values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[[input$Plot_display_type]]==i),1],
              y = values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[[input$Plot_display_type]]==i),2],
              name = i,
              marker = list(
                color = palette()[c],
                size = input$Plot_scatter_size_UMAP
              ),
              showlegend = T,
              text = i,
              customdata = rownames(values$UMAP@meta.data)[which(values$UMAP@meta.data[[input$Plot_display_type]]==i)],
              hovertemplate = paste0("Cell : %{customdata}<br>",
                                     "Cluster : %{text}",
                                     "<extra></extra>")
            )
          c = c+1
          }
        }
    }
    
    fig <- fig %>% layout(xaxis = list(showgrid = input$show_grid_scatter_pie, zeroline=input$show_grid_scatter_pie,
                                       visible = input$show_grid_scatter_pie),
                          yaxis = list(showgrid = input$show_grid_scatter_pie, zeroline=input$show_grid_scatter_pie,
                                       visible = input$show_grid_scatter_pie))
    
    fig <- fig %>% event_register('plotly_selected')
    
    }
  return(fig)
})