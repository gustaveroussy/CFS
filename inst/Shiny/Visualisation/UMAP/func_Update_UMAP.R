##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

current_plot_umap <- reactive({
  
  if(!(input$UMAP_visualisation_comput)){
    return(NULL)
  }
  
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
  
  if(input$image_display_UMAP){
    TissueCoordinates = TissueCoordinates()
    meta.data = values$UMAP@meta.data[(rownames(values$UMAP@meta.data) %in% rownames(TissueCoordinates)),]
    cell.embeddings <- values$UMAP@reductions$ica@cell.embeddings[(rownames(values$UMAP@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates)),]
    annotation = values$UMAP@misc$annotation
    cell.embeddings.umap = values$UMAP[["umap"]]@cell.embeddings[(rownames(values$UMAP[["umap"]]@cell.embeddings) %in% rownames(TissueCoordinates)),]
  } else {
    TissueCoordinates = TissueCoordinates()
    meta.data = values$UMAP@meta.data
    cell.embeddings <- values$UMAP@reductions$ica@cell.embeddings
    annotation = values$UMAP@misc$annotation
    cell.embeddings.umap = values$UMAP[["umap"]]@cell.embeddings
  }


  fig <- plot_ly(type = 'scatter',
                 mode='markers',
                 source = "G"
  )
  
  if (input$Plot_analysis_type == "UMAP"){
    if (input$Plot_display_type == "seurat_clusters"){
      
      ################# utiliser la fonction , split = de plotly et pas une boucle
      for (i in as.numeric(as.vector(unique(meta.data[["seurat_clusters"]])))[order(as.numeric(as.vector(unique(meta.data[["seurat_clusters"]]))))]){
        if(length(which(meta.data[["seurat_clusters"]]==i)) == 1){
          table = t(as.data.frame(cell.embeddings[which(meta.data[["seurat_clusters"]]==i),]))
          rownames(table) = rownames(meta.data[which(meta.data[["seurat_clusters"]]==i),])
        } else {
          table = cell.embeddings[which(meta.data[["seurat_clusters"]]==i),]
        }
        
        list_cells_ICs = c()
        for(k in 1:length(rownames(table))){
          top_10_ICs = head(colnames(table)[order(table[rownames(table)[k], ],decreasing = TRUE)],10)
          final_vector = c('Cluster : ', i,'\nTop 10 ICs :\n')
          for (j in top_10_ICs){
            if(input$full_annotation_UMAP  & (j %in% rownames(annotation))){
              final_vector = c(final_vector,j,' : ',round(table[rownames(table)[k],j],4),' : ',annotation[j,'Type'],' : ',annotation[j,'Annotation'],'\n')
            } else {
              final_vector = c(final_vector,j,' : ',round(table[rownames(table)[k],j],4),'\n')
            }
            final_vector = paste(final_vector,collapse = "")
          }
          list_cells_ICs = c(list_cells_ICs,final_vector)
        }
        
        r = length(as.vector(cell.embeddings.umap[which(meta.data[["seurat_clusters"]]==i),2]))
        
        datatable <- data.frame("x" = as.vector(cell.embeddings.umap[which(meta.data[["seurat_clusters"]]==i),1]),
                                "y" = as.vector(cell.embeddings.umap[which(meta.data[["seurat_clusters"]]==i),2]),
                                "cell_name" = rownames(meta.data)[which(meta.data[["seurat_clusters"]]==i)],
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
            text = datatable$t,
            customdata = datatable$cell_name,
            showlegend = T,
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "%{text}",
                                   "<extra></extra>")
          )
      }
    } else if (input$Plot_display_type == "gene") {
      if(input$image_display_UMAP){
        scale.data = values$UMAP@assays$SCT@scale.data[,(colnames(values$UMAP@assays$SCT@scale.data) %in% rownames(TissueCoordinates))]
      } else {
        scale.data = values$UMAP@assays$SCT@scale.data
      }
      if(input$select_color_visualisation_projection %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
        fig <- fig %>%
          add_trace(
            x = cell.embeddings.umap[,1],
            y = cell.embeddings.umap[,2],
            marker = list(
              color = scale.data[input$gene_UMAP_choice,],
              colorscale = input$select_color_visualisation_projection,
              reversescale=input$invert_color_visualisation_UMAP,
              size = input$Plot_scatter_size_UMAP,
              showscale = T,
              cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
              opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{(scale.data[input$gene_UMAP_choice,])/max(scale.data[input$gene_UMAP_choice,])*input$transparency_visual_spatial_range}
            ),
            showlegend = T,
            text = scale.data[input$gene_UMAP_choice,],
            customdata = rownames(meta.data),
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Value : %{text}",
                                   "<extra></extra>")
          )
      } else {
        #prepare colorscales
        l = list()
        se = seq(0, 1, (1/(nrow(TissueCoordinates)-1)))
        col = viridis_pal(option = input$select_color_visualisation_projection)(nrow(TissueCoordinates))
        for(i in 1:length(se)){
          l[[i]] = c(se[i],col[i])
        }
        
        fig <- fig %>%
          add_trace(
            x = cell.embeddings.umap[,1],
            y = cell.embeddings.umap[,2],
            marker = list(
              color = scale.data[input$gene_UMAP_choice,],
              colorscale = l,
              reversescale=input$invert_color_visualisation_UMAP,
              size = input$Plot_scatter_size_UMAP,
              showscale = T,
              cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
              opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{(scale.data[input$gene_UMAP_choice,])/max(scale.data[input$gene_UMAP_choice,])*input$transparency_visual_spatial_range}
            ),
            showlegend = T,
            text = scale.data[input$gene_UMAP_choice,],
            customdata = rownames(meta.data),
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Value : %{text}",
                                   "<extra></extra>")
          )
      }
      
      fig <- fig %>% layout(showlegend = F)
      
    } else if (input$Plot_display_type == "IC") {
      if(input$select_color_visualisation_projection %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
        fig <- fig %>%
          add_trace(
            x = cell.embeddings.umap[,1],
            y = cell.embeddings.umap[,2],
            marker = list(
              color = cell.embeddings[,input$IC_UMAP_choice],
              colorscale = input$select_color_visualisation_projection,
              reversescale=input$invert_color_visualisation_UMAP,
              size = input$Plot_scatter_size_UMAP,
              showscale = T,
              cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
              opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{(cell.embeddings[,input$IC_UMAP_choice])/max(cell.embeddings[,input$IC_UMAP_choice])*input$transparency_visual_spatial_range}
            ),
            showlegend = T,
            text = cell.embeddings[,input$IC_UMAP_choice],
            customdata = rownames(meta.data),
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Value : %{text}",
                                   "<extra></extra>")
          )
      } else {
        #prepare colorscales
        l = list()
        se = seq(0, 1, (1/(nrow(TissueCoordinates)-1)))
        col = viridis_pal(option = input$select_color_visualisation_projection)(nrow(TissueCoordinates))
        for(i in 1:length(se)){
          l[[i]] = c(se[i],col[i])
        }
        
        fig <- fig %>%
          add_trace(
            x = cell.embeddings.umap[,1],
            y = cell.embeddings.umap[,2],
            marker = list(
              color = cell.embeddings[,input$IC_UMAP_choice],
              colorscale = l,
              reversescale=input$invert_color_visualisation_UMAP,
              size = input$Plot_scatter_size_UMAP,
              showscale = T,
              cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
              opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{(cell.embeddings[,input$IC_UMAP_choice])/max(cell.embeddings[,input$IC_UMAP_choice])*input$transparency_visual_spatial_range}
            ),
            showlegend = T,
            text = cell.embeddings[,input$IC_UMAP_choice],
            customdata = rownames(meta.data),
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Value : %{text}",
                                   "<extra></extra>")
          )
      }
      
      fig <- fig %>% layout(showlegend = F)
      
    } else {
      if(typeof(meta.data[[input$Plot_display_type]]) == "double" | grepl('nCount_|nFeature_|percent_', input$Plot_display_type)){
        
        if(input$select_color_visualisation_projection %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
          fig <- fig %>%
            add_trace(
              x = cell.embeddings.umap[,1],
              y = cell.embeddings.umap[,2],
              marker = list(
                color = meta.data[[input$Plot_display_type]],
                colorscale = input$select_color_visualisation_projection,
                reversescale=input$invert_color_visualisation_UMAP,
                size = input$Plot_scatter_size_UMAP,
                showscale = T,
                cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
                opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{(meta.data[[input$Plot_display_type]])/max(meta.data[[input$Plot_display_type]])*input$transparency_visual_spatial_range}
              ),
              showlegend = T,
              text = meta.data[[input$Plot_display_type]],
              customdata = rownames(meta.data),
              hovertemplate = paste0("Cell : %{customdata}<br>",
                                     "Value : %{text}",
                                     "<extra></extra>")
            )
        } else {
          #prepare colorscales
          l = list()
          se = seq(0, 1, (1/(nrow(TissueCoordinates)-1)))
          col = viridis_pal(option = input$select_color_visualisation_projection)(nrow(TissueCoordinates))
          for(i in 1:length(se)){
            l[[i]] = c(se[i],col[i])
          }
          
          fig <- fig %>%
            add_trace(
              x = cell.embeddings.umap[,1],
              y = cell.embeddings.umap[,2],
              marker = list(
                color = meta.data[[input$Plot_display_type]],
                colorscale = l,
                reversescale=input$invert_color_visualisation_UMAP,
                size = input$Plot_scatter_size_UMAP,
                showscale = T,
                cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
                opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{(meta.data[[input$Plot_display_type]])/max(meta.data[[input$Plot_display_type]])*input$transparency_visual_spatial_range}
              ),
              showlegend = T,
              text = meta.data[[input$Plot_display_type]],
              customdata = rownames(meta.data),
              hovertemplate = paste0("Cell : %{customdata}<br>",
                                     "Value : %{text}",
                                     "<extra></extra>")
            )
        }
        
        fig <- fig %>% layout(showlegend = F)
        
      } else {
        c = 1
        for (i in unique(meta.data[[input$Plot_display_type]])){
          fig <- fig %>%
            add_trace(
              x = cell.embeddings.umap[which(meta.data[[input$Plot_display_type]]==i),1],
              y = cell.embeddings.umap[which(meta.data[[input$Plot_display_type]]==i),2],
              name = i,
              marker = list(
                color = palette()[c],
                size = input$Plot_scatter_size_UMAP
              ),
              showlegend = T,
              text = i,
              customdata = rownames(meta.data)[which(meta.data[[input$Plot_display_type]]==i)],
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