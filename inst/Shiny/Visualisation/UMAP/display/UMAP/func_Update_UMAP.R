##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

current_plot_umap <- reactive({
  
  if(!(input$UMAP_visualisation_comput)){
    return(NULL)
  }

  req(values$data)
  req(values$data@reductions$umap)
  
  if(input$image_display_UMAP){
    TissueCoordinates = TissueCoordinates()
    meta.data = values$data@meta.data[(rownames(values$data@meta.data) %in% unlist(c(sapply(TissueCoordinates,rownames)))),]
    cell.embeddings <- values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% unlist(c(sapply(TissueCoordinates,rownames)))),]
    annotation = values$data@misc$annotation
    cell.embeddings.umap = values$data[[input$Visualisation_selected_dimred_to_display]]@cell.embeddings[(rownames(values$data[[input$Visualisation_selected_dimred_to_display]]@cell.embeddings) %in% unlist(c(sapply(TissueCoordinates,rownames)))),]
  } else {
    TissueCoordinates = TissueCoordinates()
    meta.data = values$data@meta.data
    cell.embeddings <- values$data@reductions$ica@cell.embeddings
    annotation = values$data@misc$annotation
    cell.embeddings.umap = values$data[[input$Visualisation_selected_dimred_to_display]]@cell.embeddings
  }
  
  if(input$interactive_display_visualisation_UMAP | ncol(cell.embeddings.umap) == 3){
  
    fig <- plot_ly(type = if(ncol(cell.embeddings.umap) == 2){'scatter'}else if(ncol(cell.embeddings.umap) == 3){"scatter3d"},
                   mode='markers',
                   source = "G"
    )
    
    if (input$Plot_analysis_display_type == "Dimentional reduction"){
      if (input$Plot_display_type == "seurat_clusters"){
        
        ################# utiliser la fonction , split = de plotly et pas une boucle
        for (i in as.numeric(as.vector(unique(meta.data[["seurat_clusters"]])))[order(as.numeric(as.vector(unique(meta.data[["seurat_clusters"]]))))]){
          ##### palette
          palette = values$palette
          while(length(as.numeric(as.vector(unique(meta.data[["seurat_clusters"]])))) > length(palette)){
            palette = c(palette,palette)
          }
          
          if(length(which(meta.data[["seurat_clusters"]]==i)) == 1){
            table = t(as.data.frame(cell.embeddings[which(meta.data[["seurat_clusters"]]==i),]))
            rownames(table) = rownames(meta.data[which(meta.data[["seurat_clusters"]]==i),])
          } else {
            table = cell.embeddings[which(meta.data[["seurat_clusters"]]==i),]
          }
          
          list_cells_ICs = c()
          for(k in 1:nrow(table)){
            if(input$full_annotation_UMAP == "IC" | input$full_annotation_UMAP == "Mean IC" | input$full_annotation_UMAP == "Full annotation" | input$full_annotation_UMAP == "Mean full annotation"){
              if(input$full_annotation_UMAP == "IC" | input$full_annotation_UMAP == "Full annotation"){
                  top_10_ICs = head(colnames(table)[order(table[rownames(table)[k], ],decreasing = TRUE)],10)
                } else {
                  cell_names = rownames(meta.data[meta.data$seurat_clusters == i,])
                  means = colMeans(cell.embeddings[cell_names,])
                  top_10_ICs = head(means[order(means,decreasing = TRUE)], n = 10L)
                }
              final_vector = c('Cluster : ', i,'\nTop 10 ICs :\n')
              for (j in 1:length(top_10_ICs)){
                if(input$full_annotation_UMAP == "Full annotation"  & (top_10_ICs[j] %in% rownames(annotation))){
                  final_vector = c(final_vector,top_10_ICs[j],' : ',round(table[rownames(table)[k],top_10_ICs[j]],4),' : ',annotation[top_10_ICs[j],'Type'],' : ',annotation[top_10_ICs[j],'Annotation'],'\n')
                } else if (input$full_annotation_UMAP == "IC" & (top_10_ICs[j] %in% rownames(annotation))) {
                  final_vector = c(final_vector,top_10_ICs[j],' : ',round(table[rownames(table)[k],top_10_ICs[j]],4),'\n')
                } else if (!is.null(names(top_10_ICs))){
                  if(input$full_annotation_UMAP == "Mean IC" & (names(top_10_ICs)[j] %in% rownames(annotation))) {
                    final_vector = c(final_vector,names(top_10_ICs)[j],' : ',round(top_10_ICs[j],4),'\n')
                  } else if (input$full_annotation_UMAP == "Mean full annotation" & (names(top_10_ICs)[j] %in% rownames(annotation))) {
                    final_vector = c(final_vector,names(top_10_ICs)[j],' : ',round(top_10_ICs[j],4),' : ',annotation[names(top_10_ICs)[j],'Type'],' : ',annotation[names(top_10_ICs)[j],'Annotation'],'\n')
                  }
                }
              }
            } else {
              final_vector = c('Cluster : ', i)
            }
            
            final_vector = paste(final_vector,collapse = "")
            
            list_cells_ICs = c(list_cells_ICs,final_vector)
          }
          
          r = length(as.vector(cell.embeddings.umap[which(meta.data[["seurat_clusters"]]==i),2]))
          
          
          if(ncol(cell.embeddings.umap) == 2){
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
                          color = palette[i+1],
                          size = input$Plot_scatter_size_UMAP
                        ),
                        text = datatable$t,
                        customdata = datatable$cell_name,
                        showlegend = T,
                        hovertemplate = paste0("Cell : %{customdata}<br>",
                                               "%{text}",
                                               "<extra></extra>")
              )
          }else if(ncol(cell.embeddings.umap) == 3){
            datatable <- data.frame("x" = as.vector(cell.embeddings.umap[which(meta.data[["seurat_clusters"]]==i),1]),
                                    "y" = as.vector(cell.embeddings.umap[which(meta.data[["seurat_clusters"]]==i),2]),
                                    "z" = as.vector(cell.embeddings.umap[which(meta.data[["seurat_clusters"]]==i),3]),
                                    "cell_name" = rownames(meta.data)[which(meta.data[["seurat_clusters"]]==i)],
                                    "t" = list_cells_ICs)
            
            fig <- fig %>%
              add_trace(data = datatable,
                        x = ~x,
                        y = ~y,
                        z = ~z,
                        name = i,
                        marker = list(
                          color = palette[i+1],
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
          
  
          
          
        }
      } else if (input$Plot_display_type == "gene") {
        if(input$image_display_UMAP){
          scale.data = values$data@assays$SCT@data[,(colnames(values$data@assays$SCT@data) %in% unlist(c(sapply(TissueCoordinates,rownames))))]
        } else {
          scale.data = values$data@assays$SCT@data
        }
        
        if(ncol(cell.embeddings.umap) == 2){
          fig <- fig %>%
            add_trace(
              x = cell.embeddings.umap[,1],
              y = cell.embeddings.umap[,2],
              marker = list(
                color = scale.data[input$gene_UMAP_choice,],
                colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
                reversescale=input$invert_color_visualisation_UMAP,
                size = input$Plot_scatter_size_UMAP,
                showscale = T,
                cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
                opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = scale.data[input$gene_UMAP_choice,], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)}
              ),
              showlegend = T,
              text = scale.data[input$gene_UMAP_choice,],
              customdata = rownames(meta.data),
              hovertemplate = paste0("Cell : %{customdata}<br>",
                                     "Value : %{text}",
                                     "<extra></extra>")
            )
        } else if (ncol(cell.embeddings.umap) == 3){
          fig <- fig %>%
            add_trace(
              x = cell.embeddings.umap[,1],
              y = cell.embeddings.umap[,2],
              z = cell.embeddings.umap[,3],
              marker = list(
                color = scale.data[input$gene_UMAP_choice,],
                colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
                reversescale=input$invert_color_visualisation_UMAP,
                size = input$Plot_scatter_size_UMAP,
                showscale = T,
                cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
                opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = scale.data[input$gene_UMAP_choice,], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)}
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
      if (ncol(cell.embeddings.umap) == 2){
        fig <- fig %>%
          add_trace(
            x = cell.embeddings.umap[,1],
            y = cell.embeddings.umap[,2],
            marker = list(
              color = cell.embeddings[,input$IC_UMAP_choice],
              colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
              reversescale=input$invert_color_visualisation_UMAP,
              size = input$Plot_scatter_size_UMAP,
              showscale = T,
              cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
              opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = cell.embeddings[,input$IC_UMAP_choice], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)}
            ),
            showlegend = T,
            text = cell.embeddings[,input$IC_UMAP_choice],
            customdata = rownames(meta.data),
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Value : %{text}",
                                   "<extra></extra>")
          )
      } else if (ncol(cell.embeddings.umap) == 3){
        fig <- fig %>%
          add_trace(
            x = cell.embeddings.umap[,1],
            y = cell.embeddings.umap[,2],
            z = cell.embeddings.umap[,3],
            marker = list(
              color = cell.embeddings[,input$IC_UMAP_choice],
              colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
              reversescale=input$invert_color_visualisation_UMAP,
              size = input$Plot_scatter_size_UMAP,
              showscale = T,
              cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
              opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = cell.embeddings[,input$IC_UMAP_choice], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)}
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
          if (ncol(cell.embeddings.umap) == 2){
            fig <- fig %>%
              add_trace(
                x = cell.embeddings.umap[,1],
                y = cell.embeddings.umap[,2],
                marker = list(
                  color = meta.data[[input$Plot_display_type]],
                  colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
                  reversescale=input$invert_color_visualisation_UMAP,
                  size = input$Plot_scatter_size_UMAP,
                  showscale = T,
                  cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
                  opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = meta.data[[input$Plot_display_type]], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)}
                ),
                showlegend = T,
                text = meta.data[[input$Plot_display_type]],
                customdata = rownames(meta.data),
                hovertemplate = paste0("Cell : %{customdata}<br>",
                                       "Value : %{text}",
                                       "<extra></extra>")
              )
          } else if (ncol(cell.embeddings.umap) == 3){
            fig <- fig %>%
              add_trace(
                x = cell.embeddings.umap[,1],
                y = cell.embeddings.umap[,2],
                z = cell.embeddings.umap[,3],
                marker = list(
                  color = meta.data[[input$Plot_display_type]],
                  colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
                  reversescale=input$invert_color_visualisation_UMAP,
                  size = input$Plot_scatter_size_UMAP,
                  showscale = T,
                  cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
                  opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = meta.data[[input$Plot_display_type]], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)}
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
          ##### palette
          palette = values$palette
          while(length(unique(meta.data[[input$Plot_display_type]])) > length(palette)){
            palette = c(palette,palette)
          }
          
          c = 1
        if (ncol(cell.embeddings.umap) == 2){
            for (i in unique(meta.data[[input$Plot_display_type]])[order(unique(meta.data[[input$Plot_display_type]]))]){
              fig <- fig %>%
                add_trace(
                  x = cell.embeddings.umap[which(meta.data[[input$Plot_display_type]]==i),1],
                  y = cell.embeddings.umap[which(meta.data[[input$Plot_display_type]]==i),2],
                  name = i,
                  marker = list(
                    color = palette[c],
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
          } else if (ncol(cell.embeddings.umap) == 3){
            for (i in unique(meta.data[[input$Plot_display_type]])[order(unique(meta.data[[input$Plot_display_type]]))]){
              fig <- fig %>%
                add_trace(
                  x = cell.embeddings.umap[which(meta.data[[input$Plot_display_type]]==i),1],
                  y = cell.embeddings.umap[which(meta.data[[input$Plot_display_type]]==i),2],
                  z = cell.embeddings.umap[which(meta.data[[input$Plot_display_type]]==i),3],
                  name = i,
                  marker = list(
                    color = values$palette[c],
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
      }
      
      fig <- fig %>% layout(xaxis = list(showgrid = input$show_grid_scatter_pie, zeroline=input$show_grid_scatter_pie,
                                         visible = input$show_grid_scatter_pie),
                            yaxis = list(showgrid = input$show_grid_scatter_pie, zeroline=input$show_grid_scatter_pie,
                                         visible = input$show_grid_scatter_pie))
      
      fig <- fig %>% event_register('plotly_selected')
      
    }
    return(fig)
    
  } else {
    
    if (input$Plot_analysis_display_type == "Dimentional reduction"){
      if (input$Plot_display_type == "seurat_clusters"){
        
        value = as.factor(meta.data[["seurat_clusters"]])
        
        fig = ggplot() +
          geom_point(aes(cell.embeddings.umap[,1], cell.embeddings.umap[,2], color=value), size = input$Plot_scatter_size_UMAP) +
          theme_void()
          
        
        
      } else if (input$Plot_display_type == "gene") {
        
        if(input$image_display_UMAP){
          scale.data = values$data@assays$SCT@data[,(colnames(values$data@assays$SCT@data) %in% unlist(c(sapply(TissueCoordinates,rownames))))]
        } else {
          scale.data = values$data@assays$SCT@data
        }
        
        fig = ggplot() +
          geom_point(aes(cell.embeddings.umap[,1], cell.embeddings.umap[,2], color=scale.data[input$gene_UMAP_choice,]), size = input$Plot_scatter_size_UMAP) +
          ggplot2::scale_color_gradientn(name = input$gene_UMAP_choice,
                                         colours = viridis_pal(option = if(input$select_color_visualisation_projection %in% c("A","B","C","D","E","F","G","H")){input$select_color_visualisation_projection}else{"D"})(ncol(values$data)), limits=c(input$slider_visual_spatial_range[1], input$slider_visual_spatial_range[2]), oob=squish) +
          guides(size = "none") +
          theme_void()
        
      } else if (input$Plot_display_type == "IC") {
        
        fig = ggplot() +
          geom_point(aes(cell.embeddings.umap[,1], cell.embeddings.umap[,2], color=cell.embeddings[,input$IC_UMAP_choice]), size = input$Plot_scatter_size_UMAP) +
          ggplot2::scale_color_gradientn(name = input$IC_UMAP_choice,
                                         colours = viridis_pal(option = if(input$select_color_visualisation_projection %in% c("A","B","C","D","E","F","G","H")){input$select_color_visualisation_projection}else{"D"})(ncol(values$data)), limits=c(input$slider_visual_spatial_range[1], input$slider_visual_spatial_range[2]), oob=squish) +
          guides(size = "none") +
          theme_void()
        
      } else {
        if(typeof(meta.data[[input$Plot_display_type]]) == "double" | grepl('nCount_|nFeature_|percent_', input$Plot_display_type)){
          
          fig = ggplot() +
            geom_point(aes(cell.embeddings.umap[,1], cell.embeddings.umap[,2], color=as.double(meta.data[[input$Plot_display_type]])), size = input$Plot_scatter_size_UMAP) +
            ggplot2::scale_color_gradientn(name = input$Plot_display_type,
                                           colours = viridis_pal(option = if(input$select_color_visualisation_projection %in% c("A","B","C","D","E","F","G","H")){input$select_color_visualisation_projection}else{"D"})(ncol(values$data)), limits=c(input$slider_visual_spatial_range[1], input$slider_visual_spatial_range[2]), oob=squish) +
            guides(size = "none") +
            theme_void()
      
        } else {
          fig = ggplot() +
            geom_point(aes(cell.embeddings.umap[,1], cell.embeddings.umap[,2], color=as.factor(meta.data[[input$Plot_display_type]])), size = input$Plot_scatter_size_UMAP) +
            labs(color = input$Plot_display_type) +
            theme_void()
        }
      }
    }
    
        
  }
})