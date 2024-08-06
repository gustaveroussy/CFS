##----------------------------------------------------------------------------##
## Spatial clustering
##----------------------------------------------------------------------------##

current_plot_spatial <- reactive({
  
  if(!(input$Spatial_visualisation_comput)){
    return(NULL)
  }
  
  if(input$interactive_display_visualisation_spatial){
    
    n = 0
    out = list()
    
    for(sample in input$Plot_image_spatial){
      n = n + 1
        
      TissueCoordinates = TissueCoordinates()[[sample]]
      meta.data = values$data@meta.data[(rownames(values$data@meta.data) %in% rownames(TissueCoordinates)),]
      cell.embeddings <- values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates)),]
      annotation = values$data@misc$annotation
      
      # keep cells based on UMAP
      
      if(input$apply_umap_selection_filter){
        if(!is.null(selected_cell_UMAP_selected())){
          cell.embeddings = values$data@reductions$ica@cell.embeddings[selected_cell_UMAP_selected()$customdata,]
          TissueCoordinates = TissueCoordinates[selected_cell_UMAP_selected()$customdata,]
          meta.data = values$data@meta.data[selected_cell_UMAP_selected()$customdata,]
        }
      }
      
      fig <- plot_ly(type = 'scatter',
                     mode='markers',
                     source = "C"
      )
      
      if (input$Spatial_display_image == TRUE){
        if (!is.null(values$HD_image)) {
          fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
        } else {
          if(length(values$low_image) != 0){
            fig <- fig %>% add_trace(type="image", source = values$low_image[[sample]], hoverinfo = 'skip')
          }
        }
      }
      
      if (input$Plot_analysis_display_type == "Dimentional reduction"){
        if (input$Plot_display_type == "metadata"){
          if(input$what_to_display_UMAP_choice == "seurat_clusters"){
            
            ##### palette
            palette = values$palette
            while(length(as.numeric(as.vector(unique(meta.data[["seurat_clusters"]])))) > length(palette)){
              palette = c(palette,palette)
            }
            
            ##### clusters
            for (i in as.numeric(as.vector(unique(meta.data[["seurat_clusters"]])))[order(as.numeric(as.vector(unique(meta.data[["seurat_clusters"]]))))]){
              if(length(which(meta.data[["seurat_clusters"]]==i)) == 1){
                table = t(as.data.frame(cell.embeddings[which(meta.data[["seurat_clusters"]]==i),]))
                rownames(table) = rownames(meta.data[which(meta.data[["seurat_clusters"]]==i),])
              } else {
                table = cell.embeddings[which(meta.data[["seurat_clusters"]]==i),]
              }
              
              list_cells_ICs = c()
              for(k in 1:length(rownames(table))){
                if(input$full_annotation_spatial == "IC" | input$full_annotation_spatial == "Full annotation" | input$full_annotation_spatial == "Mean IC" | input$full_annotation_spatial == "Mean full annotation"){
                  if(input$full_annotation_spatial == "IC" | input$full_annotation_spatial == "Full annotation"){
                    top_10_ICs = head(colnames(table)[order(table[rownames(table)[k], ],decreasing = TRUE)],10)
                  } else {
                    cell_names = rownames(meta.data[meta.data$seurat_clusters == i,])
                    means = colMeans(cell.embeddings[cell_names,])
                    top_10_ICs = head(means[order(means,decreasing = TRUE)], n = 10L)
                  }
                  final_vector = c('Cluster : ', i,'\nTop 10 ICs :\n')
                  for (j in 1:length(top_10_ICs)){
                    if(input$full_annotation_spatial == "Full annotation" & (top_10_ICs[j] %in% rownames(annotation))){
                      final_vector = c(final_vector,top_10_ICs[j],' : ',round(table[rownames(table)[k],top_10_ICs[j]],4),' : ',annotation[top_10_ICs[j],'Type'],' : ',annotation[top_10_ICs[j],'Annotation'],'\n')
                    } else if (input$full_annotation_spatial == "IC" & (top_10_ICs[j] %in% rownames(annotation))) {
                      final_vector = c(final_vector,top_10_ICs[j],' : ',round(table[rownames(table)[k],top_10_ICs[j]],4),'\n')
                    } else if (!is.null(names(top_10_ICs))){
                      if(input$full_annotation_spatial == "Mean IC" & (names(top_10_ICs)[j] %in% rownames(annotation))) {
                        final_vector = c(final_vector,names(top_10_ICs)[j],' : ',round(top_10_ICs[j],4),'\n')
                      } else if (input$full_annotation_spatial == "Mean full annotation" & (names(top_10_ICs)[j] %in% rownames(annotation))) {
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
              
              r = TissueCoordinates[,"imagecol"][which(meta.data[["seurat_clusters"]]==i)]
              r = length(as.vector(r))
              
              datatable <- data.frame("x" = as.vector(TissueCoordinates[,"imagecol"][which(meta.data[["seurat_clusters"]]==i)]),
                                      "y" = as.vector(TissueCoordinates[,"imagerow"][which(meta.data[["seurat_clusters"]]==i)]),
                                      "cell_name" = as.vector(rownames(meta.data)[which(meta.data[["seurat_clusters"]]==i)]),
                                      "t" = list_cells_ICs)
              
              fig <- fig %>%
                add_trace(data = datatable,
                  x = ~x,
                  y = ~y,
                  name = i,
                  marker = list(
                    color = palette[i+1],
                    size = input$Plot_scatter_size_spatial
                  ),
                  showlegend = T,
                  customdata = datatable$cell_name,#i,
                  text = datatable$t,
                  hovertemplate = paste0("Cell : %{customdata}<br>",
                                         "%{text}",
                                         "<extra></extra>")
                )
            }
          }  else if (typeof(meta.data[[input$what_to_display_UMAP_choice]]) == "double" | grepl('nCount_|nFeature_|percent_', input$what_to_display_UMAP_choice)){
              req(input$select_color_visualisation_projection)
              req(input$slider_visual_spatial_range)
              fig <- fig %>%
                add_trace(
                  x = TissueCoordinates[,"imagecol"],
                  y = TissueCoordinates[,"imagerow"],
                  marker = list(
                    color = meta.data[[input$what_to_display_UMAP_choice]],
                    colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
                    reversescale=input$invert_color_visualisation_spatial,
                    size = input$Plot_scatter_size_spatial,
                    showscale = T,
                    opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = meta.data[[input$what_to_display_UMAP_choice]], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)},
                    cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2]
                  ),
                  showlegend = T,
                  text = meta.data[[input$what_to_display_UMAP_choice]],
                  customdata = rownames(meta.data),
                  hovertemplate = paste("Cell : %{customdata}<br>",
                                        "Value : %{text}<br>",
                                        "<extra></extra>")
                )
              
              fig <- fig %>% layout(showlegend = F)
              
            } else {
              
              ##### palette
              palette = values$palette
              while(length(unique(meta.data[[input$what_to_display_UMAP_choice]])) > length(palette)){
                palette = c(palette,palette)
              }
              
              c = 1
              for (i in unique(meta.data[[input$what_to_display_UMAP_choice]])[order(unique(meta.data[[input$what_to_display_UMAP_choice]]))]){
                fig <- fig %>%
                  add_trace(
                    x = TissueCoordinates[,"imagecol"][which(meta.data[[input$what_to_display_UMAP_choice]]==i)],
                    y = TissueCoordinates[,"imagerow"][which(meta.data[[input$what_to_display_UMAP_choice]]==i)],
                    name = i,
                    marker = list(
                      color = palette[c],
                      size = input$Plot_scatter_size_spatial
                    ),
                    showlegend = T,
                    text = meta.data[[input$what_to_display_UMAP_choice]][which(meta.data[[input$what_to_display_UMAP_choice]]==i)],
                    customdata = rownames(meta.data)[which(meta.data[[input$what_to_display_UMAP_choice]]==i)],
                    hovertemplate = paste("Cell : %{customdata}<br>",
                                          "Ploïdie : %{text}<br>",
                                          "<extra></extra>")
                  )
                c = c + 1
              }
            }
      } else if (input$Plot_display_type == "gene") {
        
        req(input$select_color_visualisation_projection)
        req(input$slider_visual_spatial_range)
        
        scale.data = GetAssayData(values$data, assay = values$data@active.assay)[,(colnames(GetAssayData(values$data, assay = values$data@active.assay)) %in% rownames(TissueCoordinates))]
        if(input$apply_umap_selection_filter){
          if(!is.null(selected_cell_UMAP_selected())){
            scale.data = scale.data[,selected_cell_UMAP_selected()$customdata]
          }
        }
        
        fig <- fig %>%
          add_trace(
            x = TissueCoordinates[,"imagecol"],
            y = TissueCoordinates[,"imagerow"],
            marker = list(
              color = scale.data[input$what_to_display_UMAP_choice,],
              colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
              reversescale=input$invert_color_visualisation_spatial,
              size = input$Plot_scatter_size_spatial,
              showscale = T,
              cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
              opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = scale.data[input$what_to_display_UMAP_choice,], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)}
            ),
            showlegend = T,
            text = scale.data[input$what_to_display_UMAP_choice,],
            customdata = rownames(meta.data),
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Value : %{text}",
                                   "<extra></extra>")
          )
    
        
        fig <- fig %>% layout(showlegend = F)
          
        }  else if (input$Plot_display_type == "ica" | input$Plot_display_type %in% names(values$data@misc$reduction_names)) {
          
          req(input$select_color_visualisation_projection)
          req(input$slider_visual_spatial_range)
          
          # get reductions values
          cell.embeddings <- values$data@reductions[[input$Plot_display_type]]@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates)),]
          
          # keep cells based on UMAP
          if(input$apply_umap_selection_filter){
            if(!is.null(selected_cell_UMAP_selected())){
              cell.embeddings = values$data@reductions[[input$Plot_display_type]]@cell.embeddings[selected_cell_UMAP_selected()$customdata,]
            }
          }
          
          if (input$Plot_display_type == "ica"){
            dimension_query = input$what_to_display_UMAP_choice
          } else {
            dimension_query = which(values$data@misc$reduction_names[[input$Plot_display_type]] == input$what_to_display_UMAP_choice)
          }
          
          fig <- fig %>%
            add_trace(
              x = TissueCoordinates[,"imagecol"],
              y = TissueCoordinates[,"imagerow"],
              marker = list(
                color = cell.embeddings[,dimension_query],
                colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
                reversescale=input$invert_color_visualisation_spatial,
                size = input$Plot_scatter_size_spatial,
                cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
                opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = cell.embeddings[,dimension_query], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)},
                showscale = T
              ),
              showlegend = T,
              text = cell.embeddings[,dimension_query],
              customdata = rownames(meta.data),
              hovertemplate = paste0("Cell : %{customdata}<br>",
                                     "Value : %{text}",
                                     "<extra></extra>")
            )
          
          fig <- fig %>% layout(showlegend = F)
          
        }
      }
      
      fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                     yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                     autosize = TRUE
      )
      
      fig <- fig %>% event_register('plotly_click')
      fig <- fig %>% event_register('plotly_brushed')
      fig <- fig %>% event_register("plotly_selected")
      
      out[[n]] = fig
      
    }
    if(length(out) > 1){
      output <- subplot(out, nrows = ceiling(length(TissueCoordinates())/3))
    } else {
      output = out[[1]]
    }
    
    return(output)
  } else {
    list = list()
    
    if (input$Plot_analysis_display_type == "Dimentional reduction"){
      if (input$Plot_display_type == "metadata"){
        if(input$what_to_display_UMAP_choice == "seurat_clusters"){
        
        coordinates = lapply(input$Plot_image_spatial,function(n){return(GetTissueCoordinates(values$data,n))})
        names(coordinates) = input$Plot_image_spatial
        
        img = lapply(values$data@images,function(n){return(n@image)})
        
        coordinates = lapply(input$Plot_image_spatial, function(sample){coordinates[[sample]] = cbind(coordinates[[sample]],values$data@meta.data[rownames(coordinates[[sample]]), "seurat_clusters"]); colnames(coordinates[[sample]]) = c("imagerow","imagecol","value");return(coordinates[[sample]])})
        names(coordinates) = input$Plot_image_spatial
        
        View(coordinates)
        
        for(sample in input$Plot_image_spatial){
          
          fig = ggplot(coordinates[[sample]], aes(imagecol, -imagerow)) +
            background_image(img[[sample]]) +
            geom_point(data = coordinates[[sample]], aes(color=value), size=input$Plot_scatter_size_spatial) +
            #scale_colour_manual(name = "value", values = palette()) +
            guides(size = "none") +
            theme_void() +
            xlim(25,ncol(img[[sample]])-25) +
            ylim(-nrow(img[[sample]])+25,-25)
          
          
          list[[sample]] = fig
        }
        
        output = ggarrange(plotlist=list, 
                           labels = input$Plot_image_spatial)
        
        
        
        
        } else if(typeof(values$data@meta.data[[input$what_to_display_UMAP_choice]]) == "double" | grepl('nCount_|nFeature_|percent_', input$what_to_display_UMAP_choice)){
          
          coordinates = lapply(input$Plot_image_spatial,function(n){return(GetTissueCoordinates(values$data,n))})
          names(coordinates) = input$Plot_image_spatial
          
          img = lapply(values$data@images,function(n){return(n@image)})
          
          coordinates = lapply(input$Plot_image_spatial, function(sample){coordinates[[sample]] = cbind(coordinates[[sample]],values$data@meta.data[rownames(coordinates[[sample]]), input$what_to_display_UMAP_choice]); colnames(coordinates[[sample]]) = c("imagerow","imagecol","value");return(coordinates[[sample]])})
          names(coordinates) = input$Plot_image_spatial
          
          for(sample in input$Plot_image_spatial){
            
            fig = ggplot(coordinates[[sample]], aes(imagecol, -imagerow)) +
              background_image(img[[sample]]) +
              geom_point(data = coordinates[[sample]], aes(color=value), size=input$Plot_scatter_size_spatial) +
              ggplot2::scale_color_gradientn(name = input$what_to_display_UMAP_choice,
                                             colours = viridis_pal(option = if(input$select_color_visualisation_projection %in% c("A","B","C","D","E","F","G","H")){input$select_color_visualisation_projection}else{"D"})(ncol(values$data)), limits=c(input$slider_visual_spatial_range[1], input$slider_visual_spatial_range[2]), oob=squish) +
              guides(size = "none") +
              theme_void() +
              xlim(25,ncol(img[[sample]])-25) +
              ylim(-nrow(img[[sample]])+25,-25)
            
            
            list[[sample]] = fig
          }
          
          output = ggarrange(plotlist=list, 
                             labels = input$Plot_image_spatial)
          
        } else {
          
          coordinates = lapply(input$Plot_image_spatial,function(n){return(GetTissueCoordinates(values$data,n))})
          names(coordinates) = input$Plot_image_spatial
          
          img = lapply(values$data@images,function(n){return(n@image)})
          
          coordinates = lapply(input$Plot_image_spatial, function(sample){coordinates[[sample]] = cbind(coordinates[[sample]],values$data@meta.data[rownames(coordinates[[sample]]), input$what_to_display_UMAP_choice]); colnames(coordinates[[sample]]) = c("imagerow","imagecol","value");return(coordinates[[sample]])})
          names(coordinates) = input$Plot_image_spatial
          
          for(sample in input$Plot_image_spatial){
            
            fig = ggplot(coordinates[[sample]], aes(imagecol, -imagerow)) +
              background_image(img[[sample]]) +
              geom_point(data = coordinates[[sample]], aes(color=value), size=input$Plot_scatter_size_spatial) +
              guides(size = "none") +
              theme_void() +
              xlim(25,ncol(img[[sample]])-25) +
              ylim(-nrow(img[[sample]])+25,-25)
            
            
            list[[sample]] = fig
          }
          
          output = ggarrange(plotlist=list, 
                             labels = input$Plot_image_spatial)
          
        }
        
        
        
        
        
        
      } else if (input$Plot_display_type == "gene") {
        
        coordinates = lapply(input$Plot_image_spatial,function(n){return(GetTissueCoordinates(values$data,n))})
        names(coordinates) = input$Plot_image_spatial
        
        img = lapply(values$data@images,function(n){return(n@image)})
        
        coordinates = lapply(input$Plot_image_spatial, function(sample){coordinates[[sample]] = cbind(coordinates[[sample]],GetAssayData(values$data, assay = values$data@active.assay)[input$what_to_display_UMAP_choice, rownames(coordinates[[sample]])]); colnames(coordinates[[sample]]) = c("imagerow","imagecol","value");return(coordinates[[sample]])})
        names(coordinates) = input$Plot_image_spatial
        
        for(sample in input$Plot_image_spatial){
          
          fig = ggplot(coordinates[[sample]], aes(imagecol, -imagerow)) +
            background_image(img[[sample]]) +
            geom_point(data = coordinates[[sample]], aes(color=value), size=input$Plot_scatter_size_spatial) +
            ggplot2::scale_color_gradientn(name = input$what_to_display_UMAP_choice,
                                           colours = viridis_pal(option = if(input$select_color_visualisation_projection %in% c("A","B","C","D","E","F","G","H")){input$select_color_visualisation_projection}else{"D"})(ncol(values$data)), limits=c(input$slider_visual_spatial_range[1], input$slider_visual_spatial_range[2]), oob=squish) +
            guides(size = "none") +
            theme_void() +
            xlim(25,ncol(img[[sample]])-25) +
            ylim(-nrow(img[[sample]])+25,-25)
          
          
          list[[sample]] = fig
        }
        
        output = ggarrange(plotlist=list, 
                           labels = input$Plot_image_spatial)
        
      }  else if (input$Plot_display_type == "ica" | input$Plot_display_type %in% names(values$data@misc$reduction_names)) {
        
        if (input$Plot_display_type == "ica"){
          dimension_query = input$what_to_display_UMAP_choice
        } else {
          dimension_query = which(values$data@misc$reduction_names[[input$Plot_display_type]] == input$what_to_display_UMAP_choice)
        }
        
        coordinates = lapply(input$Plot_image_spatial,function(n){return(GetTissueCoordinates(values$data,n))})
        names(coordinates) = input$Plot_image_spatial
        
        img = lapply(values$data@images,function(n){return(n@image)})
        
        coordinates = lapply(input$Plot_image_spatial, function(sample){coordinates[[sample]] = cbind(coordinates[[sample]],values$data@reductions[[input$Plot_display_type]]@cell.embeddings[rownames(coordinates[[sample]]), dimension_query]); colnames(coordinates[[sample]]) = c("imagerow","imagecol","value");return(coordinates[[sample]])})
        names(coordinates) = input$Plot_image_spatial
        
        limits = c(input$slider_visual_spatial_range[1], input$slider_visual_spatial_range[2])
        
        for(sample in input$Plot_image_spatial){
          
          if(limits[1] == limits[2]){
            limits = c(min(coordinates[[sample]]$value),max(coordinates[[sample]]$value))
          }
          
          fig = ggplot(coordinates[[sample]], aes(imagecol, -imagerow)) +
            background_image(img[[sample]]) +
            geom_point(data = coordinates[[sample]], aes(color=value), size=input$Plot_scatter_size_spatial) +
            ggplot2::scale_color_gradientn(name = input$what_to_display_UMAP_choice,
                                           colours = viridis_pal(option = if(input$select_color_visualisation_projection %in% c("A","B","C","D","E","F","G","H")){input$select_color_visualisation_projection}else{"D"})(ncol(values$data)), limits=c(limits[1], limits[2]), oob=squish) +
            guides(size = "none") +
            theme_void() +
            xlim(25,ncol(img[[sample]])-25) +
            ylim(-nrow(img[[sample]])+25,-25) 
          
          
          list[[sample]] = fig
        }
        
        output = ggarrange(plotlist=list, 
                           labels = input$Plot_image_spatial)
        
      }
    }
    return(output)
  }
})

