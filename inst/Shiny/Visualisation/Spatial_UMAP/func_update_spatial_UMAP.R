##----------------------------------------------------------------------------##
## Spatial clustering
##----------------------------------------------------------------------------##

current_plot_spatial <- reactive({
  
  if(!(input$Spatial_visualisation_comput)){
    return(NULL)
  }
  
  n = 0
  out = list()
  
  for(sample in input$Plot_image_spatial){
    n = n + 1
      
    TissueCoordinates = TissueCoordinates()[[n]]
    meta.data = values$data@meta.data[(rownames(values$data@meta.data) %in% rownames(TissueCoordinates)),]
    cell.embeddings <- values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates)),]
    annotation = values$data@misc$annotation
    
    # keep cells based on UMAP
    if(!is.null(square_cell_UMAP_selected())){
      cell.embeddings = values$data@reductions$ica@cell.embeddings[square_cell_UMAP_selected()$customdata,]
      TissueCoordinates = TissueCoordinates[square_cell_UMAP_selected()$customdata,]
      meta.data = values$data@meta.data[square_cell_UMAP_selected()$customdata,]
    }
    
    fig <- plot_ly(type = 'scatter',
                   mode='markers',
                   source = "C"
    )
    
    if (input$Spatial_display_image == TRUE){
      if (!is.null(values$HD_image)) {
        fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
      } else {
        if(!is.null(values$low_image)){
          fig <- fig %>% add_trace(type="image", source = values$low_image[[n]], hoverinfo = 'skip')
        }
      }
    }
    
    if (input$Plot_analysis_display_type == "Dimentional reduction"){
      if (input$Plot_display_type == "seurat_clusters"){
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
      scale.data = values$data@assays$SCT@data[,(colnames(values$data@assays$SCT@data) %in% rownames(TissueCoordinates))]
      
      fig <- fig %>%
        add_trace(
          x = TissueCoordinates[,"imagecol"],
          y = TissueCoordinates[,"imagerow"],
          marker = list(
            color = scale.data[input$gene_UMAP_choice,],
            colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
            reversescale=input$invert_color_visualisation_spatial,
            size = input$Plot_scatter_size_spatial,
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
  
      
      fig <- fig %>% layout(showlegend = F)
        
      }  else if (input$Plot_display_type == "IC") {
        fig <- fig %>%
          add_trace(
            x = TissueCoordinates[,"imagecol"],
            y = TissueCoordinates[,"imagerow"],
            marker = list(
              color = cell.embeddings[,input$IC_UMAP_choice],
              colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
              reversescale=input$invert_color_visualisation_spatial,
              size = input$Plot_scatter_size_spatial,
              cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2],
              opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = cell.embeddings[,input$IC_UMAP_choice], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)},
              showscale = T
            ),
            showlegend = T,
            text = cell.embeddings[,input$IC_UMAP_choice],
            customdata = rownames(meta.data),
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Value : %{text}",
                                   "<extra></extra>")
          )
        
        fig <- fig %>% layout(showlegend = F)
        
      } else {
      if(typeof(meta.data[[input$Plot_display_type]]) == "double" | grepl('nCount_|nFeature_|percent_', input$Plot_display_type)){
        
        fig <- fig %>%
          add_trace(
            x = TissueCoordinates[,"imagecol"],
            y = TissueCoordinates[,"imagerow"],
            marker = list(
              color = meta.data[[input$Plot_display_type]],
              colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
              reversescale=input$invert_color_visualisation_spatial,
              size = input$Plot_scatter_size_spatial,
              showscale = T,
              opacity = if(input$transparency_visual_spatial_choice == 1){input$transparency_visual_spatial_range}else{alpha_color_scale(values = meta.data[[input$Plot_display_type]], slider_1 =input$slider_visual_spatial_range[1], slider_2 = input$slider_visual_spatial_range[2], alpha = input$transparency_visual_spatial_range)},
              cmin = input$slider_visual_spatial_range[1], cmax=input$slider_visual_spatial_range[2]
            ),
            showlegend = T,
            text = meta.data[[input$Plot_display_type]],
            customdata = rownames(meta.data),
            hovertemplate = paste("Cell : %{customdata}<br>",
                                  "Value : %{text}<br>",
                                  "<extra></extra>")
          )
        
        fig <- fig %>% layout(showlegend = F)
        
      } else {
          c = 1
          for (i in unique(meta.data[[input$Plot_display_type]])[order(unique(meta.data[[input$Plot_display_type]]))]){
            fig <- fig %>%
              add_trace(
                x = TissueCoordinates[,"imagecol"][which(meta.data[[input$Plot_display_type]]==i)],
                y = TissueCoordinates[,"imagerow"][which(meta.data[[input$Plot_display_type]]==i)],
                name = i,
                marker = list(
                  color = palette()[c],
                  size = input$Plot_scatter_size_spatial
                ),
                showlegend = T,
                text = meta.data[[input$Plot_display_type]][which(meta.data[[input$Plot_display_type]]==i)],
                customdata = rownames(meta.data)[which(meta.data[[input$Plot_display_type]]==i)],
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
    
    out[[n]] = fig
    
  }
  
  output <- subplot(out, nrows = ceiling(length(TissueCoordinates())/3))
  
  return(output)
})

