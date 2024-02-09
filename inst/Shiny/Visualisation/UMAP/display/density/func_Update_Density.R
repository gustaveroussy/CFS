##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

current_plot_density <- reactive({
  
  if(!(input$UMAP_visualisation_comput)){
    return(NULL)
  }
  
  if (!is.null(values$UMAP)){
    
    l=length(UMAP_type())
    
    if (l > 1){
      
      griddf = UMAP_griddf()
      
      # Create plotly object
      fig <- plot_ly(type = 'scatter',
                     mode='markers'
      )
      
      # Add density
        
      fig <- fig %>%
        add_trace(
          type = "contour",
          x = griddf$x,
          y = griddf$y,
          z = griddf$z2,
          showlegend = T,
          contours = list(
            end = 1, 
            size = 0.1, 
            start = input$Plot_thresh_density
          ),
          opacity=1
        )
      
      fig <- fig %>% colorbar(title = "UMAP\ndensity")
  
      # ADD umap
      for (i in 0:length(summary(values$UMAP@meta.data[,"seurat_clusters"]))-1){
        fig <- fig %>%
          add_trace(
            x = values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[,"seurat_clusters"]==i),1],
            y = values$UMAP[["umap"]]@cell.embeddings[which(values$UMAP@meta.data[,"seurat_clusters"]==i),2],
            name = i,
            marker = list(
              color = palette()[i+1],
              size = 5
            ),
            showlegend = T,
            text = i,
            customdata = rownames(values$UMAP@meta.data)[which(values$UMAP@meta.data[,"seurat_clusters"]==i)],
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Cluster : %{text}",
                                   "<extra></extra>"),
            opacity=input$Plot_thresh_alpha_density
          )
      }
      
      fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                            yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                            autosize = TRUE
      )
    } else {
      ic_types=values$UMAP@reductions$ica@cell.embeddings[,UMAP_type()]
      # Create plotly object
      fig <- plot_ly(type = 'scatter',
                     mode='markers'
      )

      # ADD umap
      fig <- fig %>%
        add_trace(
          x = values$UMAP[["umap"]]@cell.embeddings[,1],
          y = values$UMAP[["umap"]]@cell.embeddings[,2],
          name = "UMAP",
          marker = list(
            color = ic_types,
            colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection),
            size = 5
          ),
          showlegend = T,
          text = ic_types,
          customdata = rownames(values$UMAP@meta.data),
          hovertemplate = paste0("Cell : %{customdata}<br>",
                                 "Level : %{text}",
                                 "<extra></extra>")
        )
      
      fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                            yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                            autosize = TRUE
      )
    }
    return(fig)
  } else {
    return(NULL)
  }
})

# get the ICs related to the annotation
UMAP_type <- reactive({
  if(length(input$Plot_display_type_choice) != 1){
    name = paste(input$Plot_display_type_choice,collapse = ",")
    for (n_cell_type in 1:length(input$Plot_display_type_choice)) {
      if(n_cell_type == 1) {
        type = values$annotation_for_output[[n_cell_type]]
      } else {
        type = append(type, values$annotation_for_output[[n_cell_type]])
      }
    }
    type = unique(type)
  } else {
    name = input$Plot_display_type_choice
    type = values$annotation_for_output[[input$Plot_display_type_choice]]
  }
  
  return(type)
})

# get the density annotation.
UMAP_griddf <- reactive({
  ic_types=values$UMAP@reductions$ica@cell.embeddings[,UMAP_type()]
  
  ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
  sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
  sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
  ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
  ic_types<-cbind(values$UMAP@reductions$umap@cell.embeddings,t(ic_types)) %>%  cbind(.,sum_IC)
  print(ic_types)
  grid=interp(ic_types[,'umap_1'],ic_types[,'umap_2'],ic_types[,'sum_IC'], nx = 400, ny = 400)
  griddf <- data.frame(x = rep(grid$x, ncol(grid$z)), 
                       y = rep(grid$y, each = nrow(grid$z)), 
                       z = as.numeric(grid$z))    
  griddf$z2=ifelse(griddf$z<quantile(griddf$z,na.rm = TRUE,probs = seq(0, 1, 1/10))[2],0,griddf$z)
  return(griddf)
})


