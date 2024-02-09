##----------------------------------------------------------------------------##
## Spatial clustering
##----------------------------------------------------------------------------##

current_plot_spatial_density <- reactive({
  if(!(input$Spatial_visualisation_comput)){
    return(NULL)
  }
  
  l=length(UMAP_type())
  out = list()
  
  if (l > 1){
    
    griddf = spatial_griddf()

    n = 0
    
    for(sample in input$Plot_image_spatial){
      n = n + 1
      # Add density
      g = griddf[[n]]
      
      
      
      fig <- plot_ly(source = "B")
  
      fig <- fig %>%
        add_trace(
          type = "contour",
          x = g$x,
          y = g$y,
          z = g$z2,
          showlegend = T,
          contours = list(
            end = 1,
            size = 0.1,
            start = input$Plot_thresh_density
          ),
          opacity=if(input$Spatial_display_image == TRUE){(input$Plot_thresh_alpha_density + 2)}else{1}
        )
      
      fig <- fig %>% colorbar(title = "Cell type\ndensity")
      
      if (!is.null(values$UMAP)){
        for (i in 0:length(summary(values$UMAP@meta.data[rownames(TissueCoordinates()[[n]]),"seurat_clusters"]))-1){
          fig <- fig %>%
            add_trace(
              type = "scatter",
              mode = "markers",
              x = TissueCoordinates()[[n]][,"imagecol"][which(values$UMAP@meta.data[rownames(TissueCoordinates()[[n]]),"seurat_clusters"]==i)],
              y = TissueCoordinates()[[n]][,"imagerow"][which(values$UMAP@meta.data[rownames(TissueCoordinates()[[n]]),"seurat_clusters"]==i)],
              name = i,
              marker = list(
                color = palette()[i+1],
                size = 10
              ),
              showlegend = T,
              text = i,
              customdata = rownames(values$UMAP@meta.data)[which(values$UMAP@meta.data[rownames(TissueCoordinates()[[n]]),"seurat_clusters"]==i)],
              hovertemplate = paste0("Cell : %{customdata}<br>",
                                     "Cluster : %{text}",
                                     "<extra></extra>"),
              opacity=input$Plot_thresh_alpha_density
            )
        }
      } else {
        fig <- fig %>%
          add_trace(
            type = "scatter",
            mode = "markers",
            x = TissueCoordinates()[[n]][,"imagecol"],
            y = TissueCoordinates()[[n]][,"imagerow"],
            marker = list(
              color = 'black',
              size = 10
            ),
            showlegend = T,
            text = i,
            customdata = rownames(TissueCoordinates()[[n]]),
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "<extra></extra>"),
            opacity=input$Plot_thresh_alpha_density
          )
      }
      # Add image in the background
      if (input$Spatial_display_image == TRUE) {
        if (is.null(values$HD_image)){
          if(!is.null(values$low_image)){
            fig <- fig %>% add_trace(type="image", source = values$low_image[[n]], hoverinfo = 'skip')
          }
        } else {
          fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
        }
      }
      
      fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                            yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                            autosize = TRUE
      )
      
      out[[n]] = fig
      
    }
    
    output = subplot(out)
    
    return(output)
    
      
      
      
      
      
      
      
    } else {
      
      n = 0
      
      for(sample in input$Plot_image_spatial){
        n = n + 1 
      
        ic_types=values$data@reductions$ica@cell.embeddings[rownames(TissueCoordinates()[[n]]),UMAP_type()]
        
        # Create plotly object
        fig <- plot_ly(source = "B")
        
        
        # Add spot
         fig <- fig %>%
           add_trace(
             type = "scatter",
             mode = "markers",
             x = TissueCoordinates()[[n]][,"imagecol"],
             y = TissueCoordinates()[[n]][,"imagerow"],
             name = sample,
             marker = list(
               color = ic_types,
               colorscale = plotly_colorscale(colorscale = input$select_color_visualisation_projection)
             ),
             showlegend = T,
             text = ic_types,
             customdata = rownames(values$data@meta.data[rownames(TissueCoordinates()[[n]]),]),
             hovertemplate = paste0("Cell : %{customdata}<br>",
                                    "Level : %{text}",
                                    "<extra></extra>")
           )
         # Add image in the background
         if (input$Spatial_display_image == TRUE){
           if (is.null(values$HD_image)){
             if(!is.null(values$low_image)){
               fig <- fig %>% add_trace(type="image", source = values$low_image[[n]], hoverinfo = 'skip')
             }
           } else {
             fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
           }
         }
        
        fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                              yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                              autosize = TRUE
        )
        
        
        out[[n]] = fig
      }
      
    output = subplot(out)

    
  }
  
  return(output)
})


current_plot_spatial_density_ggplot <- reactive({
  
  fig <- ggplot( data = ic_types,aes(x = imagecol, y = imagerow)) +
    annotation_raster(if(!is.null(values$HD_image)){as.raster(values$HD_image)}else{raster::as.raster(values$data@images$slice1@image)}, xmin = 0, xmax = Inf, ymin = 0, ymax = Inf)+
    geom_contour(data = griddf,aes(x = x, y = y , z=z2),breaks = input$Plot_thresh_density)  +
    geom_contour_filled(data = griddf,aes(x = x, y = y , z=z2),alpha=input$Plot_thresh_alpha_density, breaks = c(input$Plot_thresh_density,1), fill="blue")+ 
    coord_equal()+
    theme_void()
  
  return(fig)
})

# get the density annotation.
spatial_griddf <- reactive({
  output = list()
  
  
  for(i in 1:length(input$Plot_image_spatial)){
    ic_types=values$data@reductions$ica@cell.embeddings[rownames(TissueCoordinates()[[i]]),UMAP_type()]
    
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    ic_types<-cbind(TissueCoordinates()[[i]],t(ic_types)) %>%  cbind(.,sum_IC) %>% as_tibble()
    grid=interp(ic_types$imagecol,ic_types$imagerow,ic_types$sum_IC, nx = 400, ny = 400)
    griddf <- data.frame(x = rep(grid$x, ncol(grid$z)), 
                         y = rep(grid$y, each = nrow(grid$z)), 
                         z = as.numeric(grid$z))
    griddf$z2=ifelse(griddf$z<quantile(griddf$z,na.rm = TRUE,probs = seq(0, 1, 1/10))[2],0,griddf$z)
    
    output = append(output,list(griddf))
  }
  
  return(output)
})



