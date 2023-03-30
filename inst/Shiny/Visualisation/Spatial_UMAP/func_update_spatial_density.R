##----------------------------------------------------------------------------##
## Spatial clustering
##----------------------------------------------------------------------------##

current_plot_spatial_density <- reactive({
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
  
  l=length(type)
  
  if (l > 1){
    ic_types=values$data@reductions$ica@cell.embeddings[,type]
    
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    ic_types<-cbind(TissueCoordinates(),t(ic_types)) %>%  cbind(.,sum_IC) %>% as_tibble()
    grid=interp(ic_types$imagecol,ic_types$imagerow,ic_types$sum_IC, nx = 400, ny = 400)
    griddf <- data.frame(x = rep(grid$x, ncol(grid$z)), 
                         y = rep(grid$y, each = nrow(grid$z)), 
                         z = as.numeric(grid$z))
    griddf$z2=ifelse(griddf$z<quantile(griddf$z,na.rm = TRUE,probs = seq(0, 1, 1/10))[2],0,griddf$z)
    
    # Create plotly object
    fig <- plot_ly(source = "B")
    
    # Add spot
  #  fig <- fig %>%
  #    add_trace(
  #      x = GetTissueCoordinates(data)[,"imagecol"],
  #      y = GetTissueCoordinates(data)[,"imagerow"],
  #      marker = list(
  #        color = 'blue'
  #      ),
  #      showlegend = F
  #    )
    
    # Add density
    if (input$Plot_contour_density == TRUE){
      p1 <- ggplot( data = ic_types,aes(x = imagecol, y = imagerow)) +
        annotation_raster(img,xmin = 0,xmax = Inf,ymin = 0,ymax = Inf)+
        geom_contour(data = griddf,aes(x = x, y = y , z=z2),breaks = input$Plot_thresh_density)  +
        geom_contour_filled(data = griddf,aes(x = x, y = y , z=z2),alpha=input$Plot_thresh_alpha_density, breaks = c(input$Plot_thresh_density,1), fill="blue")+ 
        coord_equal()+
        theme_void()
      
      fig = ggplotly(p1)
      
    } else {
      if (input$Plot_show_image_density == TRUE) {
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
            opacity=(input$Plot_thresh_alpha_density + 2)
          )
      } else {
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
      }
      fig <- fig %>% colorbar(title = "Cell type\ndensity")
    }
    
    if (!is.null(values$UMAP)){
      for (i in 0:length(summary(values$UMAP@meta.data[["seurat_clusters"]]))-1){
        fig <- fig %>%
          add_trace(
            type = "scatter",
            mode = "markers",
            x = TissueCoordinates()[,"imagecol"][which(values$UMAP@meta.data[["seurat_clusters"]]==i)],
            y = TissueCoordinates()[,"imagerow"][which(values$UMAP@meta.data[["seurat_clusters"]]==i)],
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
                                   "<extra></extra>"),
            opacity=input$Plot_thresh_alpha_density
          )
      }
    } else {
      fig <- fig %>%
        add_trace(
          type = "scatter",
          mode = "markers",
          x = TissueCoordinates()[,"imagecol"],
          y = TissueCoordinates()[,"imagerow"],
          marker = list(
            color = 'black',
            size = 10
          ),
          showlegend = T,
          text = i,
          customdata = rownames(values$data@meta.data),
          hovertemplate = paste0("Cell : %{customdata}<br>",
                                 "<extra></extra>"),
          opacity=input$Plot_thresh_alpha_density
        )
    }
    
    # Add image in the background
    if (input$Plot_show_image_density == TRUE) {
      if (is.null(values$HD_image)){
        fig <- fig %>% add_trace(type="image", source = values$low_image, hoverinfo = 'skip')
      } else {
        fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
      }
    }
    
    fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                   yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                   autosize = TRUE
    )
  } else {
    ic_types=values$data@reductions$ica@cell.embeddings[,type]
    
    # Create plotly object
    fig <- plot_ly(source = "B")
    
    
    # Add spot
     fig <- fig %>%
       add_trace(
         type = "scatter",
         mode = "markers",
         x = TissueCoordinates()[,"imagecol"],
         y = TissueCoordinates()[,"imagerow"],
         name = name,
         marker = list(
           color = ic_types
         ),
         showlegend = T,
         text = ic_types,
         customdata = rownames(values$data@meta.data),
         hovertemplate = paste0("Cell : %{customdata}<br>",
                                "Level : %{text}",
                                "<extra></extra>")
       )
     
     # Add image in the background
     if (input$Plot_show_image_density == TRUE) {
       if (is.null(values$HD_image)){
         fig <- fig %>% add_trace(type="image", source = values$low_image, hoverinfo = 'skip')
       } else {
         fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
       }
     }
    
    fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                          yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                          autosize = TRUE
    )
  }
  return(fig)
})