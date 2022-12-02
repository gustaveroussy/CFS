##----------------------------------------------------------------------------##
## Spatial clustering
##----------------------------------------------------------------------------##

current_plot_spatial_density <- reactive({
  if(length(input$Plot_display_type_choice) != 1){
    for (n_cell_type in 1:length(input$Plot_display_type_choice)) {
      if(n_cell_type == 1) {
        type = values$annotation_for_output[[n_cell_type]]
      } else {
        type = append(type, values$annotation_for_output[[n_cell_type]])
      }
    }
    type = unique(type)
  } else {
    type = values$annotation_for_output[[input$Plot_display_type_choice]]
  }
  
  l=length(type)
  ic_types=values$data@reductions$ica@cell.embeddings[,type]
  
  ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
  sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
  sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
  ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
  ic_types<-cbind(GetTissueCoordinates(values$data),t(ic_types)) %>%  cbind(.,sum_IC) %>% as.tibble()
  grid=interp(ic_types$imagecol,ic_types$imagerow,ic_types$sum_IC)
  griddf <- data.frame(x = rep(grid$x, ncol(grid$z)), 
                       y = rep(grid$y, each = nrow(grid$z)), 
                       z = as.numeric(grid$z))
  griddf$z2=ifelse(griddf$z<quantile(griddf$z,na.rm = TRUE,probs = seq(0, 1, 1/10))[2],0,griddf$z)
  
  # Create plotly object
  fig <- plot_ly(type = 'scatter',
                  mode='markers',
                 source = "B"
  )
  
  
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
  
  
  for (i in 0:length(summary(data@meta.data[["seurat_clusters"]]))-1){
    fig <- fig %>%
      add_trace(
        x = TissueCoordinates()[,"imagecol"][which(data@meta.data[["seurat_clusters"]]==i)],
        y = TissueCoordinates()[,"imagerow"][which(data@meta.data[["seurat_clusters"]]==i)],
        name = i,
        marker = list(
          color = palette()[i+1],
          size = 10
        ),
        showlegend = T,
        text = i,
        customdata = rownames(data@meta.data)[which(data@meta.data[["seurat_clusters"]]==i)],
        hovertemplate = paste0("Cell : %{customdata}<br>",
                               "Cluster : %{text}",
                               "<extra></extra>"),
        opacity=input$Plot_thresh_alpha_density
      )
  }
  
  # Add image in the background
  if (input$Plot_show_image_density == TRUE) {
    fig <- fig %>% add_trace(type="image", source = raster2uri(raster::as.raster(values$data@images$slice1@image)), hoverinfo = "skip")
  }
  
  fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                 yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                 autosize = TRUE
  )
  
  return(fig)
})
