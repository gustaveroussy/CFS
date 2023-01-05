##############################
# Scatter pie
##############################

current_plot_spatial_scatter_pie <- reactive({
  
  data <- values$data
  
  if (req(input$ggplot_scatter_pie) == TRUE) {
    
    type = c(1,2,3,4)
    
    # we catch the IC considered there as cell type or activities
    ic_types=data@reductions$ica@cell.embeddings[,type]
    # we put negative vvalue to 0
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    #We normalize by the sum
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    #we calculate the factor of size to reduce pie size where IC are low
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    
    #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    # We build the final object
    dim = dim(img)
    dim[1]
    dim[2]
    min(data@images$slice1@coordinates[,'imagerow'])
    max(data@images$slice1@coordinates[,'imagerow'])
    min(data@images$slice1@coordinates[,'imagecol'])
    max(data@images$slice1@coordinates[,'imagecol'])
    ic_types<-cbind((data@images$slice1@coordinates),ic_types) %>%  cbind(.,sum_IC) %>% as.tibble
    
    #We extract the image
    #img<-grDevices::as.raster(data@images$slice1@image)
    
    img <- readPNG('/home/c_thuilliez/Desktop/Work/output/Spatial_Patient/MAP330/tissue_hires_image.png')
    g <- rasterGrob(img, interpolate=TRUE)
    g = as.raster(img)
    # We build the plot
    fig <- ggplot(data = ic_types,aes(x = imagecol, y = imagerow)) +
      annotation_raster(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      geom_scatterpie(data = ic_types,
                      aes(x = imagecol, y = (-imagerow),
                          r=sum_IC*120),
                      cols = paste0("IC_",type),
                      color=NA) +
      scale_x_continuous(expand = expansion(mult = 0.1, add = 0))+
      scale_y_continuous(expand = expansion(mult = 0.1, add = 0))+
      coord_fixed() +
      theme_classic()
  } else {
    
    #type=c(48,13,18,26)
    type = unlist(values$annotation_for_output[input$Scatter_pie_cell_type], use.names=FALSE)
    type=unique(c(input$All_IC_chosen_projection,type))
    
    ic_types=data@reductions$ica@cell.embeddings[,type]
    
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    
    #We normalize by the sum
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    #we calculate the factor of size to reduce pie size where IC are low
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    
    #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    # We build the final object
    ic_types<-cbind(GetTissueCoordinates(data),ic_types) %>%  cbind(.,sum_IC)
    
    ic_types <- ic_types[rowSums(ic_types[,1:length(type)+2])>0,]
    
    #We build the plot
    
    #img<-grDevices::as.raster(data@images$slice1@image)
    
    ic_types$imagerow = max(ic_types$imagerow) - ic_types$imagerow + min(ic_types$imagerow)
    
    ####
    
    fig <- plot_ly()
    
    
    for (i in 1:nrow(ic_types)) {
      r=sum_IC
      
      t = colnames(ic_types[i,1:length(type)+2][which(ic_types[i,1:length(type)+2] != 0)])
      v = round(as.double(ic_types[i,1:length(type)+2][which(ic_types[i,1:length(type)+2] != 0)])/sum(as.double(ic_types[i,1:length(type)+2][which(ic_types[i,1:length(type)+2] != 0)]))*100,2)
      
      text_final = ""
      
      for(k in 1:length(t)) {
        text_final = paste(text_final,paste0(t[k]," : ",v[k],"%<br>"))
      }
      
      
      col_coordinates = (ic_types[i,"imagecol"])/dim(data@images$slice1@image)[1]
      row_coordinates = (ic_types[i,"imagerow"])/dim(data@images$slice1@image)[2]
      
      x = c(col_coordinates-(ic_types[i,"sum_IC"]/input$pieplot_size),col_coordinates+ic_types[i,"sum_IC"]/input$pieplot_size)
      y = c(row_coordinates-(ic_types[i,"sum_IC"]/input$pieplot_size),row_coordinates+ic_types[i,"sum_IC"]/input$pieplot_size)
      
      fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = colnames(ic_types[i,1:length(type)+2][which(ic_types[i,1:length(type)+2] != 0)])
                               , values = as.double(ic_types[i,1:length(type)+2][which(ic_types[i,1:length(type)+2] != 0)]),
                               name = rownames(ic_types[i,]), domain = list(x = x, y = y),
                               showlegend = TRUE, textposition = "none", textinfo = "none",
                               text = text_final,
                               hovertemplate = paste0("%{text}",
                                                      "<extra></extra>"))
    }
    
    fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                          yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                          grid = list(columns = dim(data@images$slice1@image)[1], rows = dim(data@images$slice1@image)[2]),
                          images = list(
                            source = raster2uri(raster::as.raster(data@images$slice1@image)),
                            xref = 'paper',
                            yref =  'paper',
                            sizex = 1,
                            sizey = 1,
                            sizing = 'stretch',
                            opacity = 1,
                            layer= 'below',
                            x = 0,
                            y = 1,   
                            yanchor = 'top',
                            xanchor = 'left'
                          )
    )
  }
  
  return(fig)
})
