##############################
# Scatter pie
##############################

current_plot_spatial_scatter_pie <- reactive({
  
  withProgress(message = 'Preparing Scatterpie', value = 0, {
  
  data <- values$data
  max_col_img = dim(data@images[[1]]@image)[2]
  max_row_img = dim(data@images[[1]]@image)[1]
  
  if (!is.null(input$Scatter_pie_cell_type)){
    type = unlist(values$annotation_for_output[input$Scatter_pie_cell_type], use.names=FALSE)
    if (!is.null(input$Scatter_pie__IC_chosen_projection)){
      type=unique(c(input$Scatter_pie__IC_chosen_projection,type))
    }
  } else {
    if (!is.null(input$Scatter_pie__IC_chosen_projection)){
      type=input$Scatter_pie__IC_chosen_projection
    } else {
      type = NULL
    }
  }
  
  incProgress(0.1, detail = "getting ICs")
  
  if(!is.null(type)){

    ic_types=data@reductions$ica@cell.embeddings[,type]
    
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    
    #We normalize by the sum
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    #we calculate the factor of size to reduce pie size where IC are low
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    
    #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    # We build the final object
    ic_types<-cbind(GetTissueCoordinates(data),ic_types) %>%  cbind(.,sum_IC)
    
    #We build the plot
    
    #img<-grDevices::as.raster(data@images[[1]]@image)
    
    #ic_types$imagerow = max(ic_types$imagerow) - ic_types$imagerow + min(ic_types$imagerow)
    
    ####
    
    max_col = max(ic_types[,"imagecol"])
    max_row = max(ic_types[,"imagerow"])
    
    fig <- plot_ly()
    
    #Radius(data@images[[1]])
    for (i in 1:nrow(ic_types)) {
      incProgress(0.7/nrow(ic_types), detail = "Preparing scatterpie data")
      t = colnames(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)])
      v = round(as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)])/sum(as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]))*100,2)
      q = list()
      for (IC in t){
        q[IC] = values$Annotation[,'Annotation'][IC]
      }
        
      text_final = ""
      
      for(k in 1:length(t)) {
        text_final = paste(text_final,paste0(t[k]," : ",v[k],"% : ",q[k],"<br>"))
      }
      
      col_coordinates = (ic_types[i,"imagecol"])
      row_coordinates = max_row_img-(ic_types[i,"imagerow"])
      
      x = c((col_coordinates/max_col_img)-(Radius(data@images[[1]])*ic_types[i,"sum_IC"])/2,(col_coordinates/max_col_img)+(Radius(data@images[[1]])*ic_types[i,"sum_IC"])/2)
      y = c((row_coordinates/max_row_img)-(Radius(data@images[[1]])*ic_types[i,"sum_IC"])/2,(row_coordinates/max_row_img)+(Radius(data@images[[1]])*ic_types[i,"sum_IC"])/2)
      
      fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = values$Annotation[colnames(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),'Annotation']
                               , values = as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),
                               name = rownames(ic_types[i,]), domain = list(x = x, y = y),
                               showlegend = TRUE, textposition = "none", textinfo = "none",
                               text = text_final,
                               hovertemplate = paste0("%{text}",
                                                      "<extra></extra>"))
    }
    
    incProgress(0.1, detail = "Preparing image")
    
    if (input$Spatial_display_image == TRUE){
      if (is.null(values$HD_image)){
        if(!is.null(values$low_image)){
          fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                                yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                                grid = list(columns = max_row_img, rows = max_col_img),
                                images = list(
                                  source = values$low_image,
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
      } else {
        fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                              yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                              grid = list(columns = max_row_img, rows = max_col_img),
                              images = list(
                                source = values$HD_image,
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
    } else {
      fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                            yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                            grid = list(columns = max_row_img, rows = max_col_img)
      )
    }
  } else {
    ic_types=data@reductions$ica@cell.embeddings
    
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    
    #We normalize by the sum
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    #we calculate the factor of size to reduce pie size where IC are low
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    
    #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    # We build the final object
    ic_types<-cbind(GetTissueCoordinates(data),ic_types) %>%  cbind(.,sum_IC)
    
    #We build the plot
    
    #img<-grDevices::as.raster(data@images[[1]]@image)
    
    #ic_types$imagerow = max(ic_types$imagerow) - ic_types$imagerow + min(ic_types$imagerow)
    
    ####
    
    max_col = max(ic_types[,"imagecol"])
    min_col = min(ic_types[,"imagecol"])
    max_row = max(ic_types[,"imagerow"])
    min_row = min(ic_types[,"imagerow"])
    
    fig <- plot_ly()
    
    #Radius(data@images[[1]])
    for (i in 1:nrow(ic_types)) {
      incProgress(0.7/nrow(ic_types), detail = "Preparing scatterpie data")
      t = colnames(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)])
      v = round(as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)])/sum(as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]))*100,2)
      q = list()
      for (IC in t){
        q[IC] = values$Annotation[,'Annotation'][IC]
      }
      
      text_final = ""
      
      for(k in 1:length(t)) {
        text_final = paste(text_final,paste0(t[k]," : ",v[k],"% : ",q[k],"<br>"))
      }
      
      col_coordinates = (ic_types[i,"imagecol"])
      row_coordinates = max_row_img-(ic_types[i,"imagerow"])
      
      
      x = c((col_coordinates/max_col_img-(Radius(data@images[[1]]))/2),(col_coordinates/max_col_img+Radius(data@images[[1]])/2))
      y = c((row_coordinates/max_row_img-(Radius(data@images[[1]]))/2),(row_coordinates/max_row_img+Radius(data@images[[1]])/2))
      
      fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = colnames(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)])
                               , values = as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),
                               name = rownames(ic_types[i,]), domain = list(x = x, y = y),
                               showlegend = TRUE, textposition = "none", textinfo = "none",
                               text = text_final,
                               hovertemplate = paste0("%{text}",
                                                      "<extra></extra>"))
    }
    
    incProgress(0.1/nrow(ic_types), detail = "Preparing image")
    
    if (input$Spatial_display_image == TRUE){
      if (is.null(values$HD_image)){
        if(!is.null(values$low_image)){
          fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                                yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                                grid = list(columns = max_row_img, rows = max_col_img),
                                images = list(
                                  source = values$low_image,
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
      } else {
        fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                              yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                              grid = list(columns = max_row_img, rows = max_col_img),
                              images = list(
                                source = values$HD_image,
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
    } else {
      fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                            yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                            grid = list(columns = max_row_img, rows = max_col_img)
      )
    }
  }
  
  incProgress(0.1, detail = "Done")
  
  })
  
  return(fig)
})
