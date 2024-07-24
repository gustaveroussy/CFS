##############################
# Scatter pie
##############################

current_plot_spatial_scatter_pie <- reactive({
  
  if(!(input$Spatial_visualisation_comput)){
    return(NULL)
  }
  
  withProgress(message = 'Preparing Scatterpie', value = 0, {
  
  data <- values$data
  
  if("image" %in% slotNames(data@images[[input$Plot_image_spatial[1]]])){
    max_col_img = dim(data@images[[input$Plot_image_spatial[1]]]@image)[2]
    max_row_img = dim(data@images[[input$Plot_image_spatial[1]]]@image)[1]
  } else {
    max_col_img = max(data@images[[input$Plot_image_spatial[1]]]@coordinates[,"x"])
    max_row_img = max(data@images[[input$Plot_image_spatial[1]]]@coordinates[,"y"])
  }
  
  if (input$Scatter_pie_values_selected == "IC"){
    if (!is.null(input$Scatter_pie_cell_type)){
      
      for (n_cell_type in input$Plot_display_type_UMAP_choice) {
        if(is.null(type)) {
          type = values$annotation_for_output[["Type"]][[n_cell_type]]
        } else {
          type = append(type, values$annotation_for_output[["Type"]][[n_cell_type]])
        }
      }
      
      if (!is.null(input$Scatter_pie_IC_chosen_projection)){
        type=unique(c(input$Scatter_pie_IC_chosen_projection,type))
      }
      
    } else {
      
      if (!is.null(input$Scatter_pie_IC_chosen_projection)){
        type=input$Scatter_pie_IC_chosen_projection
      } else {
        type = NULL
      }
      
    }
  }
  
  incProgress(0.1, detail = "getting ICs")
  if (input$Scatter_pie_values_selected == "IC"){
    if(!is.null(type)){
      ic_types=data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),type]
    }else{
      ic_types=data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),rownames(values$Annotation)[as.logical(values$Annotation[,"Use"])]]
    }
      
      ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
      
      #We normalize by the sum
      sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
      #we calculate the factor of size to reduce pie size where IC are low
      sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
      
      #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
      # We build the final object
      ic_types<-cbind(TissueCoordinates()[[input$Plot_image_spatial[1]]],ic_types) %>%  cbind(.,sum_IC)
      
      #We build the plot
      
      #img<-grDevices::as.raster(data@images[[1]]@image)
      
      #ic_types$imagerow = max(ic_types$imagerow) - ic_types$imagerow + min(ic_types$imagerow)
      
      ####
  } else {
    ic_types=data@meta.data[(rownames(data@meta.data) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),input$Scatter_pie_metadata_select]
    
    #We normalize by the sum
    sum<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    
    #we calculate the factor of size to reduce pie size where IC are low
    sum=sqrt((rowSums(sum)/max(rowSums(sum))))
    
    #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    # We build the final object
    ic_types<-cbind(GetTissueCoordinates(data),ic_types) %>%  cbind(.,sum)
  }
  
      max_col = max(ic_types[,"imagecol"])
      max_row = max(ic_types[,"imagerow"])
      
      fig <- plot_ly()
      #Radius(data@images[[1]])
      for (i in 1:nrow(ic_types)) {
        if (input$Scatter_pie_values_selected == "IC"){
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
          
        } else {
          if(!(sum(as.double(ic_types[i,input$Scatter_pie_metadata_select])) == 0)){
            v = round(as.double(ic_types[i,input$Scatter_pie_metadata_select])/sum(as.double(ic_types[i,input$Scatter_pie_metadata_select]))*100,2)
          } else {
            next
          }
          
          text_final = ""
          
          for(k in 1:length(input$Scatter_pie_metadata_select)) {
            text_final = paste(text_final,paste0(input$Scatter_pie_metadata_select[k]," : ",v[k],"%<br>"))
          }
        }

        
        col_coordinates = (ic_types[i,"imagecol"])
        row_coordinates = max_row_img-(ic_types[i,"imagerow"])
        
        if(input$Scatter_pie_size_activate){
          x = c((col_coordinates/max_col_img)-(Radius(data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[i,"sum_IC"])/2,(col_coordinates/max_col_img)+(Radius(data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[i,"sum_IC"])/2)
          y = c((row_coordinates/max_row_img)-(Radius(data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[i,"sum_IC"])/2,(row_coordinates/max_row_img)+(Radius(data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[i,"sum_IC"])/2)
        } else {
          x = c((col_coordinates/max_col_img)-(Radius(data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids))/2,(col_coordinates/max_col_img)+(Radius(data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids))/2)
          y = c((row_coordinates/max_row_img)-(Radius(data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids))/2,(row_coordinates/max_row_img)+(Radius(data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids))/2)
        }
        
        if (input$Scatter_pie_values_selected == "IC"){
        fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = values$Annotation[colnames(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),'Annotation'],
                                 values = as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),
                                 name = rownames(ic_types[i,]), domain = list(x = x, y = y),
                                 showlegend = TRUE, textposition = "none", textinfo = "none",
                                 text = text_final,
                                 hovertemplate = paste0("%{text}",
                                                        "<extra></extra>"))
        } else {
          fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = input$Scatter_pie_metadata_select,
                                   values = as.double(ic_types[i,input$Scatter_pie_metadata_select]),
                                   name = rownames(ic_types[i,]), domain = list(x = x, y = y),
                                   showlegend = TRUE, textposition = "none", textinfo = "none",
                                   text = text_final,
                                   hovertemplate = paste0("%{text}",
                                                          "<extra></extra>"))
        }
        
      }
      incProgress(0.1, detail = "Preparing image")
      
      if (input$Spatial_display_image == TRUE){
        if (is.null(values$HD_image)){
          if(length(values$low_image) != 0){
            fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                                  yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                                  grid = list(columns = max_row_img, rows = max_col_img),
                                  images = list(
                                    source = values$low_image[[input$Plot_image_spatial[1]]],
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
        if(input$black_b_scatter_pie){
          fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                                yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                                grid = list(columns = max_row_img, rows = max_col_img)) %>%
            layout(plot_bgcolor='black') %>%
            layout(paper_bgcolor='black') %>%
            layout(legend = list(bgcolor = 'white'))
        }else{
          fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                                yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                                grid = list(columns = max_row_img, rows = max_col_img)
          )
        }
      }

  })
  incProgress(0.1, detail = "Done")
  return(fig)
})

#####for ggplot
current_plot_spatial_scatter_pie_ggplot <- reactive({
  req(values$data)
  
  data <- values$data
  
  if("image" %in% slotNames(data@images[[input$Plot_image_spatial[1]]])){
    max_col_img = dim(data@images[[input$Plot_image_spatial[1]]]@image)[2]
    max_row_img = dim(data@images[[input$Plot_image_spatial[1]]]@image)[1]
  } else {
    max_col_img = max(data@images[[input$Plot_image_spatial[1]]]@coordinates[,"x"])
    max_row_img = max(data@images[[input$Plot_image_spatial[1]]]@coordinates[,"y"])
  }
  
  if (input$Scatter_pie_values_selected == "IC"){
    if (!is.null(input$Scatter_pie_cell_type)){
      
      for (n_cell_type in input$Plot_display_type_UMAP_choice) {
        if(is.null(type)) {
          type = values$annotation_for_output[["Type"]][[n_cell_type]]
        } else {
          type = append(type, values$annotation_for_output[["Type"]][[n_cell_type]])
        }
      }
      
      if (!is.null(input$Scatter_pie_IC_chosen_projection)){
        type=unique(c(input$Scatter_pie_IC_chosen_projection,type))
      }
      
    } else {
      
      if (!is.null(input$Scatter_pie_IC_chosen_projection)){
        type=input$Scatter_pie_IC_chosen_projection
      } else {
        type = NULL
      }
      
    }
  }
  
  incProgress(0.1, detail = "getting ICs")
  if (input$Scatter_pie_values_selected == "IC"){
    if(!is.null(type)){
      ic_types=data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),type]
    }else{
      ic_types=data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),rownames(values$Annotation)[as.logical(values$Annotation[,"Use"])]]
    }
    
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    
    #We normalize by the sum
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    #we calculate the factor of size to reduce pie size where IC are low
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    
    #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    # We build the final object
    ic_types<-cbind(TissueCoordinates()[[input$Plot_image_spatial[1]]],ic_types) %>%  cbind(.,sum_IC)
    
    #We build the plot
    
    #img<-grDevices::as.raster(data@images[[1]]@image)
    
    #ic_types$imagerow = max(ic_types$imagerow) - ic_types$imagerow + min(ic_types$imagerow)
    
    ####
  } else {
    ic_types=data@meta.data[(rownames(data@meta.data) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),input$Scatter_pie_metadata_select]
    
    #We normalize by the sum
    sum<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    
    #we calculate the factor of size to reduce pie size where IC are low
    sum=sqrt((rowSums(sum)/max(rowSums(sum))))
    
    #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    # We build the final object
    ic_types<-cbind(GetTissueCoordinates(data),ic_types) %>%  cbind(.,sum)
  }
  
  ic_types$radius = (Radius(data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[,"sum_IC"])
  
  img = data@images[[1]]@image
  
  ic_types$imagerow = -(ic_types$imagerow)
  
  fig = ggplot() +
    background_image(img) +
    geom_scatterpie(aes(x=imagecol, y=imagerow, r=radius), data=ic_types,
                    cols=type) + coord_equal()
  
  return(fig)
})