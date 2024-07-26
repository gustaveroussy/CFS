##############################
# Scatter pie
##############################

current_plot_spatial_scatter_pie <- reactive({
  
    if(!(input$Spatial_visualisation_comput)){
      return(NULL)
    }
  
    withProgress(message = 'Preparing Scatterpie', value = 0, {
    
    if("image" %in% slotNames(values$data@images[[input$Plot_image_spatial[1]]])){
      max_col_img = dim(values$data@images[[input$Plot_image_spatial[1]]]@image)[2]
      max_row_img = dim(values$data@images[[input$Plot_image_spatial[1]]]@image)[1]
    } else {
      max_col_img = max(values$data@images[[input$Plot_image_spatial[1]]]@coordinates[,"x"])
      max_row_img = max(values$data@images[[input$Plot_image_spatial[1]]]@coordinates[,"y"])
    }

    if (input$Scatter_pie_values_selected == "ica"){
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

    incProgress(0.1, detail = paste0("getting ",input$Scatter_pie_values_selected))
    
    if (input$Scatter_pie_values_selected == "ica"){
      if(!is.null(type)){
        ic_types=values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),type]
      }else{
        ic_types=values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),rownames(values$Annotation)[as.logical(values$Annotation[,"Use"])]]
      }
        
        ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
        
        #We normalize by the sum
        sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
        
        #we calculate the factor of size to reduce pie size where IC are low
        sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
        
        # We build the final object
        ic_types<-cbind(TissueCoordinates()[[input$Plot_image_spatial[1]]],ic_types) %>%  cbind(.,sum_IC)

    } else if (input$Scatter_pie_values_selected == "Metadata") {
      ic_types=values$data@meta.data[(rownames(values$data@meta.data) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),input$Scatter_pie_metadata_select]
      
      #We normalize by the sum
      sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
      
      #we calculate the factor of size to reduce pie size where IC are low
      sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
      
      # We build the final object
      ic_types<-cbind(TissueCoordinates()[[input$Plot_image_spatial[1]]],ic_types) %>%  cbind(.,sum_IC)
      
    } else {
      
      ic_types=values$data@reductions[[input$Scatter_pie_values_selected]]@cell.embeddings[(rownames(values$data@reductions[[input$Scatter_pie_values_selected]]@cell.embeddings) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),]
      
      colnames(ic_types) = values$data@misc$reduction_names[[input$Scatter_pie_values_selected]]
      
      ic_types = ic_types[,input$Scatter_pie_other_type]
      
      ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
      
      #We normalize by the sum
      sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
      
      #we calculate the factor of size to reduce pie size where IC are low
      sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
      
      # We build the final object
      ic_types<-cbind(TissueCoordinates()[[input$Plot_image_spatial[1]]],ic_types) %>%  cbind(.,sum_IC)
      
    }

      max_col = max(ic_types[,"imagecol"])
      max_row = max(ic_types[,"imagerow"])
      
      col_coordinates = (ic_types[,"imagecol"])
      row_coordinates = max_row_img-(ic_types[,"imagerow"])
      
      if(class(values$data@images[[input$Plot_image_spatial[1]]]) == "VisiumV1"){
          x = list((col_coordinates/max_col_img)-(Radius(values$data@images[[input$Plot_image_spatial[1]]])*ic_types[,"sum_IC"])/2,(col_coordinates/max_col_img)+(Radius(values$data@images[[input$Plot_image_spatial[1]]])*ic_types[,"sum_IC"])/2)
          y = list((row_coordinates/max_row_img)-(Radius(values$data@images[[input$Plot_image_spatial[1]]])*ic_types[,"sum_IC"])/2,(row_coordinates/max_row_img)+(Radius(values$data@images[[input$Plot_image_spatial[1]]])*ic_types[,"sum_IC"])/2)
      } else {
          x = list((col_coordinates/max_col_img)-(Radius(values$data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[,"sum_IC"])/2,(col_coordinates/max_col_img)+(Radius(values$data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[,"sum_IC"])/2)
          y = list((row_coordinates/max_row_img)-(Radius(values$data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[,"sum_IC"])/2,(row_coordinates/max_row_img)+(Radius(values$data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[,"sum_IC"])/2)
      }

      test = abs(as.vector(scale(abs(x[[2]]) - abs(x[[1]])))) > 1
      
      x[[1]] = x[[1]][test]
      x[[2]] = x[[2]][test]
      y[[1]] = y[[1]][test]
      y[[2]] = y[[2]][test]
      
      ic_types = ic_types[test,]
      
      fig <- plot_ly()
      for (i in 1:nrow(ic_types)){

        incProgress(0.7/nrow(ic_types), detail = "Preparing scatterpie data")
        
        if (input$Scatter_pie_values_selected == "ica"){
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
          
        } else if(input$Scatter_pie_values_selected == "Metadata"){
            if(!(sum(as.double(ic_types[i,input$Scatter_pie_metadata_select])) == 0)){
              v = round(as.double(ic_types[i,input$Scatter_pie_metadata_select])/sum(as.double(ic_types[i,input$Scatter_pie_metadata_select]))*100,2)
            } else {
              next
            }
          text_final = ""
          
          for(k in 1:length(input$Scatter_pie_metadata_select)) {
            text_final = paste(text_final,paste0(input$Scatter_pie_metadata_select[k]," : ",v[k],"%<br>"))
          }
        } else {
          
          if(!(sum(as.double(ic_types[i,input$Scatter_pie_other_type])) == 0)){
            v = round(as.double(ic_types[i,input$Scatter_pie_other_type])/sum(as.double(ic_types[i,input$Scatter_pie_other_type]))*100,2)
          } else {
            next
          }
          
          text_final = ""
          
          for(k in 1:length(input$Scatter_pie_other_type)) {
            text_final = paste(text_final,paste0(input$Scatter_pie_other_type[k]," : ",v[k],"%<br>"))
          }
          
        }

        if (input$Scatter_pie_values_selected == "ica"){
          fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = values$Annotation[colnames(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),'Annotation'],
                                   values = as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),
                                   name = rownames(ic_types[i,]), domain = list(x = c(x[[1]][i],x[[2]][i]), y = c(y[[1]][i],y[[2]][i])),
                                   showlegend = TRUE, textposition = "none", textinfo = "none",
                                   text = text_final,
                                   hovertemplate = paste0("%{text}",
                                                          "<extra></extra>"))
        } else if (input$Scatter_pie_values_selected == "Metadata") {

          fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = input$Scatter_pie_metadata_select,
                                   values = as.double(ic_types[i,input$Scatter_pie_metadata_select]),
                                   name = rownames(ic_types[i,]), domain = list(x = c(x[[1]][i],x[[2]][i]), y = c(y[[1]][i],y[[2]][i])),
                                   showlegend = TRUE, textposition = "none", textinfo = "none",
                                   text = text_final,
                                   hovertemplate = paste0("%{text}",
                                                          "<extra></extra>"))

        } else {
          fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = input$Scatter_pie_other_type,
                                   values = as.double(ic_types[i,input$Scatter_pie_other_type]),
                                   name = rownames(ic_types[i,]), domain = list(x = c(x[[1]][i],x[[2]][i]), y = c(y[[1]][i],y[[2]][i])),
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
  
    incProgress(0.1, detail = "Done")

  })
  
  return(fig)
})

#####for ggplot
current_plot_spatial_scatter_pie_ggplot <- reactive({
  req(values$data)
  
  if(!(input$Spatial_visualisation_comput)){
    return(NULL)
  }
  
  if("image" %in% slotNames(values$data@images[[input$Plot_image_spatial[1]]])){
    max_col_img = dim(values$data@images[[input$Plot_image_spatial[1]]]@image)[2]
    max_row_img = dim(values$data@images[[input$Plot_image_spatial[1]]]@image)[1]
  } else {
    max_col_img = max(values$data@images[[input$Plot_image_spatial[1]]]@coordinates[,"x"])
    max_row_img = max(values$data@images[[input$Plot_image_spatial[1]]]@coordinates[,"y"])
  }
  
  if (input$Scatter_pie_values_selected == "ica"){
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
  if (input$Scatter_pie_values_selected == "ica"){
    if(!is.null(type)){
      ic_types=values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),type]
    }else{
      ic_types=values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),rownames(values$Annotation)[as.logical(values$Annotation[,"Use"])]]
    }
    
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    
    #We normalize by the sum
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    
    #we calculate the factor of size to reduce pie size where IC are low
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    
    # We build the final object
    ic_types<-cbind(TissueCoordinates()[[input$Plot_image_spatial[1]]],ic_types) %>%  cbind(.,sum_IC)
    
  } else if (input$Scatter_pie_values_selected == "Metadata") {
    ic_types=values$data@meta.data[(rownames(values$data@meta.data) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),input$Scatter_pie_metadata_select]
    
    #We normalize by the sum
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    
    #we calculate the factor of size to reduce pie size where IC are low
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    
    # We build the final object
    ic_types<-cbind(TissueCoordinates()[[input$Plot_image_spatial[1]]],ic_types) %>%  cbind(.,sum_IC)
    
  } else {
    
    ic_types=values$data@reductions[[input$Scatter_pie_values_selected]]@cell.embeddings[(rownames(values$data@reductions[[input$Scatter_pie_values_selected]]@cell.embeddings) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),]
    
    colnames(ic_types) = values$data@misc$reduction_names[[input$Scatter_pie_values_selected]]
    
    ic_types = ic_types[,input$Scatter_pie_other_type]
    
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    
    #We normalize by the sum
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    
    #we calculate the factor of size to reduce pie size where IC are low
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    
    # We build the final object
    ic_types<-cbind(TissueCoordinates()[[input$Plot_image_spatial[1]]],ic_types) %>%  cbind(.,sum_IC)
    
  }
  
  if(class(values$data@images[[input$Plot_image_spatial[1]]]) == "VisiumV1"){
    ic_types$radius = (Radius(values$data@images[[input$Plot_image_spatial[1]]])*ic_types[,"sum_IC"]) * 100
  } else {
    ic_types$radius = (Radius(values$data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[,"sum_IC"]) * 100
  }
  
  img = values$data@images[[input$Plot_image_spatial[1]]]@image
  
  ic_types$imagerow = -(ic_types$imagerow)
  
  fig = ggplot() +
    background_image(img) +
    geom_scatterpie(aes(x=imagecol, y=imagerow, r=radius), data=ic_types,
                    cols=type) + coord_equal()  +
    xlim(25,ncol(img)-25) +
    ylim(-nrow(img)+25,-25)
  
  return(fig)
})