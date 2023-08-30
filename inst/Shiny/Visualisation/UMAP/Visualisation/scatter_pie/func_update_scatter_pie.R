##############################
# Scatter pie
##############################

current_plot_scatter_pie <- reactive({
  req(values$data)
  
  if(!(input$UMAP_visualisation_comput)){
    return(NULL)
  }
  
  if (!is.null(values$data)) {
    data <- values$data
    max_col_img = ceiling(max(data[["umap"]]@cell.embeddings[,'UMAP_2']))
    max_row_img = ceiling(max(data[["umap"]]@cell.embeddings[,'UMAP_1']))
    
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
    
    if(!is.null(type)){
  
      ic_types=data@reductions$ica@cell.embeddings[,type]
      
      ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
      
      #We normalize by the sum
      sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
      #we calculate the factor of size to reduce pie size where IC are low
      sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
      
      #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
      # We build the final object
      ic_types<-cbind(data[["umap"]]@cell.embeddings,ic_types) %>%  cbind(.,sum_IC)
      
      #We build the plot
      
      #img<-grDevices::as.raster(data@images$slice1@image)
      
      #ic_types$imagerow = max(ic_types$imagerow) - ic_types$imagerow + min(ic_types$imagerow)
      
      ####
      
      min_col = min(ic_types[,"UMAP_1"])
      min_row = min(ic_types[,"UMAP_2"])
      
      if(min_col < 0){
        ic_types[,"UMAP_1"] = ic_types[,"UMAP_1"] - min_col
      }
      
      if(min_row < 0){
        ic_types[,"UMAP_2"] = ic_types[,"UMAP_2"] - min_row
      }
      
      max_col = max(ic_types[,"UMAP_1"])
      max_row = max(ic_types[,"UMAP_2"])
      
      fig <- plot_ly()
      
      #Radius(data@images$slice1)
      for (i in 1:nrow(ic_types)) {
        
        t = names(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)])
        v = round(as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)])/sum(as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]))*100,2)
        q = list()
        for (IC in t){
          q[IC] = values$Annotation[,'Annotation'][IC]
        }
          
        text_final = ""
        
        for(k in 1:length(t)) {
          text_final = paste(text_final,paste0(t[k]," : ",v[k],"% : ",q[k],"<br>"))
        }
        
        col_coordinates = ic_types[i,"UMAP_1"]
        row_coordinates = ic_types[i,"UMAP_2"]
        
        x = c((col_coordinates/max_col)-(Radius(data@images$slice1)*ic_types[i,"sum_IC"])/2,(col_coordinates/max_col)+(Radius(data@images$slice1)*ic_types[i,"sum_IC"])/2)
        y = c((row_coordinates/max_row)-(Radius(data@images$slice1)*ic_types[i,"sum_IC"])/2,(row_coordinates/max_row)+(Radius(data@images$slice1)*ic_types[i,"sum_IC"])/2)
        
        fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = values$Annotation[names(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),'Annotation']
                                 , values = as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),
                                 name = rownames(ic_types[i,]), domain = list(x = x, y = y),
                                 showlegend = TRUE, textposition = "none", textinfo = "none",
                                 text = text_final,
                                 hovertemplate = paste0("%{text}",
                                                        "<extra></extra>"))
      }
      
      # fig = plotly::plot_ly()
      # working on mignature pie
      # fig <- fig %>% add_markers(x = NULL, y = NULL, showlegend = F,
      #                            xaxis = 'x2', yaxis = 'y2') %>%
      #   layout(xaxis2 = list(domain = c(0.7, 0.95), anchor='y2', range=c(-1,1), title = "Effect Size",
      #                        zeroline = T, showticklabels = T),
      #          yaxis2 = list(domain = c(0.5, 0.95), anchor='x2', title = NA, zeroline = F,
      #                        showticklabels = T))
      
    } else {
      ic_types=data@reductions$ica@cell.embeddings
      
      ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
      
      #We normalize by the sum
      sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
      #we calculate the factor of size to reduce pie size where IC are low
      sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
      
      #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
      # We build the final object
      ic_types<-cbind(data[["umap"]]@cell.embeddings,ic_types) %>%  cbind(.,sum_IC)
      
      #We build the plot
      
      #img<-grDevices::as.raster(data@images$slice1@image)
      
      #ic_types$imagerow = max(ic_types$imagerow) - ic_types$imagerow + min(ic_types$imagerow)
      
      ####
      
      min_col = min(ic_types[,"UMAP_1"])
      min_row = min(ic_types[,"UMAP_2"])
      
      if(min_col < 0){
        ic_types[,"UMAP_1"] = ic_types[,"UMAP_1"] - min_col
      }
      
      if(min_row < 0){
        ic_types[,"UMAP_2"] = ic_types[,"UMAP_2"] - min_row
      }
      
      max_col = max(ic_types[,"UMAP_1"])
      max_row = max(ic_types[,"UMAP_2"])
      
      fig <- plot_ly()
      
      #Radius(data@images$slice1)
      for (i in 1:nrow(ic_types)) {
        
        t = names(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)])
        v = round(as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)])/sum(as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]))*100,2)
        q = list()
        for (IC in t){
          q[IC] = values$Annotation[,'Annotation'][IC]
        }
        
        text_final = ""
        
        for(k in 1:length(t)) {
          text_final = paste(text_final,paste0(t[k]," : ",v[k],"% : ",q[k],"<br>"))
        }
        
        col_coordinates = ic_types[i,"UMAP_1"]
        row_coordinates = ic_types[i,"UMAP_2"]
        
        x = c((col_coordinates/max_col)-(Radius(data@images$slice1)*ic_types[i,"sum_IC"])/2,(col_coordinates/max_col)+(Radius(data@images$slice1)*ic_types[i,"sum_IC"])/2)
        y = c((row_coordinates/max_row)-(Radius(data@images$slice1)*ic_types[i,"sum_IC"])/2,(row_coordinates/max_row)+(Radius(data@images$slice1)*ic_types[i,"sum_IC"])/2)
        
        fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = values$Annotation[names(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),'Annotation']
                                 , values = as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),
                                 name = rownames(ic_types[i,]), domain = list(x = x, y = y),
                                 showlegend = TRUE, textposition = "none", textinfo = "none",
                                 text = text_final,
                                 hovertemplate = paste0("%{text}",
                                                        "<extra></extra>"))
      }
    }
    return(fig)
  } else {
    return(NULL)
  }
})
