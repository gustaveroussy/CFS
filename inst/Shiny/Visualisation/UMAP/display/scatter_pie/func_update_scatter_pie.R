##############################
# Scatter pie
##############################

current_plot_scatter_pie <- reactive({
  req(values$data)
  
  if(!(input$UMAP_visualisation_comput)){
    return(NULL)
  }
  
  withProgress(message = 'Preparing Scatterpie', value = 0, {
  
  if (!is.null(values$data)) {
    data <- values$data
    max_col_img = ceiling(max(data[[input$Visualisation_selected_dimred_to_display]]@cell.embeddings[,2]))
    max_row_img = ceiling(max(data[[input$Visualisation_selected_dimred_to_display]]@cell.embeddings[,1]))
    
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
        ic_types<-cbind(data[[input$Visualisation_selected_dimred_to_display]]@cell.embeddings[rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]]),],ic_types) %>%  cbind(.,sum_IC)
        
      } else if (input$Scatter_pie_values_selected == "Metadata") {
        types=data@meta.data[(rownames(data@meta.data) %in% rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]])),input$Scatter_pie_metadata_select]
        
        types<-apply(types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
        
        #We normalize by the sum
        sum<-apply(types,2,function(x){x=x/sum(x); return(x)})
        #we calculate the factor of size to reduce pie size where IC are low
        sum=sqrt((rowSums(sum)/max(rowSums(sum))))
        
        #types<-apply(types,1,function(x){x/sum(x); return(x)})
        # We build the final object
        ic_types<-cbind(data[[input$Visualisation_selected_dimred_to_display]]@cell.embeddings[rownames(TissueCoordinates()[[input$Plot_image_spatial[1]]]),],types) %>%  cbind(.,sum)
        
      }
        #We build the plot
        
        ####
        
        min_col = min(ic_types[,1])
        min_row = min(ic_types[,2])
        
        if(min_col < 0){
          ic_types[,1] = ic_types[,1] - min_col
        }
        
        if(min_row < 0){
          ic_types[,2] = ic_types[,2] - min_row
        }
        
        max_col = max(ic_types[,1])
        max_row = max(ic_types[,2])
        
        fig <- plot_ly()
        
        #Radius(data@images$slice1)
        for (i in 1:nrow(ic_types)) {
          incProgress(0.8/nrow(ic_types), detail = "Preparing scatterpie data")
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
          
          col_coordinates = ic_types[i,1]
          row_coordinates = ic_types[i,2]
          
          if(input$Scatter_pie_size_activate){
            x = c((col_coordinates/max_col)-(Radius(data@images[[input$Plot_image_spatial[1]]])*ic_types[i,"sum_IC"])/2,(col_coordinates/max_col)+(Radius(data@images[[input$Plot_image_spatial[1]]])*ic_types[i,"sum_IC"])/2)
            y = c((row_coordinates/max_row)-(Radius(data@images[[input$Plot_image_spatial[1]]])*ic_types[i,"sum_IC"])/2,(row_coordinates/max_row)+(Radius(data@images[[input$Plot_image_spatial[1]]])*ic_types[i,"sum_IC"])/2)
          } else {
            x = c((col_coordinates/max_col)-(Radius(data@images[[input$Plot_image_spatial[1]]]))/2,(col_coordinates/max_col)+(Radius(data@images[[input$Plot_image_spatial[1]]]))/2)
            y = c((row_coordinates/max_row)-(Radius(data@images[[input$Plot_image_spatial[1]]]))/2,(row_coordinates/max_row)+(Radius(data@images[[input$Plot_image_spatial[1]]]))/2)
          }
          if (input$Scatter_pie_values_selected == "IC"){
            
            fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = values$Annotation[names(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),'Annotation'],
                                     values = as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),
                                     name = rownames(ic_types[i,]), domain = list(x = x, y = y),
                                     showlegend = TRUE, textposition = "none", textinfo = "none",
                                     text = text_final,
                                     hovertemplate = paste0("%{text}",
                                                            "<extra></extra>"))
            
          } else {
            
            fig <- fig %>% add_trace(type = 'pie', data = types, labels = input$Scatter_pie_metadata_select,
                                     values = as.double(types[i,input$Scatter_pie_metadata_select]),
                                     name = rownames(types[i,]), domain = list(x = x, y = y),
                                     showlegend = TRUE, textposition = "none", textinfo = "none",
                                     text = text_final,
                                     hovertemplate = paste0("%{text}",
                                                            "<extra></extra>"))
            
          }
        }
      }

    incProgress(0.1, detail = "Done")
    
    return(fig)
    
    } else {
      return(NULL)
    }
  })
})
