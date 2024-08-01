##############################
# Scatter pie
##############################

current_plot_scatter_pie <- reactive({
  req(values$data)
  req(input$Plot_image_spatial)
  
  if(!(input$UMAP_visualisation_comput)){
    return(NULL)
  }
  
  withProgress(message = 'Preparing Scatterpie', value = 0, {
    
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
        ic_types=values$data@reductions$ica@cell.embeddings[,type]
      }else{
        ic_types=values$data@reductions$ica@cell.embeddings[,rownames(values$Annotation)[as.logical(values$Annotation[,"Use"])]]
      }
      
      ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
      
      #We normalize by the sum
      sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
      
      #we calculate the factor of size to reduce pie size where IC are low
      sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
      
      # We build the final object
      ic_types<- cbind(values$data[[input$Visualisation_selected_dimred_to_display]]@cell.embeddings,ic_types) %>%  cbind(.,sum_IC)
      
    } else if (input$Scatter_pie_values_selected == "Metadata") {
      ic_types=values$data@meta.data[,input$Scatter_pie_metadata_select]
      
      #We normalize by the sum
      sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
      
      #we calculate the factor of size to reduce pie size where IC are low
      sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
      
      # We build the final object
      ic_types <- cbind(values$data[[input$Visualisation_selected_dimred_to_display]]@cell.embeddings,ic_types) %>%  cbind(.,sum_IC)
      
    } else {
      
      ic_types=values$data@reductions[[input$Scatter_pie_values_selected]]@cell.embeddings
      
      colnames(ic_types) = values$data@misc$reduction_names[[input$Scatter_pie_values_selected]]
      
      ic_types = ic_types[,input$Scatter_pie_other_type]
      
      ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
      
      #We normalize by the sum
      sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
      
      #we calculate the factor of size to reduce pie size where IC are low
      sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
      
      # We build the final object
      ic_types <- cbind(values$data[[input$Visualisation_selected_dimred_to_display]]@cell.embeddings,ic_types) %>%  cbind(.,sum_IC)
      
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
    
    col_coordinates = (ic_types[,1])
    row_coordinates = (ic_types[,2])
    
    if(class(values$data@images[[input$Plot_image_spatial[1]]]) == "VisiumV1"){
      x = list((col_coordinates/max_col)-(Radius(values$data@images[[input$Plot_image_spatial[1]]])*ic_types[,"sum_IC"])/2,(col_coordinates/max_col)+(Radius(values$data@images[[input$Plot_image_spatial[1]]])*ic_types[,"sum_IC"])/2)
      y = list((row_coordinates/max_row)-(Radius(values$data@images[[input$Plot_image_spatial[1]]])*ic_types[,"sum_IC"])/2,(row_coordinates/max_row)+(Radius(values$data@images[[input$Plot_image_spatial[1]]])*ic_types[,"sum_IC"])/2)
    } else {
      x = list((col_coordinates/max_col)-(Radius(values$data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[,"sum_IC"])/2,(col_coordinates/max_col)+(Radius(values$data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[,"sum_IC"])/2)
      y = list((row_coordinates/max_row)-(Radius(values$data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[,"sum_IC"])/2,(row_coordinates/max_row)+(Radius(values$data@images[[input$Plot_image_spatial[1]]]@boundaries$centroids)*ic_types[,"sum_IC"])/2)
    }
    
    test_1 = abs(as.vector(scale(abs(x[[2]]) - abs(x[[1]])))) > 1
    test_2 = ic_types[,"sum_IC"] != 0
    
    test = test_1 | test_2
    
    x[[1]] = x[[1]][test]
    x[[2]] = x[[2]][test]
    y[[1]] = y[[1]][test]
    y[[2]] = y[[2]][test]
    
    ic_types = ic_types[test,]
    
    fig <- plot_ly()
    for (i in 1:nrow(ic_types)) {
      
      if (input$Scatter_pie_values_selected == "ica"){
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
        label = names(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)])
        
        fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = label,
                                 values = as.double(ic_types[i,grep('IC_',colnames(ic_types))][which(ic_types[i,grep('IC_',colnames(ic_types))] != 0)]),
                                 name = rownames(ic_types)[i], domain = list(x = c(x[[1]][i],x[[2]][i]), y = c(y[[1]][i],y[[2]][i])),
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

    incProgress(0.1, detail = "Done")
    
    return(fig)
    
  })
})
