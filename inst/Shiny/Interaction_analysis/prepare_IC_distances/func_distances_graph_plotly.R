##----------------------------------------------------------------------------##
## functions to create plotly object for graph
##----------------------------------------------------------------------------##

create_distance_graph_plotly = function(G = NULL, layout = "kk", n_dim = 2,edges_list = NULL, tree_table = NULL){
  ##### create edgelist #####
  
  if(input$choose_distances_to_determine == input$choose_distances_to_determine_2){
    colnames(edges_list) = c(input$choose_distances_to_determine,paste0(input$choose_distances_to_determine_2,"_2"))
  } else {
    colnames(edges_list) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
  }
  
  ##### create layout #####
  
  if(layout %in% c("fr","kk","mds")){
    layout <- do.call(paste0("layout_with_",layout),list(graph = G, dim = n_dim))
  } else if(layout %in% c("sugiyama")){
    layout <- do.call(paste0("layout_with_",layout),list(graph = G))$layout
  } else {
    layout <- do.call(paste0("layout_with_",layout),list(graph = G))
  }
  
  #### create plot #####
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  
  fig = plot_ly(source = "S")
  
  rownames(layout) = names(V(G))
  
  ##### create coordinates ####
  edges_list[,"x_start"] = as.double(layout[edges_list[,1],1])
  edges_list[,"x_end"] = as.double(layout[edges_list[,2],1])
  edges_list[,"y_start"] = as.double(layout[edges_list[,1],2])
  edges_list[,"y_end"] = as.double(layout[edges_list[,2],2])
  edges_list[,"x_median"] = (edges_list[,"x_start"] + edges_list[,"x_end"])/2
  edges_list[,"y_median"] = (edges_list[,"y_start"] + edges_list[,"y_end"])/2
  edges_list[,"weight"] = tree_table[(paste0(tree_table[,1],"_",tree_table[,2]) %in% paste0(edges_list[,1],"_",edges_list[,2]) | paste0(tree_table[,2],"_",tree_table[,1]) %in% paste0(edges_list[,1],"_",edges_list[,2])) ,"weight"]
  
  if (input$choose_sample_for_distances == "All"){
    edges_list[,"N"] = tree_table[(paste0(tree_table[,1],"_",tree_table[,2]) %in% paste0(edges_list[,1],"_",edges_list[,2]) | paste0(tree_table[,2],"_",tree_table[,1]) %in% paste0(edges_list[,1],"_",edges_list[,2])) ,"N"]
  }
  
  if(n_dim == 3){
    edges_list[,"z_start"] = as.double(layout[edges_list[,1],3])
    edges_list[,"z_end"] = as.double(layout[edges_list[,2],3])
    edges_list[,"z_median"] = (edges_list[,"z_start"] + edges_list[,"z_end"])/2
  }
  
  if(input$distances_use_log_for_graph){
    w = log(edges_list[,"weight"] + 1)
    limits=range(w)
    pal = viridis::viridis(100)
    edges_list[,"colors"] = pal[findInterval(w,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
  } else {
    limits=range(edges_list[,"weight"])
    pal = viridis::viridis(100)
    edges_list[,"colors"] = pal[findInterval(edges_list[,"weight"],seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
  }
  
  edges_list[,"text"] = paste0(edges_list[,1]," <-> ",edges_list[,2],"<br>Value : ",edges_list$weight)
  
  ##### create edges #####
  if(n_dim == 2){
    for(i in 1:nrow(edges_list)){
      fig = fig %>% add_segments(data = edges_list[i,],
                                 x = ~x_start,
                                 xend = ~x_median,
                                 y = ~y_start,
                                 yend = ~y_median,
                                 line=list(color = ~colors,
                                           width = if(input$choose_sample_for_distances == "All"){input$choose_edges_size_for_distances * edges_list[i,"N"]}else{input$choose_edges_size_for_distances}
                                 )
      )
      
      fig = fig %>% add_segments(data = edges_list[i,],
                                 x = ~x_median,
                                 xend = ~x_end,
                                 y = ~y_median,
                                 yend = ~y_end,
                                 line=list(color=~colors,
                                           width = if(input$choose_sample_for_distances == "All"){input$choose_edges_size_for_distances * edges_list[i,"N"]}else{input$choose_edges_size_for_distances}
                                 ),
                                 customdata = ~text,
                                 hovertemplate = paste0("%{customdata}",
                                                        "<extra></extra>")
      )
    }
  } else if (n_dim == 3){
    for(i in 1:nrow(edges_list)){
      fig = fig %>% add_trace(x=c(as.double(edges_list[i,"x_start"]), as.double(edges_list[i,"x_median"])), y=c(as.double(edges_list[i,"y_start"]), as.double(edges_list[i,"y_median"])), z=c(as.double(edges_list[i,"z_start"]), as.double(edges_list[i,"z_median"])),
                              type="scatter3d", mode="lines",
                              line=list(color=edges_list[i,"colors"],
                                        width = if(input$choose_sample_for_distances == "All"){input$choose_edges_size_for_distances * edges_list[i,"N"]}else{input$choose_edges_size_for_distances}
                              ),
                              customdata = edges_list[i,"text"],
                              hovertemplate = paste0("%{customdata}",
                                                     "<extra></extra>")
      )
      
      fig = fig %>% add_trace(x=c(as.double(edges_list[i,"x_median"]), as.double(edges_list[i,"x_end"])), y=c(as.double(edges_list[i,"y_median"]), as.double(edges_list[i,"y_end"])), z=c(as.double(edges_list[i,"z_median"]), as.double(edges_list[i,"z_end"])),
                              type="scatter3d", mode="lines",
                              line=list(color=edges_list[i,"colors"],
                                        width = if(input$choose_sample_for_distances == "All"){input$choose_edges_size_for_distances * edges_list[i,"N"]}else{input$choose_edges_size_for_distances}
                              ),
                              customdata = edges_list[i,"text"],
                              hovertemplate = paste0("%{customdata}",
                                                     "<extra></extra>")
      )
      
    }
  }
  
  ##### create vertices colors #####
  if(input$choose_distances_to_determine != "Genes" & input$choose_distances_to_determine_2 != "Genes"){
    annotation = c()
    annotation_2 = c()
    
    for(i in names(V(G))){
      
      if(i %in% rownames(values$Annotation)){
        annotation = c(annotation,values$Annotation[i,input$choose_vertices_color_for_distances])
        annotation_2 = c(annotation_2,values$Annotation[i,input$choose_vertices_annotation_for_distances])
        
      } else {
        annotation = c(annotation,"")
        annotation_2 = c(annotation_2,"")
        
      }
      
    }
    
    annotation_3 = paste0("Color: ", annotation,"<br>Annotation: ",annotation_2)
    
  } else if (input$choose_distances_to_determine == "Genes" | input$choose_distances_to_determine_2 == "Genes") {
    
    annotation = c()
    annotation_2 = c()
    
    for(i in names(V(G))){
      if(i %in% rownames(values$data@assays$SCT@data)){
        ICs = lapply(values$data@misc$GeneAndStat$Contrib_gene,function(x){x = x[x$Sig > 0,];return(i %in% x$gene)})
        ICs = names(ICs[ICs == TRUE])
        ICs = paste0(ICs, " : ", values$Annotation[rownames(values$Annotation) %in% ICs,"Type"] ," : ",values$Annotation[rownames(values$Annotation) %in% ICs,"Annotation"])
        annotation = c(annotation,paste(ICs,collapse = "<br>"))
        annotation_2 = c(annotation_2,"")
      } else {
        if(i %in% rownames(values$Annotation)){
          annotation = c(annotation,values$Annotation[i,input$choose_vertices_color_for_distances])
          annotation_2 = c(annotation_2,values$Annotation[i,input$choose_vertices_annotation_for_distances])
          
        } else {
          annotation = c(annotation,"")
          annotation_2 = c(annotation_2,"")
          
          
        }
      }
      
    }
    
    annotation_3 = paste0("Color: ", annotation,"<br>Annotation: ",annotation_2)
    
  }
  
  colors = rep(values$palette,ceiling(length(unique(annotation))/length(palette())))[as.numeric(as.factor(annotation))]
  
  ##### create vertices #####
  if(n_dim == 2){
    fig = fig %>%
      add_markers(x = ~layout[,1], y = ~layout[,2],
                  marker = list(
                    color = colors,
                    size = input$choose_vertices_size_for_distances
                  ),
                  opacity = 1,
                  text = names(V(G)),
                  customdata = annotation_3,
                  hovertemplate = paste0(input$choose_distances_to_determine, " : %{text}<br>",
                                         "%{customdata}",
                                         "<extra></extra>")
      ) %>% layout(
        title = 'Distance graph',
        xaxis = axis,
        yaxis = axis,
        hoverlabel = list(align = "left")
      ) %>%
      add_text(x = ~layout[,1], y = ~layout[,2], text = names(V(G)), textposition = "center") %>%
      hide_legend()
  } else {
    fig = fig %>%
      add_markers(x = ~layout[,1], y = ~layout[,2], z = ~layout[,3],
                  marker = list(
                    color = colors,
                    size = input$choose_vertices_size_for_distances
                  ),
                  opacity = 1,
                  text = names(V(G)),
                  customdata = annotation_3,
                  hovertemplate = paste0(input$choose_distances_to_determine, " : %{text}<br>",
                                         "%{customdata}",
                                         "<extra></extra>")
      ) %>% layout(
        title = 'Distance graph',
        xaxis = axis,
        yaxis = axis,
        hoverlabel = list(align = "left")
      ) %>%
      add_text(x = ~layout[,1], y = ~layout[,2], z = ~layout[,3], text = names(V(G)), textposition = "center") %>%
      hide_legend()
  }
  
  return(fig)
}