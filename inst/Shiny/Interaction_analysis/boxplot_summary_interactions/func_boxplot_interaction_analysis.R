##----------------------------------------------------------------------------##
## boxplot for each samples
##----------------------------------------------------------------------------##

fig_distance_boxplot_analysis <- function(){
  
  # req(values$distances)
  # req(values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]])
  req(input$boxplot_interaction_1)
  req(input$boxplot_interaction_2)
  
  samples <- images_names()
  
  df = lapply(samples,function(sample){
    coordinates = GetTissueCoordinates(values$data, sample);
    cell_names = rownames(coordinates);
    knn = knearneigh(coordinates, k=6, longlat = NULL, use_kd_tree=TRUE);
    neighbours = knn2nb(knn, row.names = NULL, sym = FALSE);
    listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL);
    
    table_1 = values$data@reductions[[input$boxplot_interaction_selection_1]]@cell.embeddings
    table_1 = table_1[cell_names,]
    if(input$boxplot_interaction_selection_1 != "ica"){
      colnames(table_1) = values$data@misc$reduction_names[[input$boxplot_interaction_selection_1]]
    }
    
    table_2 = values$data@reductions[[input$boxplot_interaction_selection_2]]@cell.embeddings
    table_2 = table_2[cell_names,]
    if(input$boxplot_interaction_selection_2 != "ica"){
      colnames(table_2) = values$data@misc$reduction_names[[input$boxplot_interaction_selection_2]]
    }
    
    x = lapply(input$boxplot_interaction_1,function(y){
      m = lapply(input$boxplot_interaction_2,function(z){
        n = lee(table_1[,y], table_2[,z], listw, length(table_2), zero.policy=attr(listw, "zero.policy"));
        if(input$boxplot_interaction_selection_value == "sumLee"){
          return(sum(n$localL))
        } else {
          return(sum(n$L))
        }
      })
      names(m) = input$boxplot_interaction_2
      return(m)
    });
    
    names(x) = input$boxplot_interaction_1
  return(x)
  })
  
  names(df) = samples
  
  if(length(df) > 0){
    
    l = list()
    
    for(i in names(df)){
      l[[i]] = data.frame(t(sapply(df[[i]],c)))
      l[[i]] = as.data.table(tidyr::pivot_longer(l[[i]], cols = 1:ncol(l[[i]]), names_to = input$boxplot_interaction_selection_2))
      l[[i]][,input$boxplot_interaction_selection_1] = unlist(lapply(input$boxplot_interaction_1,function(v){return(rep(v,length(input$boxplot_interaction_2)))}))
      l[[i]][,"sample"] = i
      l[[i]][,"value"] = as.double(unlist(l[[i]][,"value"]))
    }
    
    l = Reduce(rbind,l)
    
    fig <- plot_ly(l, x = unlist(l[,3]), y = ~value, color = unlist(l[,1]), boxmean = TRUE, boxpoints = input$boxplot_interaction_boxploint_type,
                   type = "box", text = ~sample)
    fig <- fig %>% layout(boxmode = "group")
    
  }
  
  return(fig)
}
