##----------------------------------------------------------------------------##
## boxplot for each samples
##----------------------------------------------------------------------------##

fig_distance_boxplot_niche_analysis <- function(){
  req(input$boxplot_niche_interaction_selection_1)
  req(input$boxplot_niche_interaction_selection_2)

  samples <- images_names()
  
  annotation = values$data@misc$annotation

  df = lapply(samples,function(sample){
    coordinates = TissueCoordinates()[[sample]];
    cell_names = rownames(coordinates);
    knn = knearneigh(coordinates, k=6, longlat = NULL, use_kd_tree=TRUE);
    neighbours = knn2nb(knn, row.names = NULL, sym = FALSE);
    listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL);
    
    # input$boxplot_niche_interaction_selection_2 = "Type"
    # input$boxplot_niche_interaction_selection_2 = "Stroma"
    
    table = values$data@misc$distances[["ica_ica"]][[sample]][["Lee"]]
    
    x = lapply(input$boxplot_niche_interaction_selection_1,function(y){
        m = lapply(input$boxplot_niche_interaction_selection_2,function(z){
          
          ica_associated_with_types = rownames(annotation)[annotation[,input$boxplot_niche_interaction_selection_2] == input$boxplot_niche_interaction_2]
          table_2 = table[table$ica %in% input$boxplot_niche_interaction_selection_1 & table$ica_2 %in% ica_associated_with_types,]
          sumweights = sum(table_2$weight)/nrow(table_2)
          
          return(sumweights)
        })
        names(m) = input$boxplot_niche_interaction_selection_2
        return(m)
      });
    

    names(x) = input$boxplot_niche_interaction_selection_1
    return(x)
  })
  
  names(df) = samples
  
  if(length(df) > 0){

    l = list()

    for(i in names(df)){
      l[[i]] = data.frame(t(sapply(df[[i]],c)))
      l[[i]] = as.data.table(tidyr::pivot_longer(l[[i]], cols = 1:ncol(l[[i]]), names_to = input$boxplot_niche_interaction_selection_2))
      l[[i]][,"ica"] = unlist(lapply(input$boxplot_niche_interaction_selection_1,function(v){return(rep(v,length(input$boxplot_niche_interaction_selection_2)))}))
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
