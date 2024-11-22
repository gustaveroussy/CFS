##----------------------------------------------------------------------------##
## boxplot for each samples
##----------------------------------------------------------------------------##

fig_distance_boxplot_niche_analysis <- function(){
  
  # req(values$distances)
  # req(values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]])
  req(input$boxplot_interaction_niche_search_with)
  req(input$boxplot_interaction_niche_comparison)
  
  samples <- images_names()
  
  df = lapply(samples,function(sample){
    coordinates = GetTissueCoordinates(values$data, sample);
    cell_names = rownames(coordinates);
    knn = knearneigh(coordinates, k=6, longlat = NULL, use_kd_tree=TRUE);
    neighbours = knn2nb(knn, row.names = NULL, sym = FALSE);
    listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL);
    x = lapply(values$data@misc$reduction_names[[input$boxplot_interaction_niche_comparison]],function(y){
      table_sample = values$data@reductions[[input$boxplot_interaction_niche_comparison]]@cell.embeddings;
      colnames(table_sample) = values$data@misc$reduction_names[[input$boxplot_interaction_niche_comparison]];
      table_sample = table_sample[cell_names,y]
      n = lee(values$data@reductions[["ica"]]@cell.embeddings[cell_names,input$boxplot_interaction_niche_search_with], table_sample, listw, length(table_sample), zero.policy=attr(listw, "zero.policy"));
      return(sum(n$localL))
    });
    names(x) = values$data@misc$reduction_names[[input$boxplot_interaction_niche_comparison]]
  return(x)
  })
  
  names(df) = samples
  
  df <- data.frame(matrix(unlist(df), nrow=length(df), byrow=TRUE))
  colnames(df) = values$data@misc$reduction_names[[input$boxplot_interaction_niche_comparison]]
  rownames(df) = samples
  
  if(nrow(df) > 0){
    
    df = tidyr::pivot_longer(df, cols = 1:length(values$data@misc$reduction_names[[input$boxplot_interaction_niche_comparison]]), names_to = "comp")
    df$samples = unlist(lapply(samples,function(sample){return(rep(sample,length(values$data@misc$reduction_names[[input$boxplot_interaction_niche_comparison]])))}))
      
    # lvls <- df %>%
    #   group_by(samples) %>%
    #   summarise(m = median(value)) %>%
    #   arrange(desc(m)) %>%
    #   pull(samples)
    
    fig <- plot_ly(df, x = ~comp, y = ~value, color = ~comp, boxmean = TRUE, boxpoints = input$boxplot_interaction_boxploint_type,
                   type = "box", text = ~samples)
    
  }
  
  return(fig)
}
