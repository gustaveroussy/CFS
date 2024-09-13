##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["Boxplot_distances_plot"]] <- plotly::renderPlotly({
  req(fig_distance_boxplot())
  
  return(fig_distance_boxplot())
})

output[["Boxplot_distances_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("Boxplot_distances_plot",
                         width = "auto",
                         height = "85vh")
  )
})

fig_distance_boxplot <- reactive({
  
  req(values$distances)
  req(values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]])
  
  samples <- names(values$data@images)
  
  if(input$boxplot_interaction_compare_with_randomized){
    # get values
    if (input$choose_distances_to_determine == "Genes") {
      
      table_sample = raster::t(GetAssayData(values$data))
      
    } else if (input$choose_distances_to_determine != "Genes") {
      
      table_sample = values$data@reductions[[input$choose_distances_to_determine]]@cell.embeddings
      
      if(input$choose_distances_to_determine != "ica"){
        colnames(table_sample) = values$data@misc$reduction_names[[input$choose_distances_to_determine]]
      }
      
      if(input$use_positive_values_for_distances){
        table_sample[table_sample < 0] = 0
      }
      
    }
    
    if (input$choose_distances_to_determine_2 == "Genes") {
      
      table_sample_2 = raster::t(GetAssayData(values$data))
      
    } else if (input$choose_distances_to_determine_2 != "Genes") {
      
      table_sample_2 = values$data@reductions[[input$choose_distances_to_determine_2]]@cell.embeddings
      
      if(input$choose_distances_to_determine_2 != "ica"){
        colnames(table_sample_2) = values$data@misc$reduction_names[[input$choose_distances_to_determine_2]]
      }
      
      if(input$use_positive_values_for_distances){
        table_sample_2[table_sample_2 < 0] = 0
      }
      
    }
    
  }
  
  df = lapply(samples,function(sample){table = values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[input$choose_method_for_distances]]; colnames(table) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2,"weight");return(table)})
  df = lapply(df,function(t){t$scaled_weight = scale(t$weight);return(t)})
  
  names(df) = samples
  Annotation = as.data.frame(values$Annotation)
  
  df = lapply(df,function(t){t$lr <- paste0(t[,1],"_",t[,2]);t[order(t$scaled_weight,decreasing = TRUE),]})
  
  combinations = unique(unlist(lapply(df,function(t){return(t[(t[,4] > 3),5])})))
  
  df = lapply(df,function(t){return(t[t[,5] %in% combinations,])})
  
  if(input$boxplot_interaction_compare_with_randomized){
    random = lapply(names(df),function(sample){
      # get distances
      knn = knearneigh(GetTissueCoordinates(values$data, sample), k=6, longlat = NULL, use_kd_tree=TRUE);
      neighbours = knn2nb(knn, row.names = NULL, sym = FALSE);
      listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL);
      
      # get table to use
      table_sample_use = table_sample[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample)),]
      table_sample_2_use = table_sample_2[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample_2)),]
      
      weight = c()
      for(i in 1:nrow(df[[sample]])){
        weight = c(weight,mean(lee.mc(as.double(table_sample_use[,df[[sample]][i,2]]), as.double(table_sample_use[,df[[sample]][i,2]]), listw, 100, zero.policy=NULL, alternative="greater", na.action=na.fail, spChk=NULL, return_boot=FALSE)[["res"]]))
        
      }
      
      # get randomized values
      return(weight)
    })
    
    i = 1
    for(names in names(df)){
      df[[names]] = cbind(df[[names]],random[i])
      colnames(df[[names]])[5] = "lr"
      colnames(df[[names]])[6] = "randomized_value"
      i = i + 1
    }
  }
  
  df = dplyr::bind_rows(df, .id = "samples")
  
  if(!is.null(input$boxplot_interaction_filter_1)){
      df = df[df[,2] %in% input$boxplot_interaction_filter_1,]
  }
  
  if(!is.null(input$boxplot_interaction_filter_2)){
    df = df[df[,3] %in% input$boxplot_interaction_filter_2,]
  }
  
  if(nrow(df) > 0){
    
    IC_vect = values$IC_names
    
    # filter ica
    if("ica" %in% names(df)){
      df = df[as.vector(df[,"ica"])[[1]] %in% IC_vect,]
    }
    if("ica_2" %in% names(df)){
      df = df[as.vector(df[,"ica_2"])[[1]] %in% IC_vect,]
    }
    
    if(input$boxplot_interaction_compare_with_randomized){
      
      df = pivot_longer(df,c("weight","randomized_value"))
      
      fig <- plot_ly(df, x = ~lr, y = ~value, color = input$boxplot_interaction_compare_with_randomized~name, boxmean = TRUE, boxpoints = input$boxplot_interaction_boxploint_type,
                     type = "box", text = ~samples)
      
      fig <- fig %>% layout(boxmode = "group")
    } else {
      lvls <- df %>%
        group_by(lr) %>%
        summarise(m = median(weight)) %>%
        arrange(desc(m)) %>%
        pull(lr)
      
      fig <- plot_ly(df, x = ~lr, y = ~weight, boxmean = TRUE, boxpoints = input$boxplot_interaction_boxploint_type,
                     type = "box", text = ~samples)
    }
      
  }
    
  return(fig)
})
