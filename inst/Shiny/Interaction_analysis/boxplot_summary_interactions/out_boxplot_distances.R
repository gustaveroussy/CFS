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
  
  df = lapply(samples,function(sample){table = values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[input$choose_method_for_distances]]; colnames(table) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2,"weight");return(table)})
  df = lapply(df,function(d){d$scaled_weight = scale(d$weight);return(d)})
  names(df) = samples
  Annotation = as.data.frame(values$Annotation)
  
  
  df = dplyr::bind_rows(df, .id = "samples")
  
  df$lr <- paste0(df[,2],"_",df[,3])
  
  df = df[order(df$weight,decreasing = TRUE),]
  
  if(!is.null(input$boxplot_interaction_filter_1)){
      df = df[df[,2] %in% input$boxplot_interaction_filter_1,]
  }
  
  if(!is.null(input$boxplot_interaction_filter_2)){
    df = df[df[,3] %in% input$boxplot_interaction_filter_2,]
  }
  
  if(nrow(df) > length(samples)){

    # take the values that stand out the most
      
    agg_df <- aggregate(df$weight, by=list(df$lr), FUN=mean)
    
    agg_df$x = scale(as.double(agg_df$x))
    
    # take the values that stand out the most
    agg_df = agg_df[agg_df$x > input$boxplot_interaction_z_score_filter,]
    
    df = df[df$lr %in% agg_df$Group.1,]
    
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
    

      lvls <- df %>%
        group_by(lr) %>%
        summarise(m = median(weight)) %>%
        arrange(desc(m)) %>%
        pull(lr)
      
    fig <- plot_ly(df, x = ~factor(lr,lvls), y = ~weight, boxmean = TRUE, boxpoints = input$boxplot_interaction_boxploint_type,
                   type = "box", text = ~samples)
    

    #   df = df %>% group_by_at(c(input$choose_distances_to_determine, input$choose_distances_to_determine_2,"samples")) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))
    #   fig <- plot_ly(df, x = as.vector(df[,input$choose_distances_to_determine])[[1]], y = ~freq, color = as.vector(df[,input$choose_distances_to_determine_2])[[1]], boxmean = TRUE, boxpoints = input$boxplot_interaction_boxploint_type,
    #                  type = "box", text = ~samples)
    # 
    # 
    # fig <- fig %>% layout(boxmode = "group")
    
    return(fig)
  } else {
    return(NULL)
  }
})
