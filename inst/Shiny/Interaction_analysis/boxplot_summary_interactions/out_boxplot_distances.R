##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["Boxplot_distances_plot"]] <- plotly::renderPlotly({
  req(values$distances[[input$choose_distances_to_determine]][[input$choose_sample_for_distances]][[input$choose_method_for_distances]])
  
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
  
  samples <- names(values$data@images)
  
  df = lapply(samples,function(sample){return(values$distances[[input$choose_distances_to_determine]][[sample]][[input$choose_method_for_distances]])})
  
  df = dplyr::bind_rows(df, .id = "samples")
  
  if(input$choose_distances_to_determine == "IC"){
    df$lr <- paste0(df$V1,"_",df$V2)
  } else if (input$choose_distances_to_determine == "Genes") {
    df$lr <- paste0(df$l,"_",df$r)
  }
  
  df = df[order(df$weight,decreasing = TRUE),]
  
  if(!is.null(input$boxplot_interaction_filter_1)){
    if(input$boxplot_interaction_filter_type == "IC"){
      
      df = df[df[,2] %in% input$boxplot_interaction_filter_1,]
      
    } else if (input$boxplot_interaction_filter_type %in% colnames(values$Annotation)){
      
      IC_filter = rownames(values$Annotation)[values$Annotation[,input$boxplot_interaction_filter_type] %in% input$boxplot_interaction_filter_1]
      df = df[df[,2] %in% IC_filter,]
      
    } else if (input$boxplot_interaction_filter_type == "lr interactions"){
      
      df = df[df[,2] %in% input$boxplot_interaction_filter_2,]
      
    }
  }
  
  if(!is.null(input$boxplot_interaction_filter_2)){
    if(input$boxplot_interaction_filter_type == "IC"){
      
      df = df[df[,3] %in% input$boxplot_interaction_filter_2,]
      
    } else if (input$boxplot_interaction_filter_type %in% colnames(values$Annotation)){
      
      IC_filter = rownames(values$Annotation)[values$Annotation[,input$boxplot_interaction_filter_type] %in% input$boxplot_interaction_filter_2]
      df = df[df[,2] %in% IC_filter,]
      
    } else if (input$boxplot_interaction_filter_type == "lr interactions"){
      
      df = df[df[,3] %in% input$boxplot_interaction_filter_2,]
      
    }
  }
  
  if(nrow(df) > length(samples)){
    
    agg_df <- aggregate(df$weight, by=list(df$lr), FUN=mean)
  
    agg_df$x = scale(as.double(agg_df$x))
    
    # take the values that stand out the most
    agg_df = agg_df[agg_df$x > input$boxplot_interaction_z_score_filter,]
    
    df = df[df$lr %in% agg_df$Group.1,]
  
  }
  
  if(nrow(df) > 0){
    df$samples = as.character(factor(df$samples, labels = samples))
    
    lvls <- df %>%
      group_by(lr) %>%
      summarise(m = median(weight)) %>%
      arrange(desc(m)) %>%
      pull(lr)
    
    fig <- plot_ly(df, x = ~factor(lr,lvls), y = ~weight, boxmean = TRUE, boxpoints = input$boxplot_interaction_boxploint_type,
                   type = "box", text = ~samples)
                   # hovertemplate = paste0("Sample : %{text}<br>",
                   #                        "Association : %{x}<br>",
                   #                        "Value: %{y}",
                   #                        "<extra></extra>")
    
    return(fig)
  } else {
    return(NULL)
  }
})
