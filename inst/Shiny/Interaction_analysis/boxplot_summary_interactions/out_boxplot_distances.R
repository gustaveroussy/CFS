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
  
  samples <- names(data@images)
  
  df = lapply(samples,function(sample){return(values$distances[[input$choose_distances_to_determine]][[sample]][[input$choose_method_for_distances]][["df"]])})
  
  df = dplyr::bind_rows(df, .id = "samples")
  
  df$lr <- paste0(df$l,"_",df$r)
  
  df = df[order(df$weight,decreasing = TRUE),]
  
  agg_df <- aggregate(df$weight, by=list(df$lr), FUN=mean)
  
  agg_df$x = scale(as.double(agg_df$x))
  
  # take the values that stand out the most
  agg_df = agg_df[agg_df$x > 3,]
  
  df = df[df$lr %in% agg_df$Group.1,]
  
  fig <- plot_ly(df, x = ~lr, y = ~weight, type = "box")
  
  # ggplot(df)+ 
  #   geom_boxplot(aes(x=fct_reorder(lr, weight, .fun=median, .desc=T),y=weight), lwd=1)+
  #   scale_y_continuous(trans = ggallin::pseudolog10_trans, breaks = c(-100,-50,-10,-2,2,10,50,100), labels = c(-100,-50,-10,-2,2,10,50,100))+
  #   theme_pubr()+
  #   theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  #   ggtitle(query1)+
  #   xlab("Associated ligands")+
  #   geom_hline(yintercept = 0, color="red", linetype="dashed")+
  #   geom_hline(yintercept = 2, color="blue", linetype="dashed")
  
  return(fig)
})
