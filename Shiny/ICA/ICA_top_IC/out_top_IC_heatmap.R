##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["top_gene_IC_plot"]] <- plotly::renderPlotly({
  data <- values$data
  
  list_gene <-  purrr::map(data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>3))],function(.x){x<-.x %>% arrange(desc(abs(Sig))) %>% head(n=input$select_number_IC_top_heatmap) ;return(x$gene)}) %>% unlist %>% unique
  
  data_heat=data@reductions$ica@feature.loadings[as.matrix(list_gene),]
  
  if (input$top_IC_kurtosis_filter){
    data_heat = data_heat[,names(which(data@misc$GeneAndStat$Kurtosis_ICs > 3))]
  }
  
  p <- pheatmap(data_heat,clustering_method = "ward.D",clustering_distance_cols = "correlation")
  
  row_order <- p[["tree_row"]][["order"]]
  
  if (input$top_IC_column_organization == TRUE){
    col_order <- p[["tree_col"]][["order"]]
    data_heat <- data_heat[,col_order]
  }
  
  data_heat <- data_heat[row_order,]
  
  b = log10(data_heat)
  b[is.nan(b)] <- 0
  
  fig <- plot_ly(
    x = colnames(data_heat), y = rownames(data_heat),
    z = if(input$log_top_IC_heatmap == TRUE){b}else{data_heat}, type = "heatmap", zmin = input$slider_IC_top_range[1], zmax = input$slider_IC_top_range[2],
    colorscale = input$select_color_IC_top,
    hovertemplate = paste(
      "Gene: %{y:.2f%}<br>",
      "IC: %{x:.2f%}<br>",
      "Value: %{z:.2f%}",
      "<extra></extra>"
    )
  )
  
  fig <- fig %>% layout(yaxis = list(title = 'Genes', tickfont = list(size = 7)),
                        xaxis = list(title = 'IC')
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["top_IC_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("top_gene_IC_plot")
  )
})
