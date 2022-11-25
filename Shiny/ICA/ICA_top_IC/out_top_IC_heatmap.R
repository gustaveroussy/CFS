##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["top_gene_IC_plot"]] <- plotly::renderPlotly({
  data <- Launch_analysis()
  
  p <- pheatmap(data@misc[["top_gene_ICA"]],clustering_method = "ward.D",clustering_distance_cols = "correlation")
  
  row_order <- p[["tree_row"]][["order"]]
  
  if (input$top_IC_column_organization == TRUE){
    col_order <- p[["tree_col"]][["order"]]
    data@misc[["top_gene_ICA"]] <- data@misc[["top_gene_ICA"]][,col_order]
  }
  
  data@misc[["top_gene_ICA"]] <- data@misc[["top_gene_ICA"]][row_order,]
  
  fig <- plot_ly(
    x = colnames(data@misc[["top_gene_ICA"]]), y = rownames(data@misc[["top_gene_ICA"]]),
    z = data@misc[["top_gene_ICA"]], type = "heatmap", zmin = input$slider_IC_top_range[1], zmax = input$slider_IC_top_range[2],
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
