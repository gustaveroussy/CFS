##----------------------------------------------------------------------------##
## heatmap gene by cells
##----------------------------------------------------------------------------##

output[["spot_gene_heatmap"]] <- plotly::renderPlotly({
  
  data <- Launch_analysis()
  IC_C = input[["IC_choice"]]
  
  p <- pheatmap(data@misc[[IC_C]]$spot_top_genes_weight,clustering_method = "ward.D",clustering_distance_cols = "correlation")
  
  col_order <- p[["tree_col"]][["order"]]
  row_order <- p[["tree_row"]][["order"]]
  data@misc[[IC_C]]$spot_top_genes_weight <- data@misc[[IC_C]]$spot_top_genes_weight[,col_order]
  data@misc[[IC_C]]$spot_top_genes_weight <- data@misc[[IC_C]]$spot_top_genes_weight[row_order,]
  
  plot_ly(
    x = colnames(data@misc[[IC_C]]$spot_top_genes_weight), y = rownames(data@misc[[IC_C]]$spot_top_genes_weight),
    z = data@misc[[IC_C]]$spot_top_genes_weight, type = "heatmap", zmin = input$slider_spot_gene_heatmap_range[1], zmax = input$slider_spot_gene_heatmap_range[2],
    colorscale = input$select_color_spot_gene_heatmap
  ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
               yaxis = list(showgrid = FALSE)
  )
})