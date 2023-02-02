##----------------------------------------------------------------------------##
## heatmap gene by cells
##----------------------------------------------------------------------------##

output[["spot_gene_heatmap"]] <- plotly::renderPlotly({
  req(values$data)
  req(input$select_number_spot_gene_heatmap)
  
  Gene_names <- names(GeneList_heatmap_IC())
  
  z <- head(values$data@assays$SCT@scale.data[Gene_names,],input$select_number_spot_gene_heatmap)
  
  p <- pheatmap(z,clustering_method = "ward.D",clustering_distance_cols = "correlation")
  
  row_order <- p[["tree_row"]][["order"]]
  z <- z[row_order,]
  
  if (input$cells_column_organization == TRUE){
    col_order <- p[["tree_col"]][["order"]]
    z <- z[,col_order]
  }
  
  plot_ly(
    x = colnames(z), y = rownames(z),
    z = z, type = "heatmap", zmin = input$slider_spot_gene_heatmap_range[1], zmax = input$slider_spot_gene_heatmap_range[2],
    colorscale = input$select_color_spot_gene_heatmap
  ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
               yaxis = list(showgrid = FALSE)
  )
})