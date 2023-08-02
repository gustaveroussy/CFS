##----------------------------------------------------------------------------##
## heatmap gene by cells
##----------------------------------------------------------------------------##

# # Pairwise correlation between samples (columns)
# cols.cor <- cor(z, use = "pairwise.complete.obs", method = "pearson")
# # Pairwise correlation between rows (genes)
# rows.cor <- cor(t(z), use = "pairwise.complete.obs", method = "pearson")
# 
# ## Row- and column-wise clustering using correlation 
# hclust.col <- hclust(as.dist(1-cols.cor)) 
# hclust.row <- hclust(as.dist(1-rows.cor))

output[["spot_gene_heatmap"]] <- plotly::renderPlotly({
  req(values$data)
  req(input$select_number_spot_gene_heatmap)

  Gene_names <- names(GeneList_heatmap_IC())
  
  if(ncol(values$data@assays$SCT@scale.data) <= 5000){
    z <- head(values$data@assays$SCT@scale.data[Gene_names,],input$select_number_spot_gene_heatmap)
    
    # gene order
    dist = dist(z, method = 'euclidean')
    y = hclust(dist, method = "ward.D")
    row_order <- y$labels[y$order]
    z <- z[row_order,]
    
    # cell order
    if (input$cells_column_organization == TRUE){
      dist = dist(t(z), method = 'euclidean')
      y = hclust(dist, method = "ward.D")
      col_order <- y$labels[y$order]
      z <- z[,col_order]
    }
    
    if(input$select_color_IC_top %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
      plot = plot_ly(
        x = colnames(z), y = rownames(z),
        z = z, type = "heatmap", zmin = input$slider_spot_gene_heatmap_range[1], zmax = input$slider_spot_gene_heatmap_range[2],
        colorscale = input$select_color_spot_gene_heatmap
      ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                   yaxis = list(showgrid = FALSE)
      )
    } else {
      plot = plot_ly(
        x = colnames(z), y = rownames(z),
        z = z, type = "heatmap", zmin = input$slider_spot_gene_heatmap_range[1], zmax = input$slider_spot_gene_heatmap_range[2],
        colors = viridis_pal(option = input$select_color_spot_gene_heatmap)(nrow(z) * ncol(z)),
      ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                   yaxis = list(showgrid = FALSE)
      )
    }

    return(plot)
  } else {
    return(NULL)
  }
})