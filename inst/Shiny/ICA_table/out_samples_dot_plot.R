output[["sample_based_dotplot"]] <- plotly::renderPlotly({
  return(sample_based_dotplot_react())
})

sample_based_dotplot_react <- reactive({
  
  # if("sample" %in% colnames(metadata)){
  
    x = c()
    for(i in 1:length(unique(values$data@meta.data$sample))){
      x = c(x,rep(i,ncol(values$data@reductions$ica@cell.embeddings)))
    }
    
    y = rep(1:ncol(values$data@reductions$ica@cell.embeddings),length(unique(values$data@meta.data$sample)))
    
    weight_percentil = c()
    n_cell_percentil = c()
    
    for(i in unique(values$data@meta.data$sample)){
      cell_names = rownames(values$data@meta.data)[values$data@meta.data$sample == i]
      IC_values = values$data@reductions$ica@cell.embeddings[cell_names,]
      
      for(k in colnames(values$data@reductions$ica@cell.embeddings)){
        weight_percentil = c(weight_percentil,quantile(IC_values[,k], c(.9)))
        
        total_quantile = quantile(values$data@reductions$ica@cell.embeddings[,k], c(.9))
        
        n_cell_percentil = c(n_cell_percentil,sum((values$data@reductions$ica@cell.embeddings[,k] >= total_quantile)[cell_names])/length(cell_names))
      }
    }
    
    n_cell_percentil[n_cell_percentil < 0.125] = 0
    
    fig <- plot_ly()
    
    fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                             x = x,
                             y = y,
                             marker = list(color = weight_percentil,
                                           #colorscale = colorscale_gene_spatial(),
                                           showscale = TRUE,
                                           size = n_cell_percentil*25,
                                           reversescale=F)
    )
    
    fig <- fig %>% layout(title = "IC dotplot by samples", xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                          yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                          showlegend = FALSE)
    
  # } else {
  #   
  #   x = rep(1,ncol(data@reductions$ica@cell.embeddings))
  #   
  #   y = 1:ncol(data@reductions$ica@cell.embeddings)
  #   
  #   cell_names = rownames(metadata)data
  #   IC_values = data@reductions$ica@cell.embeddings[cell_names,]
  #   
  #   weight_percentil = c()
  #   n_cell_percentil = c()
  #   
  #   for(k in colnames(data@reductions$ica@cell.embeddings)){
  #     weight_percentil = c(weight_percentil,quantile(IC_values[,k], c(.9)))
  #     
  #     total_quantile = quantile(data@reductions$ica@cell.embeddings[,k], c(.9))
  #     
  #     n_cell_percentil = c(n_cell_percentil,sum((data@reductions$ica@cell.embeddings[,k] >= total_quantile)[cell_names])/length(cell_names))
  #   }
  #   
  #   table_final = data.frame(x = x, y = y, weight_percentil = weight_percentil, n_cells = n_cell_percentil)
  #   
  #   fig <- plot_ly()
  #   
  #   fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
  #                            x = table_final$x,
  #                            y = table_final$y,
  #                            marker = list(color = table_final$weight_percentil,
  #                                          #colorscale = colorscale_gene_spatial(),
  #                                          showscale = TRUE,
  #                                          size = table_final$n_cells*25,
  #                                          reversescale=input$invert_color_gene_projection),
  #                            text = NULL,
  #                            customdata = NULL,
  #                            hovertemplate = paste0("IC : %{customdata}<br>",
  #                                                   "Expression: %{text}",
  #                                                   "<extra></extra>")
  #   )
  #   
  #   fig <- fig %>% layout(title = "IC dotplot by samples", xaxis=list(showgrid = FALSE, showticklabels=FALSE),
  #                         yaxis = list(showgrid = FALSE, showticklabels=FALSE),
  #                         showlegend = FALSE)
  #   
  # }
  return(fig)
})