##----------------------------------------------------------------------------##
## Sample based dotplot
##----------------------------------------------------------------------------##

output[["sample_based_dotplot"]] <- plotly::renderPlotly({
  return(sample_based_dotplot_react())
})

sample_based_dotplot_react <- reactive({
  
  if("sample" %in% colnames(values$data@meta.data)){
  
    x = c()
    for(i in names(values$data@images)){
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
                                           colorscale = colorscale_sample_based_dotplot(),
                                           showscale = TRUE,
                                           size = n_cell_percentil*25,
                                           reversescale=F),
                             text = n_cell_percentil,
                             customdata = weight_percentil,
                             hovertemplate = paste0("90th percentile weight : %{customdata}<br>",
                                                    "% of spot > 90% percentile: %{text}",
                                                    "<extra></extra>")
    )
    
    fig <- fig %>% layout(title = "IC dotplot by samples", xaxis=list(title = 'Sample', showgrid = FALSE, showticklabels=TRUE),
                          yaxis = list(title = 'IC', showgrid = TRUE, showticklabels=TRUE),
                          showlegend = FALSE)
    
  } else {

    x = rep(names(values$data@images),ncol(values$data@reductions$ica@cell.embeddings))

    y = 1:ncol(values$data@reductions$ica@cell.embeddings)

    cell_names = rownames(values$data@meta.data)
    IC_values = values$data@reductions$ica@cell.embeddings[cell_names,]

    weight_percentil = c()
    n_cell_percentil = c()

    for(k in colnames(values$data@reductions$ica@cell.embeddings)){
      weight_percentil = c(weight_percentil,quantile(IC_values[,k], c(.9)))

      total_quantile = quantile(values$data@reductions$ica@cell.embeddings[,k], c(.9))

      n_cell_percentil = c(n_cell_percentil,sum((values$data@reductions$ica@cell.embeddings[,k] >= total_quantile)[cell_names])/length(cell_names))
    }

    table_final = data.frame(x = x, y = y, weight_percentil = weight_percentil, n_cells = n_cell_percentil)

    fig <- plot_ly()

    fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                             x = table_final$x,
                             y = table_final$y,
                             marker = list(color = table_final$weight_percentil,
                                           colorscale = colorscale_sample_based_dotplot(),
                                           showscale = TRUE,
                                           size = table_final$n_cells*25#,
                                           #reversescale=input$invert_color_gene_projection
                                           ),
                             text = n_cell_percentil,
                             customdata = weight_percentil,
                             hovertemplate = paste0("90th percentile weight : %{customdata}<br>",
                                                    "% of sport > 90% percentile: %{text}",
                                                    "<extra></extra>")
    )

    fig <- fig %>% layout(title = "IC dotplot by samples", xaxis=list(title = 'Sample', showgrid = FALSE, showticklabels=TRUE),
                          yaxis = list(title = 'IC', showgrid = TRUE, showticklabels=TRUE),
                          showlegend = FALSE)

  }
  return(fig)
})

##----------------------------------------------------------------------------##
## Create the colorscale for dotplot
##----------------------------------------------------------------------------##
colorscale_sample_based_dotplot <- reactive({
  if(input$select_color_sample_based_dotplot %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
    return(input$select_color_sample_based_dotplot)
  } else {
    #prepare colorscales
    l = list()
    se = seq(0, 1, (1/(5000-1)))
    col = viridis_pal(option = input$select_color_sample_based_dotplot)(5000)
    for(i in 1:length(se)){
      l[[i]] = list(se[i],col[i])
    }
    return(l)
  }
})