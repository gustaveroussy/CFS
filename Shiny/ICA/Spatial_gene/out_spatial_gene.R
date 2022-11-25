##----------------------------------------------------------------------------##
## Spatial plot of gene expression output
##----------------------------------------------------------------------------##

output[["Spatial_gene_plot"]] <- plotly::renderPlotly({
  data <- Launch_analysis()
  
  IC_C = input[["IC_choice"]]
  
  if (length(input$gene_projection_gene_choice) == 1){
    
    fig <- plot_ly()
    
    fig <- fig %>% add_trace(type="image", source = raster2uri(raster::as.raster(data@images$slice1@image)), hoverinfo = 'skip')
    
    fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                             x = TissueCoordinates()[,"imagecol"],
                             y = TissueCoordinates()[,"imagerow"],
                             marker = list(color = data@misc[[IC_C]]$spot_top_genes_weight[input$gene_projection_gene_choice,],
                                           colorscale = input$select_color_gene_projection,
                                           showscale = TRUE),
                             text = data@misc[[IC_C]]$spot_top_genes_weight[input$gene_projection_gene_choice,],
                             customdata = names(data@misc[[IC_C]]$spot_top_genes_weight[input$gene_projection_gene_choice,]),
                             hovertemplate = paste0("Cell : %{customdata}<br>",
                                                    "Expression: %{text}",
                                                    "<extra></extra>")
    )
    
    
    
    fig <- fig %>% layout(title = input$gene_projection_gene_choice, xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                          yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                          showlegend = FALSE)
    
    return(fig)
    
  } else if (length(input$gene_projection_gene_choice) > 1) {
    
    plotList <- list()
    i = 1
    for ( x in input$gene_projection_gene_choice ) {
      plotList[[i]] <-  plot_ly() %>%
        add_trace(type="image", source = raster2uri(raster::as.raster(data@images$slice1@image)), hoverinfo = 'skip'
        ) %>% add_trace(x = TissueCoordinates()[,"imagecol"], y = TissueCoordinates()[,"imagerow"],
                        marker = list(color = data@misc[[IC_C]]$spot_top_genes_weight[x,],
                                      colorscale = input$select_color_gene_projection),
                        type = 'scatter', mode = "markers",
                        text = data@misc[[IC_C]]$spot_top_genes_weight[x,],
                        customdata = names(data@misc[[IC_C]]$spot_top_genes_weight[x,]),
                        hovertemplate = paste0("Cell : %{customdata}<br>",
                                               "Expression: %{text}",
                                               "<extra></extra>")
        ) %>% layout(title = input$gene_projection_gene_choice, xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                     yaxis = list(showgrid = FALSE, showticklabels=FALSE))
      i = i+1
    }
    
    subplot(plotList, nrows = ceiling(length(input$gene_projection_gene_choice)/3)) %>% layout(showlegend = FALSE)
    
  }
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Spatial_gene_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("Spatial_gene_plot")
  )
})