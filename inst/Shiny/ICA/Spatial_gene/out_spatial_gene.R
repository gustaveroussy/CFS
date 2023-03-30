##----------------------------------------------------------------------------##
## Spatial plot of gene expression output
##----------------------------------------------------------------------------##

output[["Spatial_gene_plot"]] <- plotly::renderPlotly({
  data <- values$data
  
  IC_C = input[["IC_choice"]]
  
  if (length(input$gene_projection_gene_choice) == 1){
    
    fig <- plot_ly()
    
    # if (!is.null(values$HD_image_2)){
    #   fig <- fig %>% add_trace(type="image", source = values$HD_image_2, hoverinfo = 'skip')
    # }
    if (!is.null(values$HD_image)) {
      fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
    } else {
      fig <- fig %>% add_trace(type="image", source = values$low_image, hoverinfo = 'skip')
    }
    
    fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                             x = TissueCoordinates()[,"imagecol"],
                             y = TissueCoordinates()[,"imagerow"],
                             marker = list(color = data@assays$SCT@scale.data[input$gene_projection_gene_choice,][rownames(TissueCoordinates())],
                                           colorscale = input$select_color_gene_projection,
                                           showscale = TRUE),
                             opacity = input$transparency_gene_projection,
                             text = data@assays$SCT@scale.data[input$gene_projection_gene_choice,][rownames(TissueCoordinates())],
                             customdata = names(data@assays$SCT@scale.data[input$gene_projection_gene_choice,][rownames(TissueCoordinates())]),
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
      plotList[[i]] <- plot_ly()
      
      # if (!is.null(values$HD_image_2)){
      #   plotList[[i]] <- plotList[[i]] %>% add_trace(type="image", source = values$HD_image_2, hoverinfo = 'skip')
      # }
      if (!is.null(values$HD_image)) {
        plotList[[i]] <- plotList[[i]] %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
      } else {
        plotList[[i]] <- plotList[[i]] %>% add_trace(type="image", source = values$low_image, hoverinfo = 'skip')
      }
      
      plotList[[i]] <- plotList[[i]] %>% add_trace(x = TissueCoordinates()[,"imagecol"], y = TissueCoordinates()[,"imagerow"],
                        marker = list(color = data@assays$SCT@scale.data[x,][rownames(TissueCoordinates())],
                                      colorscale = input$select_color_gene_projection),
                        opacity = input$transparency_gene_projection,
                        type = 'scatter', mode = "markers",
                        text = data@assays$SCT@scale.data[x,][rownames(TissueCoordinates())],
                        customdata = names(data@assays$SCT@scale.data[x,][rownames(TissueCoordinates())]),
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
    plotly::plotlyOutput("Spatial_gene_plot",
                         width = "auto",
                         height = "85vh")
  )
})