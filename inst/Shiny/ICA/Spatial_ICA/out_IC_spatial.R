##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

output[["Spatial_IC_plot"]] <- plotly::renderPlotly({
  req(values$data)
  
  data <- values$data
  
  IC_C = input[["IC_choice"]]
  
  if(input$select_color_IC_projection != "Range"){
    
    fig <- plot_ly(source = "E")
    
    # if (!is.null(values$HD_image_2)){
    #   fig <- fig %>% add_trace(type="image", source = values$HD_image_2, hoverinfo = 'skip')
    # }
    if (!is.null(values$HD_image)) {
      fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
    } else {
      fig <- fig %>% add_trace(type="image", source = values$low_image, hoverinfo = 'skip')
    }
    
    fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                             x = TissueCoordinates()[,"imagecol"], y = TissueCoordinates()[,"imagerow"],
                             marker = list(color = data@reductions$ica@cell.embeddings[, IC_C][rownames(TissueCoordinates())],
                                           colorscale = input$select_color_IC_projection,
                                           cmin = input$slider_IC_spatial_range[1], cmax=input$slider_IC_spatial_range[2],
                                           showscale = TRUE),
                             opacity = input$transparency_IC_spatial_range,
                             text = data@reductions$ica@cell.embeddings[, IC_C][rownames(TissueCoordinates())],
                             customdata = names(data@reductions$ica@cell.embeddings[, IC_C][rownames(TissueCoordinates())]),
                             hovertemplate = paste0("Cell : %{customdata}<br>",
                                                    "Expression: %{text}",
                                                    "<extra></extra>")
    )
    
    fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                          yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                          showlegend = FALSE
    )
  } else {
    
    fig <- plot_ly(source = "E")
    
    # if (!is.null(values$HD_image_2)){
    #   fig <- fig %>% add_trace(type="image", source = values$HD_image_2, hoverinfo = 'skip')
    # }
    if (!is.null(values$HD_image)) {
      fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
    } else {
      fig <- fig %>% add_trace(type="image", source = values$low_image, hoverinfo = 'skip')
    }
    
    fig <- fig %>% add_trace(type = "scatter", mode = "markers", x = TissueCoordinates()[,"imagecol"], y = TissueCoordinates()[,"imagerow"],
                             marker = list(color = data@reductions$ica@cell.embeddings[, IC_C],
                                           colors = colfunc(),
                                           cmin = input$slider_IC_spatial_range[1], cmax=input$slider_IC_spatial_range[2],
                                           showscale = TRUE),
                             opacity = input$transparency_IC_spatial_range,
                             text = data@reductions$ica@cell.embeddings[, IC_C],
                             customdata = names(data@reductions$ica@cell.embeddings[, IC_C]),
                             hoverinfo = "text",
                             hovertemplate = paste0("Cell : %{customdata}<br>",
                                                    "Expression: %{text}",
                                                    "<extra></extra>")
    )
    
    fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                          yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                          showlegend = FALSE
    ) # add trace, show legend
    
  }
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Spatial_IC_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("Spatial_IC_plot",
                         width = "auto",
                         height = "85vh")
  )
})