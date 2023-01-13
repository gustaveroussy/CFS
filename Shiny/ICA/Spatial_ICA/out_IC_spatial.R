##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

output[["Spatial_IC_plot"]] <- plotly::renderPlotly({

  data <- Launch_analysis()
  
  IC_C = input[["IC_choice"]]
  
  if(input$select_color_IC_projection != "Range"){
    
    fig <- plot_ly()
    
    if (!is.null(values$HD_image_2)){
      fig <- fig %>% add_trace(type="image", source = values$HD_image_2, hoverinfo = 'skip')
    } else if (!is.null(values$HD_image)) {
      fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
    } else {
      fig <- fig %>% add_trace(type="image", source = values$low_image, hoverinfo = 'skip')
    }
    
    fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                             x = TissueCoordinates()[,"imagecol"], y = TissueCoordinates()[,"imagerow"],
                             marker = list(color = data@misc[[IC_C]]$IC_weight,
                                           colorscale = input$select_color_IC_projection,
                                           cmin = input$slider_IC_spatial_range[1], cmax=input$slider_IC_spatial_range[2],
                                           showscale = TRUE),
                             opacity = input$transparency_IC_spatial_range,
                             text = data@misc[[IC_C]]$IC_weight[rownames(TissueCoordinates())],
                             customdata = names(data@misc[[IC_C]]$IC_weight[rownames(TissueCoordinates())]),
                             hovertemplate = paste0("Cell : %{customdata}<br>",
                                                    "Expression: %{text}",
                                                    "<extra></extra>")
    )
    
    fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                          yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                          showlegend = FALSE
    )
  } else {
    
    fig <- plot_ly()
    
    if (!is.null(values$HD_image_2)){
      fig <- fig %>% add_trace(type="image", source = values$HD_image_2, hoverinfo = 'skip')
    } else if (!is.null(values$HD_image)) {
      fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
    } else {
      fig <- fig %>% add_trace(type="image", source = values$low_image, hoverinfo = 'skip')
    }
    
    fig <- fig %>% add_trace(type = "scatter", mode = "markers", x = TissueCoordinates()[,"imagecol"], y = TissueCoordinates()[,"imagerow"],
                             marker = list(color = data@misc[[IC_C]]$IC_weight,
                                           colors = colfunc(),
                                           cmin = input$slider_IC_spatial_range[1], cmax=input$slider_IC_spatial_range[2],
                                           showscale = TRUE),
                             opacity = input$transparency_IC_spatial_range,
                             text = data@misc[[IC_C]]$IC_weight,
                             customdata = names(data@misc[[IC_C]]$IC_weight),
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
    plotly::plotlyOutput("Spatial_IC_plot")
  )
})