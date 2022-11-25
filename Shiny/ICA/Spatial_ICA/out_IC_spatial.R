##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

output[["Spatial_IC_plot"]] <- plotly::renderPlotly({
  if (input$pie_plot == TRUE){
    
    req(pie_plots$pie_plot)
    pie_plots$pie_plot
    
  }else{
    data <- Launch_analysis()
    
    IC_C = input[["IC_choice"]]
    
    if(input$select_color_IC_projection != "Range"){
      
      fig <- plot_ly()
      
      fig <- fig %>% add_trace(type="image", source = raster2uri(raster::as.raster(data@images$slice1@image)), hoverinfo = 'skip')
      
      fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                               x = TissueCoordinates()[,"imagecol"], y = TissueCoordinates()[,"imagerow"],
                               marker = list(color = data@misc[[IC_C]]$IC_weight,
                                             colorscale = input$select_color_IC_projection,
                                             cmin = input$slider_IC_spatial_range[1], cmax=input$slider_IC_spatial_range[2],
                                             showscale = TRUE),
                               text = data@misc[[IC_C]]$IC_weight,
                               customdata = names(data@misc[[IC_C]]$IC_weight),
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
      
      fig <- fig %>% add_trace(type="image", source = raster2uri(raster::as.raster(data@images$slice1@image)), hoverinfo = 'skip')
      
      fig <- fig %>% add_trace(type = "scatter", mode = "markers", x = TissueCoordinates()[,"imagecol"], y = TissueCoordinates()[,"imagerow"],
                               marker = list(color = data@misc[[IC_C]]$IC_weight,
                                             colors = colfunc(),
                                             cmin = input$slider_IC_spatial_range[1], cmax=input$slider_IC_spatial_range[2],
                                             showscale = TRUE),
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