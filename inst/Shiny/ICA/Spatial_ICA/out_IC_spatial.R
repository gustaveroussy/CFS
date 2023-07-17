##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

output[["Spatial_IC_plot"]] <- plotly::renderPlotly({
  return(IC_spatial_output_react())
})

IC_spatial_output_react <- reactive({
  req(values$data)
  
  data <- values$data
  
  IC_C = input[["IC_choice"]]
    
  fig <- plot_ly(source = "E")
  
  # if (!is.null(values$HD_image_2)){
  #   fig <- fig %>% add_trace(type="image", source = values$HD_image_2, hoverinfo = 'skip')
  # }
  if (!is.null(values$HD_image)) {
    fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
  } else {
    if(!is.null(values$low_image)){
      fig <- fig %>% add_trace(type="image", source = values$low_image, hoverinfo = 'skip')
    }
  }
  
  
  #determines colorscale
  
  fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                           x = TissueCoordinates()[,"imagecol"], y = TissueCoordinates()[,"imagerow"],
                           marker = list(color = data@reductions$ica@cell.embeddings[, IC_C][rownames(TissueCoordinates())],
                                         colorscale = input$select_color_IC_projection,
                                         cmin = input$slider_IC_spatial_range[1], cmax=input$slider_IC_spatial_range[2],
                                         size = input$Plot_spatial_IC_size,
                                         showscale = TRUE,
                                         opacity = if(input$transparency_IC_spatial_choice == 1){input$transparency_IC_spatial_range}else{(data@reductions$ica@cell.embeddings[, IC_C][rownames(TissueCoordinates())])/max(data@reductions$ica@cell.embeddings[, IC_C][rownames(TissueCoordinates())])*input$transparency_IC_spatial_range},
                                         reversescale=input$invert_color_ICA_projection
                                         ),
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
