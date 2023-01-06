##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

# search for the cells that were selected while in density
clicked_cell_UMAP <- reactive({
  return(plotly::event_data(c("plotly_click"), source = "C"))
})

observeEvent(clicked_cell_UMAP(), {
  print(plotly::event_data(c("plotly_click"), source = "C"))
  library(imagefx)
  table = plotly::event_data(c("plotly_click"), source = "C")
  
  min_x = min(c(ceiling(table$x),floor(table$x)))-4
  min_y = min(c(ceiling(table$y),floor(table$y)))-4
  max_x = min(c(ceiling(table$x),floor(table$x)))+4
  max_y = min(c(ceiling(table$y),floor(table$y)))+4
  
  cropped_image = crop.image(values$data@images$slice1@image, min_x, min_y, max_x, max_y)
  output$cropped_image = cropped_image[1]
  
  shinyalert(html = TRUE, text = tagList(
    plotlyOutput('mini_image')
  ))
})

output$mini_image <- renderPlotly({
  plot = plot_ly(type = 'scatter') %>% add_trace(type="image", source = raster2uri(raster::as.raster(output$cropped_image[1])), hoverinfo = 'skip')
  })