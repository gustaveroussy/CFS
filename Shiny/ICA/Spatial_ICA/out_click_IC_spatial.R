##----------------------------------------------------------------------------##
## Element that calls the HD image on click
##----------------------------------------------------------------------------##

# search for the cells that were selected while in IC display in spatial
clicked_cell_ICA <- reactive({
  return(plotly::event_data(c("plotly_click"), source = "E"))
})

# Create the image to display in plotly 
observeEvent(clicked_cell_ICA(), {
  req(values$HD_image_2)
  print(plotly::event_data(c("plotly_click"), source = "E"))

  table = plotly::event_data(c("plotly_click"), source = "E")
  
  table$x = table$x * (1/values$data@images[["slice1"]]@scale.factors[["lowres"]])
  table$y = table$y * (1/values$data@images[["slice1"]]@scale.factors[["lowres"]])
  
  min_x = table$x-ceiling(1/(values$data@images[["slice1"]]@spot.radius))*2
  min_y = table$y-ceiling(1/(values$data@images[["slice1"]]@spot.radius))*2
  max_x = table$x+ceiling(1/(values$data@images[["slice1"]]@spot.radius))*2
  max_y = table$y+ceiling(1/(values$data@images[["slice1"]]@spot.radius))*2
  
  cropped_image = crop.image(values$HD_image_2, min_y, min_x, max_y, max_x)
  
  values$cropped_image = raster2uri(raster::as.raster(cropped_image$img.crop))
  
  shinyalert(html = TRUE, text = tagList(
    plotlyOutput('mini_plot_ICA')
  ))
})

output[["mini_plot_ICA"]] <- plotly::renderPlotly({
  fig = plot_ly()
  fig <- fig %>% add_trace(type="image", source = values$cropped_image, hoverinfo = 'skip')
  # add shapes to the layout
  fig <- layout(fig,
                shapes = list(
                  list(type = "rect",
                       fillcolor = NULL, line = list(color = "black"), opacity = 0.5,
                       x0 = ceiling(1/(values$data@images[["slice1"]]@spot.radius)),
                       x1 = ceiling(1/(values$data@images[["slice1"]]@spot.radius))*3, xref = "x",
                       y0 = ceiling(1/(values$data@images[["slice1"]]@spot.radius)),
                       y1 = ceiling(1/(values$data@images[["slice1"]]@spot.radius))*3, yref = "y")))
})

