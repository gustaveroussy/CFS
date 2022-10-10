########################################
# reactive of the image to plot by plotly
########################################

observeEvent(input$input_file$datapath, {
  plot_image()
})

plot_image <- reactive({
  data <- Launch_analysis()
  image <- list(
    source = raster2uri(raster::as.raster(data@images$slice1@image)),
    xref = 'x',
    yref =  'y',
    sizex = 11100,
    sizey = 11400,
    sizing = 'stretch',
    opacity = 1,
    layer= 'below',
    x = 0,
    y = 0,   
    yanchor = 'top',
    xanchor = 'left'
  )
  return(image)
})


