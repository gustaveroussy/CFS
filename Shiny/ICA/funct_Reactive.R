########################################
# reactive of the image to plot by plotly
########################################

TissueCoordinates <- reactive({
  c <- GetTissueCoordinates(Launch_analysis(),
                            scale = "lowres",
                            cols = c("imagerow", "imagecol"))
  return(c)
})

observeEvent(input$input_file$datapath, {
  plot_image()
})

plot_image <- reactive({
  data <- Launch_analysis()
  
  image <- list(
    source = raster2uri(raster::as.raster(data@images$slice1@image)),
    xref = 'x',
    yref =  'y',
    sizex = dim(data_3@images$slice1@image)[1],
    sizey = dim(data_3@images$slice1@image)[2],
    sizing = 'stretch',
    opacity = 1,
    layer= 'below',
    x = -6,
    y = -10,   
    yanchor = 'top',
    xanchor = 'left'
  )
  return(image)
})


