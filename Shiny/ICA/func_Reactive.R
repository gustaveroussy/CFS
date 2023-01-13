########################################
# Color palette
########################################

colfunc <- reactive({
  color <- colorRampPalette(c("blue", "green", "yellow", "orange", "red"))
  return(color)
})

########################################
# reactive of the image to plot by plotly
########################################

TissueCoordinates <- reactive({
  if (!is.null(values$HD_image_2)) {
    c <- values$data@images$slice1@coordinates
  } else if(!is.null(values$HD_image)) {
    c <- values$data@images$slice1@coordinates * values$data@images$slice1@scale.factors$hires
  } else {
    c <- GetTissueCoordinates(values$data)
  }
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
    sizex = dim(data@images$slice1@image)[1],
    sizey = dim(data@images$slice1@image)[2],
    sizing = 'stretch',
    opacity = 1,
    layer= 'below',
    x = -10,
    y = -4,   
    yanchor = 'top',
    xanchor = 'left'
  )
  return(image)
})

plot_image_2 <- reactive({
  data <- Launch_analysis()
  image <- list(
    source = raster2uri(raster::as.raster(flipImage(rotateFixed((data@images$slice1@image), 180), mode = "horizontal"))),
    xref = 'paper',
    yref =  'paper',
    sizex = 1,
    sizey = 1,
    sizing = 'stretch',
    opacity = 1,
    layer= 'below',
    x = 0,
    y = 1,   
    yanchor = 'top',
    xanchor = 'left'
  )
  return(image)
})

