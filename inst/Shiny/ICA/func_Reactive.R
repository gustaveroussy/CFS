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
  # if (!is.null(values$HD_image_2)) {
  #   c <- values$data@images$slice1@coordinates
  # }
  if(!is.null(values$HD_image)) {
    c <- values$data@images$slice1@coordinates * values$data@images$slice1@scale.factors$hires
  } else {
    c <- GetTissueCoordinates(values$data)
  }
  return(c)
})

