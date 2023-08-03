########################################
# Color palette
########################################

colfunc <- reactive({
  color <- colorRampPalette(c("blue", "green", "yellow", "orange", "red"))
  return(color)
})

########################################
# reactive of the tissue coordinates to use
########################################

TissueCoordinates <- reactive({
  # if (!is.null(values$HD_image_2)) {
  #   c <- values$data@images$slice1@coordinates
  # }
  if(!is.null(values$HD_image)) {
    c <- values$data@images[[input$Plot_image_spatial]]@coordinates * values$data@images[[input$Plot_image_spatial]]@scale.factors$hires
  } else {
    c <- GetTissueCoordinates(values$data, image = input$Plot_image_spatial)
    names(c)[names(c) == "x"] <- "imagerow"
    names(c)[names(c) == "y"] <- "imagecol"
  }
  
  if(input$spatial_mirror_X){
    c$imagecol = c$imagecol * (-1)
  }
  if(input$spatial_mirror_Y){
    c$imagerow = c$imagerow * (-1)
  }
  if(input$spatial_flip){
    imagerow = c$imagerow
    c$imagerow = c$imagecol
    c$imagecol = imagerow
  }

  return(c)
})

