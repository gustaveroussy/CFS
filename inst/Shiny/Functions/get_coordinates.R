########################################
# reactive of the tissue coordinates to use
########################################

TissueCoordinates <- reactive({
  TC = list()
  
  for(image in names(values$data@images)){
    c = GetTissueCoordinates(values$data, image = image)
    names(c)[names(c) == "x"] <- "imagerow"
    names(c)[names(c) == "y"] <- "imagecol"
    
    if(is.null(values$HD_image)) {
      c[,"imagerow"] <- c[,"imagerow"] * values$data@images[[image]]@scale.factors$lowres
      c[,"imagecol"] <- c[,"imagecol"] * values$data@images[[image]]@scale.factors$lowres
    } else {
      c[,"imagerow"] <- c[,"imagerow"] * values$data@images[[image]]@scale.factors$hires
      c[,"imagecol"] <- c[,"imagecol"] * values$data@images[[image]]@scale.factors$hires
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
    
    TC = append(TC,list(c))
  }
  
  names(TC) = names(values$data@images)
  
  return(TC)
})

