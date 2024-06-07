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

  # if only one image is selected
    if(!is.null(values$HD_image)) {
      TC = list()
      for (image in input$Plot_image_spatial){
        c <- values$data@images[[input$Plot_image_spatial]]@coordinates * values$data@images[[input$Plot_image_spatial]]@scale.factors$hires
        names(c)[names(c) == "x"] <- "imagerow"
        names(c)[names(c) == "y"] <- "imagecol"
        
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
      
    } else {
      TC = list()
      for (image in input$Plot_image_spatial){
        c <- GetTissueCoordinates(values$data, image = image)
        names(c)[names(c) == "x"] <- "imagerow"
        names(c)[names(c) == "y"] <- "imagecol"
        
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
    }
  
  names(TC) = input$Plot_image_spatial
  
  return(TC)
})

