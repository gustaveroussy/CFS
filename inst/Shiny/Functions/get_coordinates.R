########################################
# reactive of the tissue coordinates to use
########################################

TissueCoordinates <- reactive({
  TC = list()
  
  for(image in names(values$data@images)){
    c = values$data@images[[image]]@coordinates
    names(c)[names(c) == "x"] <- "imagerow"
    names(c)[names(c) == "y"] <- "imagecol"
    
    if(("image" %in% slotNames(values$data@images[[image]])) && as.logical(sum(dim(values$data@images[[image]]@image) > 1000))){
      c[,"imagerow"] <- c[,"imagerow"] * values$data@images[[image]]@scale.factors$hires
      c[,"imagecol"] <- c[,"imagecol"] * values$data@images[[image]]@scale.factors$hires
    } else {
      c[,"imagerow"] <- c[,"imagerow"] * values$data@images[[image]]@scale.factors$lowres
      c[,"imagecol"] <- c[,"imagecol"] * values$data@images[[image]]@scale.factors$lowres
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

TissueCoordinates_ggplot <- function(){
  coordinates = list()
  
  coordinates = lapply(images_names(), function(sample){
    coordinates[[sample]] = values$data@images[[sample]]@coordinates;
    if(("image" %in% slotNames(values$data@images[[sample]])) && as.logical(sum(dim(values$data@images[[sample]]@image) > 1000))){
      coordinates[[sample]][,"imagerow"] <- coordinates[[sample]][,"imagerow"] * values$data@images[[sample]]@scale.factors$hires;
      coordinates[[sample]][,"imagecol"] <- coordinates[[sample]][,"imagecol"] * values$data@images[[sample]]@scale.factors$hires
    } else {
      coordinates[[sample]][,"imagerow"] <- coordinates[[sample]][,"imagerow"] * values$data@images[[sample]]@scale.factors$lowres;
      coordinates[[sample]][,"imagecol"] <- coordinates[[sample]][,"imagecol"] * values$data@images[[sample]]@scale.factors$lowres
    };
    return(coordinates[[sample]])})
  
  names(coordinates) = images_names()
  return(coordinates)
}
