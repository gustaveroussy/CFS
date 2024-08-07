##----------------------------------------------------------------------------##
## Tab: Load data
##
## out_get_images_from_sample
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## prepared de low quality images for samples
##----------------------------------------------------------------------------##

observeEvent(input$Plot_image_spatial, {
  req(values$data)
  values$low_image = list()
  
  for(image in input$Plot_image_spatial){
    req(values$data@images[[image]])
    
    if("image" %in% slotNames(values$data@images[[image]])){
      
      values$low_image = append(values$low_image,raster2uri(raster::as.raster(values$data@images[[image]]@image)))
      
    } else {
      
      values$low_image = append(values$low_image,NULL)
      
    }
  }

  if(length(values$low_image) > 0){

    names(values$low_image) = input$Plot_image_spatial

  }
  
})