##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select high res image file
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## treat the input from import file
##----------------------------------------------------------------------------##

observeEvent(input$input_image, {
  
  req(values$data@images[[input$input_image_sample_choice]])
  
  if (grepl('.*JPG$',toupper(input$input_image$datapath))) {
    image <- readJPEG(input$input_image$datapath)
  } else if (grepl('.*PNG$',toupper(input$input_image$datapath))){
    image <- readPNG(input$input_image$datapath)
  } else if (grepl('.*TIF$',toupper(input$input_image$datapath))){
    image = readTIFF(input$input_image$datapath)
  } else if (grepl('.*TIFF$',toupper(input$input_image$datapath))){
    image = readTIFF(input$input_image$datapath)
  } else {
    shinyalert("Oops!", "Wrong format (expecting .png, .jpg or .tif)", type = "error")
  }
  
  values$data@images[[input$input_image_sample_choice]]@image = image
  values$low_image[[input$input_image_sample_choice]] = raster2uri(raster::as.raster(image))
})

##----------------------------------------------------------------------------##
## treat the input from local file
##----------------------------------------------------------------------------##

observeEvent(input$input_image_local, {
  volumes <- getVolumes()
  
  shinyFileChoose(input, 'input_image_local', roots=volumes(), session=session)
  if(!is.null(input$input_image_local)){
    path = parseFilePaths(getVolumes(), input$input_image_local)
    if (length(path$datapath) != 0){
      req(values$data@images[[input$input_image_sample_choice]])
      
      withProgress(message = 'Loading HD image', value = 0, {
      
      incProgress(0.2, detail = "Loading image")
      
      if (grepl('.*JPG$',toupper(path$datapath))) {
        image <- readJPEG(path$datapath)
      } else if (grepl('.*PNG$',toupper(path$datapath))){
        image <- readPNG(path$datapath)
      } else if (grepl('.*TIF$',toupper(path$datapath))){
        image = readTIFF(path$datapath)
      } else if (grepl('.*TIFF$',toupper(path$datapath))){
        image = readTIFF(path$datapath)
      } else {
        shinyalert("Oops!", "Wrong format (expecting .png, .jpg or .tif)", type = "error")
      }
      
      values$data@images[[input$input_image_sample_choice]]@image = image
      values$low_image[[input$input_image_sample_choice]] = raster2uri(raster::as.raster(image))
        
      incProgress(0.8, detail = "Finished")
      
      })
    }
  }
})