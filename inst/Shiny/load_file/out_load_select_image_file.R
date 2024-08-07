##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select high res image file
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## treat the input from import file
##----------------------------------------------------------------------------##

observeEvent(input$input_image, {
  values$HD_image = NULL
  if (grepl('.*JPG$',toupper(input$input_image$datapath))) {
    values$HD_image <- raster2uri(readJPEG(input$input_image$datapath))
  } else if (grepl('.*PNG$',toupper(input$input_image$datapath))){
    values$HD_image <- raster2uri(readPNG(input$input_image$datapath))
  } else if (grepl('.*TIF$',toupper(input$input_image$datapath))){
    values$HD_image = raster2uri(readTIFF(input$input_image$datapath))
  } else if (grepl('.*TIFF$',toupper(input$input_image$datapath))){
    values$HD_image = raster2uri(readTIFF(input$input_image$datapath))
  } else {
    shinyalert("Oops!", "Wrong format (expecting .png, .jpg or .tif)", type = "error")
  }
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
      values$HD_image = NULL
      
      withProgress(message = 'Loading HD image', value = 0, {
      
      incProgress(0.2, detail = "Loading image")
      
      if (grepl('.*JPG$',toupper(path$datapath))) {
        values$HD_image <- raster2uri(readJPEG(path$datapath))
      } else if (grepl('.*PNG$',toupper(path$datapath))){
        values$HD_image <- raster2uri(readPNG(path$datapath))
      } else if (grepl('.*TIF$',toupper(path$datapath))){
        values$HD_image = raster2uri(readTIFF(path$datapath))
      } else if (grepl('.*TIFF$',toupper(path$datapath))){
        values$HD_image = raster2uri(readTIFF(path$datapath))
      } else {
        shinyalert("Oops!", "Wrong format (expecting .png, .jpg or .tif)", type = "error")
      }
      
      incProgress(0.8, detail = "Finished")
      
      })
    }
  }
})