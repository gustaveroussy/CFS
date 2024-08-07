##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select local file
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## treat the input from local file
##----------------------------------------------------------------------------##

observeEvent(input$input_file_local, {
  volumes <- getVolumes()
  
  shinyFileChoose(input, 'input_file_local', roots=volumes(), session=session)
  if(!is.null(input$input_file_local)){
    path = parseFilePaths(getVolumes(), input$input_file_local)
    if (length(path$datapath) != 0){
      
      load_file_from_path(filepath = path$datapath)
      
    }
  }
})