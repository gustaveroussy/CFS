##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file 2
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## treat the input from select_file_2
##----------------------------------------------------------------------------##

observeEvent(input$load_data_file_select_integration, {
  volumes <- getVolumes()
  shinyDirChoose(input, 'load_data_file_select_integration', roots=volumes(), session=session)
  
  if(!is.null(input$load_data_file_select_integration)){
    path = parseDirPath(volumes(), input$load_data_file_select_integration)
    
    values$integration_folders = c(values$integration_folders,path)
  }
})

text_integration_folders <- reactive({
  if(is.null(values) | length(values$integration_folders) == 0){
    return("")
  } else {
    loop = "<ul>"
    for(i in values$integration_folders){
      loop = paste0(loop, paste0("<li><b>", i, "</b></li>"))
    }
    loop = paste0(loop, "</ul>")
    return(loop)
  }
})



