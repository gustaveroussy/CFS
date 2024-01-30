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
    
    print(values$integration_folders)
  }
})

observe({
  if(is.null(values) | is.null(values$integration_folders)){
    values$text_integration_folders = ""
  }else{
    values$text_integration_folders = "<ul>"
    for(i in values$integration_folders){
      values$text_integration_folders = paste0(values$text_integration_folders, paste0("<li><b>", i, "</b></li>"))
    }
    values$text_integration_folders = paste0(values$text_integration_folders, "</ul>")
  }
})



