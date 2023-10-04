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
    paths = parseDirPath(volumes(), input$load_data_file_select_integration)
    paths = list.dirs(paths, recursive = FALSE)
    if (length(paths) != 0){
      # preparing the list of loaded files
      list_files = list()
      
      # cleaning the shiny
      values$annotation_for_output = list()
      values$data = NULL
      values$IC_names = NULL
      values$Stat = NULL
      values$Annotation = NULL
      values$UMAP = NULL
      values$low_image = NULL
      values$marker_gene = NULL
      values$IC_names = NULL
      values$HD_image = NULL
      values$HD_image_2 = NULL
      
      withProgress(message = 'Loading Visium output', value = 0, {
        for (path in paths){
          incProgress((1-0.2)/length(paths), detail = "Loading spatial 10X")
          
          if (file.exists(paste0(path,'/filtered_feature_bc_matrix.h5')) & file.exists(paste0(path,'/spatial/tissue_lowres_image.png'))){
              # load the seurat object
              files_names = list.files(path=path, pattern=".h5", all.files=TRUE, full.names=FALSE)
              files_names = files_names[grepl("filtered",files_names)][1]
    
              list_files[[path]] = Load10X_Spatial(path, filename = files_names)
    
              # if('image' %in% names(attributes(values$data@images[[1]]))){
              #   values$low_image = raster2uri(raster::as.raster(values$data@images$slice1@image))
              # }
          } else {
            shinyalert("Wrong format", "requires Visium output", type = "error")
          }
          
        }
        incProgress(0.1, detail = "Merging")
        
        split = stringr::str_split(paths,"/")
        names = c()
        for(i in split){
          names = c(names,tail(i, n=1))
        }
        
        values$data <- merge(list_files[[paths[1]]], y = list_files[paths[2:length(paths)]], add.cell.ids = names, project = "Merge")
        
        incProgress(0.1, detail = "Done")
      })
    }
  }
})