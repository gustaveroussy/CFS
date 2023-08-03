##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file 2
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## treat the input from select_file_2
##----------------------------------------------------------------------------##

volumes <- getVolumes()

observeEvent(input$load_data_file_select, {
  shinyDirChoose(input, 'load_data_file_select', roots=volumes(), session=session)
  if(!is.null(input$load_data_file_select)){
    path = parseDirPath(volumes(), input$load_data_file_select)
    if (length(path) != 0){
      if (file.exists(paste0(path,'/filtered_feature_bc_matrix.h5')) & file.exists(paste0(path,'/spatial/tissue_lowres_image.png'))){
        withProgress(message = 'Loading Visium output', value = 0, {
          incProgress(0.4, detail = "Cleaning Shiny")
          # empty relevant values
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
          
          incProgress(0.2, detail = "Loading spatial 10X")
          # load the seurat object
          files_names = list.files(path=path, pattern=".h5", all.files=TRUE, full.names=FALSE)
          files_names = files_names[grepl("filtered",files_names)][1]
          
          values$data = Load10X_Spatial(path, filename = files_names)
          
          if('image' %in% names(attributes(values$data@images[[1]]))){
            values$low_image = raster2uri(raster::as.raster(values$data@images$slice1@image))
          }
          
          incProgress(0.4, detail = "Done")
        })
      } else {
        shinyalert("Wrong format", "requires Visium output", type = "error")
      }
    }
  }
})