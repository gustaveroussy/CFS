##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file 2
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## treat the input from select_file_2
##----------------------------------------------------------------------------##

observeEvent(input$start_integration_button, {
  if(length(values$integration_folders) >= 2){
    
    withProgress(message = 'Preparing list', value = 0, {
      
    list_of_data = list()
    
    for(i in values$integration_folders){
      
      incProgress(0.7/length(values$integration_folders), detail = paste0("Loading ", basename(i)))
      
      # load the seurat object
      files_names = list.files(path=i, pattern=".h5", all.files=TRUE, full.names=FALSE)
      files_names = files_names[grepl("filtered",files_names)][1]
      
      list_of_data[[basename(i)]] = Load10X_Spatial(i,
                                     filename = files_names,
                                     assay = "Spatial",
                                     slice = basename(i)
                                     )
    }
    
    # empty relevant values
    values$annotation_for_output = list()
    values$data = NULL
    values$IC_names = NULL
    values$Stat = NULL
    values$Annotation = NULL
    values$low_image = NULL
    values$marker_gene = NULL
    values$IC_names = NULL
    values$HD_image = NULL
    values$HD_image_2 = NULL
    
    
    # merging the list of samples
    incProgress(0.2, detail = "Merging")
    values$data = merge(x = list_of_data[[names(list_of_data)[1]]], y = list_of_data[2:length(list_of_data)], add.cell.ids = names(list_of_data))
    
    # adding sample to metadata
    values$data@meta.data$sample = sapply(strsplit(rownames(values$data@meta.data),"_"),"[[",1)
    
    # adding image to sample
    
    if('image' %in% names(attributes(values$data@images[[1]]))){
      if(grepl("^[[:digit:]]",names(list_of_data)[1])){
        values$low_image = raster2uri(raster::as.raster(values$data@images[[paste0("X",names(list_of_data)[1])]]@image))
      } else {
        values$low_image = raster2uri(raster::as.raster(values$data@images[[names(list_of_data)[1]]]@image))
      }
    }
    
    list_of_data = NULL
    gc()
    
    incProgress(0.1, detail = "Done")
    
    })
  }
})



