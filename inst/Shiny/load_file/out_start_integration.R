##----------------------------------------------------------------------------##
## Tab: Load data
##
## event: loading directly from spaceranger output integration
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## treat the input from select_file_2
##----------------------------------------------------------------------------##

observeEvent(input$start_integration_button, {
  if(length(values$integration_folders) >= 2){
      
    list_of_data = list()
    
    for(i in values$integration_folders){
      
      # load the seurat object
      files_names = list.files(path=i, pattern=".h5", all.files=TRUE, full.names=FALSE)
      files_names = files_names[grepl("filtered",files_names)][1]
      
      list_of_data[[basename(i)]] = Load10X_Spatial(i,
                                     filename = files_names,
                                     assay = "Spatial",
                                     slice = basename(i)
                                     )
      
      cells = names(colSums(GetAssayData(list_of_data[[basename(i)]]))[colSums(GetAssayData(list_of_data[[basename(i)]])) != 0])
      
      list_of_data[[basename(i)]] = subset(list_of_data[[basename(i)]],cells = cells)
      
    }
    
    # merging the list of samples
    
    load_file_from_path(filepath = merge(x = list_of_data[[names(list_of_data)[1]]], y = list_of_data[2:length(list_of_data)], add.cell.ids = names(list_of_data)), from_path = FALSE)
    
    # adding sample to metadata
    values$data@meta.data$sample = sapply(strsplit(rownames(values$data@meta.data),"_"),"[[",1)
    
    list_of_data = NULL
    gc()
    
  } else if (length(values$integration_folders) == 1) {
    
    # load the seurat object
    files_names = list.files(path=values$integration_folders, pattern=".h5", all.files=TRUE, full.names=FALSE)
    files_names = files_names[grepl("filtered",files_names)][1]
    
    if (file.exists(paste0(values$integration_folders,'/',files_names)) & file.exists(paste0(values$integration_folders,'/spatial/tissue_lowres_image.png'))){
        
        # load the seurat object
        load_file_from_path(filepath = Load10X_Spatial(values$integration_folders, filename = files_names, assay = "Spatial", slice = basename(values$integration_folders)), from_path = FALSE)
        
    } else {
      shinyalert("Wrong format", "requires Visium output", type = "error")
    }
  } else {
    shinyalert("No input", "Select data to load", type = "error")
  }
})



