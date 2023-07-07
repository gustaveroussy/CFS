################################################################################
Create_CosMX_seurat_cells=function(matrix=NULL, metadata = NULL, min.features = 5){
  genes = colnames(matrix)[-c(1,2)]
  
  matrix = t(matrix)
  matrix = matrix[,which(matrix['cell_ID',] != 0)]
  colnames(matrix) = paste0(matrix['fov',],"_",matrix['cell_ID',])
  
  matrix = matrix[-c(1,2),]
  rownames(matrix) = genes
  
  seurat.object = CreateSeuratObject(matrix,
                                     project = "CTD",
                                     assay = "Spatial",
                                     names.field = 1,
                                     names.delim = "_",
                                     meta.data = NULL,
                                     min.cells = 0,
                                     min.features = 0,
                                     row.names = NULL,
  )

  DT = data.frame (x  = metadata$CenterX_global_px,
                   y = metadata$CenterY_global_px,
                   'imagerow' = metadata$CenterX_global_px,
                   'imagecol' = metadata$CenterY_global_px)
  
  rownames(DT) = colnames(matrix)
  
  seurat.object@images$image =  new(
    Class = 'SlideSeq',
    assay = "Spatial",
    key = "image_",
    coordinates = DT
  )
  
  #removing empty spots
  toKeep <- colnames(seurat.object@assays$Spatial@counts)[(colSums(seurat.object@assays$Spatial@counts) >= min.features)]
  seurat.object = subset(seurat.object, cells = toKeep)
  
  return(seurat.object)
}


################################################################################
Create_CosMX_seurat_tx=function(tx=NULL, pixel_format = 600, min.features = 5){
  # get window work :
  max_x = max(tx$x_global_px)
  min_x = min(tx$x_global_px)
  
  max_y = max(tx$y_global_px)
  min_y = min(tx$y_global_px)
  
  # get sample genes
  genes = unique(tx[,'target'])
  
  # get n_spot for coordinates
  n_spot = ceiling((max_x-min_x)/pixel_format)*ceiling((max_y-min_y)/pixel_format)
  
  message(c('Preparing output for ',n_spot,' cells'))
  
  low_window_x = min_x
  low_window_y = min_y
  
  low_x = c()
  low_y = c()
  high_x = c()
  high_y = c()

  # fill table
  low_window_x = min_x
  low_window_y = min_y
    
  for (i in 1:ceiling((max_x-min_x)/pixel_format)){
    high_window_x = min_x + i*pixel_format
    for (j in 1:ceiling((max_y-min_y)/pixel_format)){
      high_window_y = min_y + j*pixel_format
      
      low_x = c(low_x, low_window_x)
      low_y = c(low_y, low_window_y)
      high_x = c(high_x, high_window_x)
      high_y = c(high_y,  high_window_y)
      
      low_window_y = min_y + j*pixel_format
    }
    low_window_x = min_x + i*pixel_format
  }
  
  spot_coordinates = data.frame(low_x = low_x, low_y = low_y, high_x = high_x, high_y = high_y)
  x = spot_coordinates$high_x - pixel_format/2
  y = spot_coordinates$high_y - pixel_format/2
  spot_coordinates$x = x
  spot_coordinates$y = y
  spot_coordinates$imagerow = x
  spot_coordinates$imagecol = y
  spot_coordinates$spot = rownames(spot_coordinates)
  
  # plot(spot_coordinates$x,spot_coordinates$y)
  # plot(spot_coordinates$low_x,spot_coordinates$low_y)
  # plot(spot_coordinates$high_x,spot_coordinates$high_y)
  
  #spot_gene_expression$spot = NA
  
  for(k in genes){
    spot_coordinates[,k] = 0
  }
  
  # get real n_spot for coordinates
  n_spot = nrow(spot_coordinates)
  
  message(c('Creating spot:'))
  
  # Initializes the progress bar
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_spot, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar

  
  for(i in 1:n_spot){
    
    #---------------------
    # Code to be executed
    #---------------------
    
    coor_1 = which(tx[,'x_global_px'] < spot_coordinates[i,'high_x'])
    if(!identical(coor_1, integer(0))){
      coor_2 = which(tx[,'y_global_px'] < spot_coordinates[i,'high_y'])
      if(!identical(coor_2, integer(0))){
        coor_3 = which(tx[,'x_global_px'] > spot_coordinates[i,'low_x'])
        if(!identical(coor_3, integer(0))){
          coor_4 = which(tx[,'y_global_px'] > spot_coordinates[i,'low_y'])
          if(!identical(coor_4, integer(0))){
            coor_condition = Reduce(intersect, list(coor_1,coor_2,coor_3,coor_4))
            if(!identical(coor_condition, integer(0))) {
              table = tx[coor_condition,]
              #tx = tx[-coor_condition,] 9585
              table = table(table$target) 
              spot_coordinates[i,names(table)] = table
            }
          }
        }
      }
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  saveRDS(spot_coordinates,paste0("./spot_coordinates_", pixel_format,"px.RDS"))
  
  genes_table = spot_coordinates[,!colnames(spot_coordinates) %in% c("low_x",
                                 "low_y",
                                 "high_x",
                                 "high_y",
                                 "x",
                                 "y",
                                 "imagerow",
                                 "imagecol",
                                 "spot")]
  genes_table = t(genes_table)
  colnames(genes_table) = as.character(1:ncol(genes_table))
  
  # creating the seurat object
  seurat.object = CreateSeuratObject(genes_table,
                                     project = "CTD",
                                     assay = "Spatial",
                                     names.field = 1,
                                     names.delim = "_",
                                     meta.data = NULL,
                                     min.cells = 0,
                                     min.features = 0,
                                     row.names = NULL,
  )
  
  DT = spot_coordinates[,colnames(spot_coordinates) %in% c("x","y","imagerow","imagecol")]
  
  seurat.object@images$image =  new(
    Class = 'SlideSeq',
    assay = "Spatial",
    key = "image_",
    coordinates = DT
  )
  
  #removing empty spots
  toKeep <- colnames(seurat.object@assays$Spatial@counts)[(colSums(seurat.object@assays$Spatial@counts) >= min.features)]
  seurat.object = subset(seurat.object, cells = toKeep)
  
  # return the seurat object
  return(seurat.object)
}

