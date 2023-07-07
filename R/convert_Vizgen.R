################################################################################
# Convert vizgen data using fov matrix
################################################################################
Convert_vizgen_fov=function(spot_gene_expression=NULL, fov_matrix = NULL){
  
  # coordinates calculation
  x = c()
  y = c()
  for(i in 1:nrow(spot_gene_expression)){
    fov_coor = which(fov_matrix==i,arr.ind=TRUE)
    x = c(x,fov_coor[,'row'])
    y = c(y,fov_coor[,'col'])
  }
  
  DT = data.frame(x = x, y = y, imagerow = x, imagecol = y, stringsAsFactors=FALSE)
  
  seurat.object@images$image =  new(
    Class = 'SlideSeq',
    assay = "Spatial",
    key = "image_",
    coordinates = DT
  )
  
  #removing empty spots
  toKeep <- colnames(seurat.object@assays$Spatial@counts)[!(colSums(seurat.object@assays$Spatial@counts) == 0)]
  seurat.object = subset(seurat.object, cells = toKeep)
  
  # return the seurat object
  return(seurat.object)
}

################################################################################
# Convert vizgen data using grid based method
################################################################################

Create_vizgen_seurat=function(spot_gene_expression=NULL, pixel_format = 40, min.features = 5){
  # get window work :
  max_x = max(spot_gene_expression$global_x)
  min_x = min(spot_gene_expression$global_x)
  
  max_y = max(spot_gene_expression$global_y)
  min_y = min(spot_gene_expression$global_y)
  
  # get sample genes
  genes = unique(spot_gene_expression[,'gene'])
  
  # get n_spot for coordinates
  n_spot = ceiling((max_x-min_x)/pixel_format)*ceiling((max_y-min_y)/pixel_format)
  
  low_window_x = min_x
  low_window_y = min_y
  
  low_x = c()
  low_y = c()
  high_x = c()
  high_y = c()
  
  # fill table
  low_window_x = min_x
  low_window_y = min_y
  
  # fill table
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
  
  for(i in 1:n_spot){
    print(i)
    table = spot_gene_expression[which(spot_gene_expression[,'global_x'] > spot_coordinates[i,'low_x']  & spot_gene_expression[,'global_y'] > spot_coordinates[i,'low_y']  & spot_gene_expression[,'global_x'] < spot_coordinates[i,'high_x'] & spot_gene_expression[,'global_y'] < spot_coordinates[i,'high_y']),]
    table = table(table$gene) 
    spot_coordinates[i,names(table)] = table
  }
  
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
