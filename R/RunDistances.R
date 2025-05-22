#' Run Distances
#'
#' Create proximity tables between values of choice using a method of choice
#' @param data Seurat object
#' @param images names of the sample(s) to calculate distances for
#' @param method method to use for distances
#' @param value1/2 Two variable within the object to calculate distances between
#' @param keep_positive_values Keep only the positive side of values to calculate de distances
#' @param gene_filter_table datatable with two columns of genes to compare when "value1" and "value2" == "Genes", use all genes from the table when only one "value" == "Genes", unused otherwise.
#' @param metadata_filter Names of the metadata column to use for the distance analysis
#' 
#' @return Seurat object with added distances in misc
#' 
#' @examples 
#' data = Run_distances(data = data, images = c("sample"), method = c("Lee"), value1 = c("ica"), value2 = c("ica"), keep_positive_values = T)
#' 
#' @export
Run_distances <- function(data = NULL, images = NULL, method = c("Lee"), value1 = c("ica", "Genes", "Metadata"), value2 = c("ica", "Genes", "Metadata"), keep_positive_values = T, gene_filter_table = NULL, metadata_filter = NULL){
  
  # metadata handling
  if (is.null(metadata_filter)){
    metadata_filter = colnames(data@meta.data[,sapply(data@meta.data, is.numeric)])
  }
  
  if(length(method) > 1){
    method = method[1]
  }
  
  if(length(value1) > 1){
    value1 = value1[1]
  }
  
  if(length(value2) > 1){
    value2 = value2[1]
  }
  
  if(!is.logical(keep_positive_values)){
    stop("keep_positive_values needs to be TRUE/FALSE")
  }
  
  for(sample in images){
  
    #################################
    # Search the first table to use
    #################################
    
    if (value1 == "Genes") {
      if(class(data@assays$Spatial) == "Assay5"){
        
        New<- JoinLayers(data@assays$Spatial)
        table_sample = raster::t(GetAssayData(New))
        rm(New)
        gc()
        
      } else {
        table_sample = raster::t(GetAssayData(data))
      }
      
    } else if (value1 != "Genes") {
      
      if (value1 == "Metadata") {
        
        table_sample = data@meta.data[,sapply(data@meta.data, is.numeric)]
        table_sample = table_sample[,metadata_filter]
        
      } else {
        table_sample = data@reductions[[value1]]@cell.embeddings
        
        if(value1 != "ica"){
          colnames(table_sample) = data@misc$reduction_names[[value1]]
        }
        
      }
      
      if(keep_positive_values){
        table_sample[table_sample < 0] = 0
      }
      
    }
    
    #################################
    # Search the second table to use
    #################################
    
    if (value2 == "Genes") {
      
      if(class(data@assays$Spatial) == "Assay5"){
        
        New<- JoinLayers(data@assays$Spatial)
        table_sample_2 = raster::t(GetAssayData(New))
        rm(New)
        gc()
        
      } else {
        table_sample_2 = raster::t(GetAssayData(data))
      }
      
    } else if (value2 != "Genes") {
      if (value2 == "Metadata") {
        
        table_sample_2 = data@meta.data[,sapply(data@meta.data, is.numeric)]
        table_sample_2 = table_sample_2[,metadata_filter]
        
        
      } else {
        
        table_sample_2 = data@reductions[[value2]]@cell.embeddings
        
        if(value2 != "ica"){
          colnames(table_sample_2) = data@misc$reduction_names[[value2]]
        }
      }
      
      if(keep_positive_values){
        table_sample_2[table_sample_2 < 0] = 0
      }
      
    }
    
    if(length(data@images) > 1){
      table_sample = table_sample[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample)),]
      table_sample_2 = table_sample_2[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample_2)),]
    }
    
    #################################
    # Search neighbors
    #################################
    
    if(method == "Lee"){
      
      knn = knearneigh(GetTissueCoordinates(data,sample), k=6, longlat = NULL, use_kd_tree=TRUE)
      neighbours = knn2nb(knn, row.names = NULL, sym = FALSE)
      listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)
      
      if (value1 == "Genes" & value2 == "Genes"){
        if(is.null(gene_filter_table)){
          lr = read.delim(paste0(system.file(package = "CFS"), "/tmp_data/human_lr_pair.csv"))$lr_pair
          
          df <- data.frame(lr=lr)
          df <- df %>% separate(lr, into = c(value1, paste0(value2,"_2")), sep = "_")
        } else {
          df = read.csv(gene_filter_table)
          colnames(df) = c(value1, paste0(value2,"_2"))
        }
        
        table_sample = as.data.frame(table_sample[,colnames(table_sample) %in% unique(c(df[,1],df[,2]))])
        table_sample_2 = as.data.frame(table_sample_2[,colnames(table_sample_2) %in% unique(c(df[,1],df[,2]))])
        
        df = df[df[,1] %in% colnames(table_sample)[colSums(table_sample) > 0],]
        df = df[df[,2] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
        
      } else if (value1 == "Genes") {
        if(is.null(gene_filter_table)){
          lr = read.delim(paste0(system.file(package = "CFS"), "/tmp_data/human_lr_pair.csv"))$lr_pair
          
          df <- data.frame(lr=lr)
          df <- df %>% separate(lr, into = c('l', 'r'), sep = "_")
        } else {
          df = read.csv(gene_filter_table)
          colnames(df) = c("l", "r")
        }
        
        table_sample = as.data.frame(table_sample[,colnames(table_sample) %in% unique(c(df$l,df$r))])
        
        df = df[df[,1] %in% colnames(table_sample)[colSums(table_sample) > 0],]
        df = df[df[,2] %in% colnames(table_sample)[colSums(table_sample) > 0],]
        
        df = expand_grid(unique(c(df$l,df$r)),unique(colnames(table_sample_2)))
        df = as.data.frame(df)
        colnames(df) = c(value1,value2)
        
      } else if(value2 == "Genes"){
        if(is.null(gene_filter_table)){
          lr = read.delim(paste0(system.file(package = "CFS"), "/tmp_data/human_lr_pair.csv"))$lr_pair
          
          df <- data.frame(lr=lr)
          df <- df %>% separate(lr, into = c('l', 'r'), sep = "_")
        } else {
          df = read.csv(gene_filter_table)
          colnames(df) = c("l", "r")
        }
        
        table_sample_2 = as.data.frame(table_sample_2[,colnames(table_sample_2) %in% unique(c(df$l,df$r))])
        
        df = df[df[,1] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
        df = df[df[,2] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
        
        df = expand_grid(unique(colnames(table_sample)),unique(c(df$l,df$r)))
        df = as.data.frame(df)
        colnames(df) = c(value1,value2)
        
      } else if(value1 != "Genes" & value2 != "Genes") {
        if(value1 == value2){
          df = t(combn(unique(colnames(table_sample)),2))
          df = as.data.frame(df)
          colnames(df) = c(value1,paste0(value2,"_2"))
          
        } else {
          df = expand_grid(unique(colnames(table_sample)),unique(colnames(table_sample_2)))
          df = as.data.frame(df)
          colnames(df) = c(value1,value2)
        }
      }
      
      if(nrow(df) > 0){
        
        x = apply(df,1,function(x){n = lee(table_sample[,x[1]], table_sample_2[,x[2]], listw, nrow(table_sample), zero.policy=attr(listw, "zero.policy"));return(n)})
        
        df[,"weight"] = unlist(lapply(x,function(n){return(n$L)}))
        
        df = df[!is.na(df[,"weight"]),]
        
        data@misc$distances[[paste0(value1,"_",value2)]][[sample]][[method]] = df
      } else {
        shinyalert("Oops!", "No LR interaction found within selected sample", type = "error")
      }
    }
  }
  
  return(data)
  
}