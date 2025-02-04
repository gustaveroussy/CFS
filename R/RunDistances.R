#' Run Distances
#'
#' Create proximity tables between values of choice using a method of choice
#' @param data Seurat object
#' @param images names of the sample(s) to calculate distances for
#' @param method method to use for distances
#' @param value1/2 Two variable within the object to calculate distances between
#' @param keep_positive_values Keep only the positive side of values to calculate de distances
#' 
#' @return Seurat object with added distances in misc
#' 
#' @examples 
#' data = Run_distances(data = data, images = c("sample"), method = c("Lee"), value1 = c("ica"), value2 = c("ica"), keep_positive_values = T)
#' 
#' @export
Run_distances <- function(data = NULL, images = NULL, method = c("Lee"), value1 = c("ica"), value2 = c("ica"), keep_positive_values = T){
  # get values
  table_sample = values$data@reductions[[value1]]@cell.embeddings
  
  if(value1 != "ica"){
    colnames(table_sample) = values$data@misc$reduction_names[[value1]]
  }
  
  if(keep_positive_values){
    table_sample[table_sample < 0] = 0
  }
  
  table_sample_2 = values$data@reductions[[value2]]@cell.embeddings
  
  if(value2 != "ica"){
    colnames(table_sample_2) = values$data@misc$reduction_names[[value2]]
  }
  
  if(keep_positive_values){
    table_sample_2[table_sample_2 < 0] = 0
  }
  
  
  if(length(values$data@images) > 1){
    table_sample = table_sample[grepl(paste0(images,"_[ACGT]+"), rownames(table_sample)),]
    table_sample_2 = table_sample_2[grepl(paste0(images,"_[ACGT]+"), rownames(table_sample_2)),]
  }
  
  # calculate distances
  knn = knearneigh(GetTissueCoordinates(data, images), k=6, longlat = NULL, use_kd_tree=TRUE)
  neighbours = knn2nb(knn, row.names = NULL, sym = FALSE)
  listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)
  
  if(value1 == value2){
    df = t(combn(unique(colnames(table_sample)),2))
    df = as.data.frame(df)
    colnames(df) = c(value1,paste0(value2,"_2"))
    
  } else {
    df = expand_grid(unique(colnames(table_sample)),unique(colnames(table_sample_2)))
    df = as.data.frame(df)
    colnames(df) = c(value1,value2)
  }
  
  if(nrow(df) > 0){
    
    x = apply(df,1,function(x){n = lee(table_sample[,x[1]], table_sample_2[,x[2]], listw, nrow(table_sample), zero.policy=attr(listw, "zero.policy"));return(n)})
    
    df[,"weight"] = unlist(lapply(x,function(n){return(n$L)}))
    
    data@misc$distances[[paste0(value1,"_",value2)]][[images]][[method]] = df
  } else {
    message("No LR interaction found within selected sample")
  }
  
  return(data)
  
}