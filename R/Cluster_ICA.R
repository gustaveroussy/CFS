#' Fahrenheit conversion
#'
#' Convert degrees Fahrenheit temperatures to degrees Celsius
#' @param F_temp The temperature in degrees Fahrenheit
#' @return The temperature in degrees Celsius
#' @examples 
#' temp1 <- F_to_C(50);
#' temp2 <- F_to_C( c(50, 63, 23) );
#' @export
Cluster_ICA=function(adata=NULL,ICs=c(1),res=1.2){
  
  adata <- FindNeighbors(adata, reduction = "ica",dims = ICs)
  adata <- FindClusters(adata, verbose = FALSE,resolution=res)
  adata <- RunUMAP(adata, reduction = "ica",dims = ICs)
  
  return(adata)
}