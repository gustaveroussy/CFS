#' Fahrenheit conversion
#'
#' Convert degrees Fahrenheit temperatures to degrees Celsius
#' @param F_temp The temperature in degrees Fahrenheit
#' @return The temperature in degrees Celsius
#' @examples 
#' temp1 <- F_to_C(50);
#' temp2 <- F_to_C( c(50, 63, 23) );
#' @export
sub_Cluster_ICA=function(data=NULL,ICs=c(1), ncis = 30,organism = "Hs", res=1.2, variable_features=20000){
  
  PrepNormData(data=data,organism=organism,variable_features=variable_features)
  data=ICASpatial(x=data,ncis=ncis,maxit=600,method="icafast")
  Stat_data=ICGeneAndStats(data=data,sd=3)
  
  adata <- FindNeighbors(data, reduction = "ica",dims = ICs)
  adata <- FindClusters(data, verbose = FALSE,resolution=res)
  adata <- RunUMAP(data, reduction = "ica",dims = ICs)
  
  return(adata)
}
