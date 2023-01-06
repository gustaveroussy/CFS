#' Cluster ICA
#'
#' Clusters cells and run UMAP
#' @param data Seurat object to clusterise
#' @param ICs Vector of IC numbers to use for the clusterisation and UMAP
#' @param res Resolution of the clusterisation
#' @return Seurat object with clustered cells and UMAP
#' @examples 
#' data <- Cluster_ICA(data=data,ICs=c(1,2,3,4,5,6,7,8,9),res=1.2)
#' @export
Cluster_ICA=function(data=NULL,ICs=c(1),res=1.2){
  
  data <- FindNeighbors(data, reduction = "ica",dims = ICs)
  data <- FindClusters(data, verbose = FALSE,resolution=res)
  data <- RunUMAP(data, reduction = "ica",dims = ICs)
  
  return(data)
}
