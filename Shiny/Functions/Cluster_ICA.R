Cluster_ICA=function(adata=NULL,ICs=c(1),res=1.2){
  
  adata <- FindNeighbors(adata, reduction = "ica",dims = ICs)
  adata <- FindClusters(adata, verbose = FALSE,resolution=res)
  adata <- RunUMAP(adata, reduction = "ica",dims = ICs)
  
  return(adata)
}