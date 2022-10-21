Ploidie_search_UMAP=function(data=NULL, Ploidie_object = NULL){
  
  pSubClone_umap<-DimPlot(data,group.by="SubClone")
  
  pCount_umap<-FeaturePlot(data,features="nCount_Spatial")

  paneuploid_umap<-DimPlot(data,group.by="aneuploid") 

  paneuploid_spatial<-SpatialDimPlot(data,group.by="aneuploid") 
  
  Ploidie_object$paneuploid_spatial <- paneuploid_spatial
  Ploidie_object$paneuploid_umap <- paneuploid_umap
  Ploidie_object$pSubClone_umap <- pSubClone_umap
  Ploidie_object$pSubClone_umap <- pCount_umap
  

  
  return(Ploidie_object)
}

