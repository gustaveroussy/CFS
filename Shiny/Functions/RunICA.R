ICASpatial=function(x=NULL,ncis=100,maxit=600,method="icafast",...){
  
  x<-RunICA(x,verbose = FALSE,nics = ncis,maxit=maxit,ica.function = "icafast")
  x@reductions$ica@feature.loadings=correct_sign(x@reductions$ica@feature.loadings)
  x@reductions$ica@cell.embeddings=correct_sign(x@reductions$ica@cell.embeddings)
  rownames(x@reductions$ica@cell.embeddings)=colnames(x@assays$Spatial@data)
  return(x)    
}
