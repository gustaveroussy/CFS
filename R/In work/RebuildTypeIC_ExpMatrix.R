RebuildTypeIC_ExpMatrix=function(data=NULL,type=NULL){
  return(tcrossprod(apply(data@reductions$ica@feature.loadings[,type],2,NegByZero),apply(data@reductions$ica@cell.embeddings[,type],2,NegByZero)))
}

NegByZero=function(x=NULL){
  return(ifelse(x<0,0,x))
}
