ICASpatial=function(data=NULL,ncis=100,maxit=600,method="icafast",sd=3,...){
  
  data<-RunICA(data,verbose = FALSE,nics = ncis,maxit=maxit,ica.function = "icafast")
  data@reductions$ica@feature.loadings=correct_sign(data@reductions$ica@feature.loadings)
  data@reductions$ica@cell.embeddings=correct_sign(data@reductions$ica@cell.embeddings)
  rownames(data@reductions$ica@cell.embeddings)=colnames(data@assays$Spatial@data)
  
  # determin genes stats
  kurt_cob=apply(data@reductions$ica@feature.loadings,2,function(x){kurtosis(x)})
  Contrib_logic =apply(data@reductions$ica@feature.loadings,2,function(x){abs((x-mean(x))/sd(x))>sd})
  Contrib_gene <- purrr::map2(as.data.frame(Contrib_logic),colnames(as.data.frame(Contrib_logic)),function(x,y){data.frame("gene"=rownames(data@reductions$ica@feature.loadings)[x],"Sig"=as.data.frame(data@reductions$ica@feature.loadings)[x,y])})
  purrr::map(Contrib_gene[names(which(kurt_cob>3))],~ .x$gene ) %>% unlist %>% unique -> list_all_gene
  
  data@misc$GeneAndStat <- list(Kurtosis_ICs=kurt_cob,Contrib_gene=Contrib_gene,All_Contrib_Gene=list_all_gene)
  
  return(data)
}
