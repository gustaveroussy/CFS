#' ICA Spatial
#'
#' Complete ICA analysis on the spatial sample after normalisation and regression. Then correct sign and determine stats from IC and Genes
#' @param data Seurat object to analyse
#' @param ncis Number of independant component to separate in the sample
#' @param maxit Number of iterations
#' @param method Method to use between "icafast", "icaimax", "icajade"
#' @param sd Value of the kurtosis filter on IC
#' 
#' @return An analysed Seurat object
#' @examples 
#' data = ICASpatial(data=data,ncis=100,maxit=600,method="icafast",sd=3)
#' 
#' @export
ICASpatial=function(data=NULL,ncis=100,maxit=600,method="icafast",sd=3,...){
  
  data<-RunICA(data,verbose = TRUE,nics = ncis,maxit=maxit,ica.function = method)
  data@reductions$ica@feature.loadings=correct_sign(data@reductions$ica@feature.loadings)
  data@reductions$ica@cell.embeddings=correct_sign(data@reductions$ica@cell.embeddings)
  rownames(data@reductions$ica@cell.embeddings)=colnames(data@assays$Spatial@data)
  
  # determine genes stats
  kurt_cob=apply(data@reductions$ica@feature.loadings,2,function(x){kurtosis(x)})
  Contrib_logic =apply(data@reductions$ica@feature.loadings,2,function(x){abs((x-mean(x))/sd(x))>sd})
  Contrib_gene <- purrr::map2(as.data.frame(Contrib_logic),colnames(as.data.frame(Contrib_logic)),function(x,y){data.frame("gene"=rownames(data@reductions$ica@feature.loadings)[x],"Sig"=as.data.frame(data@reductions$ica@feature.loadings)[x,y])})
  purrr::map(Contrib_gene[names(which(kurt_cob>3))],~ .x$gene ) %>% unlist %>% unique -> list_all_gene
  
  data@misc$GeneAndStat <- list(Kurtosis_ICs=kurt_cob,Contrib_gene=Contrib_gene,All_Contrib_Gene=list_all_gene)
  
  return(data)
}
