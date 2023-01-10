#' ICs Stats
#'
#' Adds Statistics analysis of ICs to the object
#' @param data Seurat object to analyse
#' @param sd standard deviation contribution to take into account
#' @return Adds a list of analysis of ICs to the object
#' @examples 
#' data <- ICGeneAndStats(data=data,sd=3)
#' @export
ICGeneAndStats=function(data="NULL",sd=3){
  kurt_cob=apply(data@reductions$ica@feature.loadings,2,function(x){kurtosis(x)})
  Contrib_logic =apply(data@reductions$ica@feature.loadings,2,function(x){abs((x-mean(x))/sd(x))>sd})
  Contrib_gene <- purrr::map2(as.data.frame(Contrib_logic),colnames(as.data.frame(Contrib_logic)),function(x,y){data.frame("gene"=rownames(data@reductions$ica@feature.loadings)[x],"Sig"=as.data.frame(data@reductions$ica@feature.loadings)[x,y])})
  purrr::map(Contrib_gene[names(which(kurt_cob>3))],~ .x$gene ) %>% unlist %>% unique -> list_all_gene
  GeneAndStat=list(Kurtosis_ICs=kurt_cob,Contrib_gene=Contrib_gene,All_Contrib_Gene=list_all_gene)
  
  data@misc$GeneAndStat <- GeneAndStat
  
  return(data)
}

