#' ICA Spatial
#'
#' Complete ICA analysis on the spatial sample after normalisation and regression. Then correct sign and determine stats from IC and Genes
#' @param data Seurat object to analyse
#' @param nics Number of independant component to separate in the sample
#' @param maxit Number of iterations
#' @param method Method to use between "icafast", "icaimax", "icajade"
#' @param kurtosis Value of the kurtosis filter on IC
#' @param sd Value of the standard deviation used for ICA genes relation selection
#' 
#' @return An analysed Seurat object
#' @examples 
#' data = ICASpatial(data=data,ncis=100,maxit=600,method="icafast",sd=3)
#' 
#' @export
ICASpatial=function(data=NULL,nics=100,maxit=600,method="icafast", kurtosis = 3, sd=3,...){
  
  set.seed(seed = 42)
  
  object = data@assays$SCT@scale.data
  
  nics <- min(nics, ncol(x = data))
  ica.fxn <- eval(expr = parse(text = method))
  
  ica.results <- ica.fxn(t(x = as.matrix(object)), nc = nics, maxit=maxit)
  cell.embeddings <- ica.results$S
  
  feature.loadings <- (as.matrix(x = object) %*% as.matrix(x = cell.embeddings))
  
  colnames(x = feature.loadings) <- paste0("IC_", 1:ncol(x = feature.loadings))
  colnames(x = cell.embeddings) <- paste0("IC_", 1:ncol(x = cell.embeddings))
  rownames(x = cell.embeddings) <- Cells(data)
  
  reduction.data <- CreateDimReducObject(
    embeddings = cell.embeddings,
    loadings = feature.loadings,
    assay = "SCT",
    key = "IC_"
  )
  
  data@reductions$ica = reduction.data
  
  data = correct_sign(data)
  
  # determine genes stats
  kurt_cob=apply(data@reductions$ica@feature.loadings,2,function(x){kurtosis(x)})
  Contrib_logic =apply(data@reductions$ica@feature.loadings,2,function(x){abs((x-mean(x))/sd(x))>sd})
  Contrib_gene <- purrr::map2(as.data.frame(Contrib_logic),colnames(as.data.frame(Contrib_logic)),function(x,y){data.frame("gene"=rownames(data@reductions$ica@feature.loadings)[x],"Sig"=as.data.frame(data@reductions$ica@feature.loadings)[x,y])})
  purrr::map(Contrib_gene[names(which(kurt_cob>kurtosis))],~ .x$gene ) %>% unlist %>% unique -> list_all_gene
  
  data@misc$GeneAndStat <- list(Kurtosis_ICs=kurt_cob,Contrib_gene=Contrib_gene,All_Contrib_Gene=list_all_gene, kurtosis_value = kurtosis)
  
  return(data)
}
