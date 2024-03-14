#' Normalize data
#'
#' Determine mitochondrial ratio, hb ratio and normalise using SCTransform
#' 
#' @param data Seurat object to analyse
#' @param organism "Hs" (human) or "Mm" (mice)
#' @param variable_features Number of variable features to use for the SCTransform
#' 
#' @return an object with normalized data
#' 
#' @examples 
#' data <- PrepNormData(data=data,organism="Hs",variable_features=20000)
#' @export
PrepNormData=function(data=NULL,organism="Hs",variable_features=20000, , min_cells = 5){
  if(organism=="Hs"){
    data <- PercentageFeatureSet(data, "^MT-", col.name = "percent_mito")
    data <- PercentageFeatureSet(data, "^HB.*-", col.name = "percent_hb")
  } else if(organism=="Mm"){
    data <- PercentageFeatureSet(data, "^Mt-", col.name = "percent_mito")
    data <- PercentageFeatureSet(data, "^Hb.*-", col.name = "percent_hb")        
  }
  else{stop("error either Hs or Mm")}
  #regress out nCount_spatial
  data<- SCTransform(data,assay = "Spatial",variable.features.n = variable_features,vars.to.regress="nCount_Spatial", return.only.var.genes = FALSE, min_cells = min_cells)
  return(data)        
}
