PrepNormData=function(data=NULL,organism="Hs",variable_features=20000){
  if(organism=="Hs"){
    data <- PercentageFeatureSet(data, "^MT-", col.name = "percent_mito")
    data <- PercentageFeatureSet(data, "^HB.*-", col.name = "percent_hb")
  } else if(organism=="Mm"){
    data <- PercentageFeatureSet(data, "^Mt-", col.name = "percent_mito")
    data <- PercentageFeatureSet(data, "^Hb.*-", col.name = "percent_hb")        
  }
  else{stop("error either Hs or Mm")}
  #regress out nCount_spatial
  data<- SCTransform(data,assay = "Spatial",variable.features.n = variable_features,vars.to.regress="nCount_Spatial")
  return(data)        
}
