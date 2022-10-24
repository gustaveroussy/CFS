######################
#ICI un essai de calcul de pseudotime a partir des IC, en utilisant une parti de monocle.
Spatial_pseudotime=function(data=NULL, IC=NULL){
  
  data_2=data@reductions$ica@cell.embeddings[which(data@meta.data$aneuploid=="aneuploid"),c(2,3,4,5,7,10,11,12,15,17,18)]
  dm=DiffusionMap(data_2)
  dpt <- DPT(dm,w_width = 0.1)
  pseudotime_dpt <- rank(dpt$dpt)
  
  # AFFICHAGE des differents variables calculés
  data@meta.data$dm=rep(NA,length(data$seurat_clusters))
  data@meta.data$dm[which(data@meta.data$aneuploid=="aneuploid")]=pseudotime_dpt
  data@meta.data$DC1=rep(NA,length( data$seurat_clusters))
  data@meta.data$DC1[which(data@meta.data$aneuploid=="aneuploid")]=dpt$DC1
  data@meta.data$DC2=rep(NA,length( data$seurat_clusters))
  data@meta.data$DC2[which(data@meta.data$aneuploid=="aneuploid")]=dpt$DC2
  data@meta.data$DC3=rep(NA,length( data$seurat_clusters))
  data@meta.data$DC3[which(data@meta.data$aneuploid=="aneuploid")]=dpt$DC3
  data@meta.data$branch=rep(NA,length( data$seurat_clusters))
  data@meta.data$branch[which(data@meta.data$aneuploid=="aneuploid")]=as.factor(dpt$branch)
  
  data@misc$dpt <- dpt
  
  #plot(dm@eigenvectors[,c(1,2)],col=data$seurat_clusters[which(data@meta.data$aneuploid=="aneuploid")])
  #plot(dm@eigenvectors[,c(1,2)],col=pseudotime_dpt)
  #SpatialFeaturePlot(data,features = "dm")
  #SpatialFeaturePlot(data,features = "DC1")
  #SpatialFeaturePlot(data,features = "DC2")
  #SpatialFeaturePlot(data,features = "DC3")
  #SpatialFeaturePlot(data,features = "branch")
  #plot(dpt)
  #plot(dpt, root = 1, paths_to = c(2,3), col_by = 'branch')
  #plot(dpt, col_by = 'branch', divide = 3, dcs = c(-1,-3,2), pch = 20)
  #plot(dpt, divide = 3, dcs = c(-1,-3,2), pch = 20)
  
  #CVGL=cv.glmnet(x=(data@reductions$ica@cell.embeddings[which(data@meta.data$aneuploid=="aneuploid"),]),y=(dpt$DC3),family = "gaussian",alpha=1,nlambda=100)
  #coef(CVGL,s = "lambda.min") %>% as.data.frame() %>% sort(decreasing=T)
  
  return(data)
}




# tentative de retrouver les genes dont l'expression evolue en fonctiion du pseudotime, mais ne fonctionne pas, probleme de code

