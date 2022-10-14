######################
#ICI un essai de calcul de pseudottime a partir des IC, en utilsant une parti de mmonocle je crois me ssouveniir


data=adataZ2@reductions$ica@cell.embeddings[which(adataZ2@meta.data$aneuploid=="aneuploid"),c(2,3,4,5,7,10,11,12,15,17,18)]
dm=DiffusionMap(data)
dpt <- DPT(dm,w_width = 0.1)
pseudotime_dpt <- rank(dpt$dpt)

# AFFICHAGE des differents variables calculés
adataZ2@meta.data$dm=rep(NA,length( adataZ2$seurat_clusters))
adataZ2@meta.data$dm[which(adataZ2@meta.data$aneuploid=="aneuploid")]=pseudotime_dpt
adataZ2@meta.data$DC1=rep(NA,length( adataZ2$seurat_clusters))
adataZ2@meta.data$DC1[which(adataZ2@meta.data$aneuploid=="aneuploid")]=dpt$DC1
adataZ2@meta.data$DC2=rep(NA,length( adataZ2$seurat_clusters))
adataZ2@meta.data$DC2[which(adataZ2@meta.data$aneuploid=="aneuploid")]=dpt$DC2
adataZ2@meta.data$DC3=rep(NA,length( adataZ2$seurat_clusters))
adataZ2@meta.data$DC3[which(adataZ2@meta.data$aneuploid=="aneuploid")]=dpt$DC3
adataZ2@meta.data$branch=rep(NA,length( adataZ2$seurat_clusters))
adataZ2@meta.data$branch[which(adataZ2@meta.data$aneuploid=="aneuploid")]=as.factor(dpt$branch)
plot(dm@eigenvectors[,c(1,2)],col=adataZ2$seurat_clusters[which(adataZ2@meta.data$aneuploid=="aneuploid")])
plot(dm@eigenvectors[,c(1,2)],col=pseudotime_dpt)
SpatialFeaturePlot(adataZ2,features = "dm")
SpatialFeaturePlot(adataZ2,features = "DC1")
SpatialFeaturePlot(adataZ2,features = "DC2")
SpatialFeaturePlot(adataZ2,features = "DC3")
SpatialFeaturePlot(adataZ2,features = "branch")
plot(dpt)
plot(dpt, root = 1, paths_to = c(2,3), col_by = 'branch')
plot(dpt, col_by = 'branch', divide = 3, dcs = c(-1,-3,2), pch = 20)
plot(dpt, divide = 3, dcs = c(-1,-3,2), pch = 20)

# tentative de retrouver les genes dont l'expression evolue en fonctiion du pseudotime, mais ne fonctionne pas, probleme de code

library(glmnet)
CVGL=cv.glmnet(x=(adataZ2@reductions$ica@cell.embeddings[which(adataZ2@meta.data$aneuploid=="aneuploid"),]),y=(dpt$DC3),family = "gaussian",alpha=1,nlambda=100)
coef(CVGL,s = "lambda.min") %>% as.data.frame() %>% sort(decreasing=T)