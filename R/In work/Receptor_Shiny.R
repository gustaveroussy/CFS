# Normalize data
IC_SUM=function(data=NULL,type=NULL){
  ic_types <-data@reductions$ica@cell.embeddings[,type]
  ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
  sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
  sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
  ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
  ic_types<-cbind(data@images$slice1@coordinates,t(ic_types)) %>%  cbind(.,sum_IC)
  return(ic_types)
}

# return negative values as 0
NegByZero=function(x=NULL){
  return(ifelse(x<0,0,x))
}

#####################################################################

# Find the density in common between the two ICs
# Find the density in common between the two ICs
Diversity_IC=function(data=NULL,type1=NULL,type2=NULL){
  
  if(length(type1)>1){
    type1=IC_SUM(data,type1)
  }else{
    type1=data.frame(sum_IC=data@reductions$ica@cell.embeddings[,type1])
  }
  if(length(type2)>1){
    type2=IC_SUM(data,type2)
  }else{
    type2=data.frame(sum_IC=data@reductions$ica@cell.embeddings[,1])
  }

  Div=NegByZero(scale(type1[,'sum_IC']))^2*NegByZero(scale(type2[,'sum_IC']))^2

  return(Div)
}

Diversity_gene=function(data=NULL,gene1=NULL,gene2=NULL){
  M_gene1 = data@assays[["SCT"]]@data[gene1,]
  M_gene2 = data@assays[["SCT"]]@data[gene2,]
  
  Div_gen=NegByZero(scale(M_gene1))^2*NegByZero(scale(M_gene2))^2
  
  return(Div_gen)
}

# croisement entre les expressions
print_receptor_ligand_interaction_final=function(data=NULL,LR_pair=NULL, type1 = 'glycolysis', type2 = 'ECM', threshold = 0.1){
  # dev.off()
  
  IC_type1 = rownames(data@misc$annotation)[grep(type1, data@misc$annotation)-length(data@misc$annotation[,'Use'])]
  IC_type2 = rownames(data@misc$annotation)[grep(type2, data@misc$annotation)-length(data@misc$annotation[,'Use'])]
  
  DIV_IC=Diversity_IC(data, IC_type1, IC_type2)
  data@meta.data[[paste0('Di_',type1,'_',type2)]]=DIV_IC

  for (i in 1:length(LR_pair$ligand_gene_symbol)) {
    ligand = as.data.frame(LR_pair[i,2])[1,]
    recepteur = as.data.frame(LR_pair[i,3])[1,]
    if (ligand %in% rownames(data@assays[["SCT"]]@data) && recepteur %in% rownames(data@assays[["SCT"]]@data)){
      
      DIV_gen=Diversity_gene(data, ligand, recepteur)
      data@meta.data[[paste0('Di_gen_',ligand,'_',recepteur)]] = DIV_gen
      
      if (sum(DIV_gen) != 0 && sum(DIV_IC)) {
        corObserved = cor(data@meta.data[[paste0('Di_gen_',ligand,'_',recepteur)]],data@meta.data[[paste0('Di_',type1,'_',type2)]])[1,1]
        if (corObserved > threshold){
          
          # pdf(paste0(ligand,'_',recepteur,".pdf"))
          
          print(corObserved)
          print(ligand)
          print(recepteur)
          
          #DIV_IC_temp = sample(DIV_IC)
          # resample
          # N = 10000
          # corPerm <- numeric(length = N)
          # for(i in 1:N)
          # {
          #   shufdata <- DIV_IC[sample(nrow(DIV_IC)),]
          #   corPerm[i] <- cor(shufdata, DIV_gen)
          # }
          # 
          # p_value_Cor <- (sum(corPerm>=corObserved)+1)/length(corPerm)
          
          # print(SpatialFeaturePlot(data,features=c(paste0('Di_gen_',ligand,'_',recepteur),paste0('Di_',type1,'_',type2),ligand,recepteur)))
          # print(ggplot() + annotate("text", x=6, y=10, label= paste0(corObserved,'\npval = ',p_value_Cor)) + theme_bw())
          # 
          # hist(corPerm, xlim=range(c(corPerm,corObserved)))
          # abline(v=corObserved, col="red")
          
          ### invert signs
          # diff = DIV_IC - DIV_gen
          # m <- 10^3 * 2
          # nulldist = rep(1234, m)
          # OGteststat <- mean(diff)
          # n = length(diff)
          # set.seed(1234)
          # for (i in 1:m){
          #   signs = sample(c(1,-1), size = n, replace = TRUE)
          #   simdiffs = signs*diff
          #   nulldist[i] = mean(simdiffs)
          # }
          # 
          # lowtail <- sum(nulldist <= OGteststat) + 1
          # uptail <- sum (nulldist >= OGteststat) + 1
          # numerator <- min(lowtail, uptail)
          # denom <- m + 1
          # pval <- 2*numerator/denom
          # 
          # print(SpatialFeaturePlot(data,features=c(paste0('Di_gen_',ligand,'_',recepteur),paste0('Di_',type1,'_',type2),ligand,recepteur)))
          # print(ggplot() + annotate("text", x=6, y=10, label= paste0(corObserved,'\npval = ',p_value_Cor)) + theme_bw())
          # 
          # hist(corPerm, xlim=range(c(corPerm,corObserved)))
          # abline(v=corObserved, col="red")
          
          dev.off()
          
        }
      }
    }
  }
}

# test if the two densities are identicales
m <- 10^3 * 2
nulldist = rep(1234, m)
OGteststat <- mean(table$diff)
n = nrow(table)
set.seed(1234)
for (i in 1:m){
  signs = sample(c(1,-1), size = n, replace = TRUE)
  simdiffs = signs*table$diff
  nulldist[i] = mean(simdiffs)
}

hist(nulldist)
abline(v = OGteststat, col = "red")
(lowtail <- sum(nulldist <= OGteststat) + 1)
(uptail <- sum (nulldist >= OGteststat) + 1)
(numerator <- min(lowtail, uptail))
denom <- m + 1
(pval <- 2*numerator/denom)

t.test(DIV_gen, DIV_IC, paired = TRUE, alternative = "two.sided")

print_receptor_ligand_interaction_glmnet=function(data=NULL,LR_pair=NULL, type1 = Tumor, type2 = Macrophage, threshold = 0.1){
  # dev.off()
  
  IC_type1 = type1
  IC_type2 = type2
  type1 = 'tumor'
  type2 = 'macrophage'
  
  DIV_IC=Diversity_IC(data, c(1), c(9))
  data@meta.data[[paste0('Di_',type1,'_',type2)]]=DIV_IC
  
  table_L_R = matrix(c(1:length(colnames(data@assays$SCT@scale.data))), nrow=length(colnames(data@assays$SCT@scale.data)), byrow=TRUE)
  rownames(table_L_R) = colnames(data@assays$SCT@scale.data)
  
  for (i in 1:length(LR_pair$ligand_gene_symbol)) {
    ligand = as.data.frame(LR_pair[i,2])[1,]
    recepteur = as.data.frame(LR_pair[i,3])[1,]
    if (ligand %in% rownames(data@assays[["SCT"]]@data) && recepteur %in% rownames(data@assays[["SCT"]]@data)){
      
      DIV_gen=Diversity_gene(data, ligand, recepteur)
      colnames(DIV_gen) = paste0(ligand,'_',recepteur)
      
      data@meta.data[[paste0(ligand,'_',recepteur)]] = DIV_gen
      table_L_R = cbind(as.data.frame(table_L_R),as.data.frame(DIV_gen))
      
    }
  }
  table_L_R = table_L_R[,-1]
  GL=glmnet(table_L_R,as.matrix(DIV_IC),family = "gaussian",alpha=0.5)
  Coef_Genes=coef(GL, s = 0.1) %>% as.matrix %>% as.data.frame %>% arrange('1')
  Coef_Genes = data.frame(Coef_Genes) %>%  filter(s1>0) %>% arrange(desc(s1))
  names(data@meta.data)[names(data@meta.data) == 'Di_tumor_macrophage'] <- 'Di_type1_type2'
  names(data@meta.data)[names(data@meta.data) == 'Tumor'] <- 'Type1'
  names(data@meta.data)[names(data@meta.data) == 'Macrophage'] <- 'Type2'
  data@meta.data$IC_3 = data@reductions$ica@cell.embeddings[,'IC_3']
  data@meta.data$IC_4 = data@reductions$ica@cell.embeddings[,'IC_4']
  SpatialFeaturePlot(data,c('Di_type1_type2','B2M_TFRC','Type1','Type2','B2M','TFRC'))
  SpatialFeaturePlot(data,c('Di_type1_type2','GRN_SORT1','Type1','Type2','GRN','SORT1'))
  SpatialFeaturePlot(data,c('Di_type1_type2','S100A9_CD68','Type1','Type2','S100A9','CD68'))
  SpatialFeaturePlot(data,c('Di_type1_type2','LPL_CD44','Type1','Type2','LPL','CD44'))
  SpatialFeaturePlot(data,c('Di_type1_type2','GRN_TNFRSF1A','Type1','Type2','GRN','TNFRSF1A'))
  # SpatialFeaturePlot(data,c('Di_IC_1_IC_2','COL1A2_CD44','IC_1','IC_2','COL1A2','CD44'))
  ### resampling
  N = 2000
  corPerm <- numeric(length = N)
  DIV_IC = data@meta.data$Di_type1_type2
  GL=glmnet(table_L_R,as.matrix(DIV_IC),family = "gaussian",alpha=0.5)
  Coef_Genes=coef(GL, s = 0.1) %>% as.matrix %>% as.data.frame %>% arrange('1')
  Coef_Genes = data.frame(Coef_Genes) %>%  filter(s1>0) %>% arrange(desc(s1))
  corObserved = Coef_Genes
  for(i in 1:N){
    shufdata <- data@meta.data$Di_type1_type2[sample(nrow(data@meta.data$Di_type1_type2)),]
    GL=glmnet(table_L_R,as.matrix(shufdata),family = "gaussian",alpha=0.5)
    Coef_Genes=coef(GL, s = 0.1) %>% as.matrix %>% as.data.frame %>% arrange('1')
    Coef_Genes = data.frame(Coef_Genes) %>%  filter(s1>0) %>% arrange(desc(s1))
    corPerm[i] <- Coef_Genes['B2M_TFRC',]
  }
  p_value_Cor <- (sum(corPerm>=corObserved)+1)/length(corPerm)
  
}
