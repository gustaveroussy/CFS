# Normalize data
IC_SUM=function(data=NULL,type=NULL){
  ic_types <-data@reductions$ica@cell.embeddings[,type]
  ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
  sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
  sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
  ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
  ic_types<-cbind(data@images$slice1@coordinates,t(ic_types)) %>%  cbind(.,sum_IC) %>% as.tibble
  return(ic_types)
}

# Find the density in common between the two ICs
Diversity=function(data=NULL,type1=NULL,type2=NULL){
  if(length(type1)>1){
    type1=IC_SUM(data,type1)
  }else{
    type1=data.frame(sum_IC=data@reductions$ica@cell.embeddings[,type1])
  }
  if(length(type2)>1){
    type2=IC_SUM(data,type2)
  }else{
    type2=data.frame(sum_IC=data@reductions$ica@cell.embeddings[,type2])
  }
  # ça c'est la formule du croisement des deux IC
  Div=NegByZero(scale(type1))^2*NegByZero(scale(type2))^2
  # sqrt(NegByZero(scale(type1$sum_IC))^2*NegByZero(scale(type2$sum_IC))^2)
  # NegByZero(scale(type1$sum_IC))*NegByZero(scale(type2$sum_IC))
  # NegByZero(type1$sum_IC)*NegByZero(type2$sum_IC)
  
  GL=glmnet(t(data@assays$SCT@scale.data),as.matrix(Div),family = "gaussian",alpha=0.5)#,nlambda=1000)
  Coef_Genes=coef(GL, s = 0.1) %>% as.matrix %>% as.data.frame %>% arrange('1')
  return(list(Diversity=Div,Coef_Genes=Coef_Genes))
}

# return negative values as 0
NegByZero=function(x=NULL){
  return(ifelse(x<0,0,x))
}

# croisement entre les IC
print_IC_interaction=function(data=NULL,LR_pair=NULL){
  l=combn(all,2)
  i=1
  dev.off()
  while(i<=10){#dim(l)[2]){
    pdf(paste0("./IC_",l[1,i],"IC_",l[2,i],".pdf"))
    DIV=Diversity(data, l[1,i],l[2,i])
    data@meta.data$Di_Im_Tumor=DIV$Diversity
    print(SpatialFeaturePlot(data,features=c("Di_Im_Tumor",paste0("IC_",l[1,i]),paste0("IC_",l[2,i]))))
    
    Coef<-data.frame(S1=DIV$Coef_Genes) %>% rownames_to_column("SYMBOL")
    Coef_lig<-Coef %>% right_join(LR_pair, by=c("SYMBOL"="ligand_gene_symbol") )  %>% dplyr::select(SYMBOL:lr_pair)
    Coef_rec<-Coef %>% right_join(LR_pair, by=c("SYMBOL"="receptor_gene_symbol") )  %>% dplyr::select(SYMBOL:lr_pair)
    Coef=Coef_lig %>% inner_join(Coef_rec,by="lr_pair")
    W=Coef$s1.x*Coef$s1.y
    W=data.frame(Coef,W) %>%  filter(s1.x>0 & s1.y>0) %>% arrange(desc(W))
    j=1
    while(j<=dim(W)[1]){
      print(SpatialFeaturePlot(data,features=c(W$SYMBOL.x[j],W$SYMBOL.y[j])))
      j=j+1
    }
    #SpatialFeaturePlot(data,features="ITGB3")
    i=i+1
    dev.off()
  }
  
  #SpatialFeaturePlot(data,features="Di_Tumor_OSC")
  #SpatialFeaturePlot(data,features="TNFRSF11A")
  #SpatialFeaturePlot(data,features="TNFSF11")
}

# Find the density in common between the two ICs
Diversity_gene=function(data=NULL,gene1=NULL,gene2=NULL){
  M_gene1 = data@assays[["SCT"]]@data[gene1,]
  M_gene2 = data@assays[["SCT"]]@data[gene2,]
  
  # ça c'est la formule du croisement des deux IC
  Div_gen=NegByZero(scale(M_gene1))^2*NegByZero(scale(M_gene2))^2
  
  data@meta.data$Di_gene=Div_gen[,1]
  
  GL=glmnet(data@reductions$ica@cell.embeddings,as.matrix(Div_gen),family = "gaussian",alpha=0.5)
  Coef_Genes=coef(GL, s = 0.1) %>% as.matrix %>% as.data.frame %>% arrange('1')
  return(list(Diversity=Div_gen,Coef_Genes=Coef_Genes))
}

# croisement entre les expressions
print_receptor_ligand_interaction=function(data=NULL,LR_pair=NULL){
  # dev.off()
  
  # IC cross
  IC_numbers = as.integer(str_remove(names(data@misc)[grep("IC_", names(data@misc))], "IC_"))
  combinations = expand.grid(IC_numbers,IC_numbers)
  for (i in 1:10){
    DIV_IC=Diversity(data, combinations[i,1],combinations[i,2])
  }
  
  for (i in 1:10){
    pdf(paste0(as.data.frame(LR_pair[i,2])[1,],as.data.frame(LR_pair[i,3])[1,],".pdf"))
    
    # genes cross
    DIV_gen=Diversity_gene(data, as.data.frame(LR_pair[i,2])[1,],as.data.frame(LR_pair[i,3])[1,])
    
    data@meta.data$Di_Im_Tumor=DIV$Diversity
    print(SpatialFeaturePlot(data,features=c("Di_Im_Tumor",as.data.frame(LR_pair[i,2])[1,],as.data.frame(LR_pair[i,3])[1,])))
    
    Coef<-data.frame(S1=DIV$Coef_Genes) %>% rownames_to_column("IC")
    # Coef_lig<-Coef %>% right_join(data@reductions$ica@cell.embeddings, by=c("IC"="ligand_gene_symbol") )  %>% dplyr::select(IC:lr_pair)
    # Coef_rec<-Coef %>% right_join(data@reductions$ica@cell.embeddings, by=c("IC"="receptor_gene_symbol") )  %>% dplyr::select(IC:lr_pair)
    # Coef=Coef_lig %>% inner_join(Coef_rec,by="lr_pair")
    # W=Coef$s1
    # W=data.frame(Coef,W) %>%  filter(s1 > 0) %>% arrange(desc(W))
    # j=1
    # while(j<=dim(W)[1]){
    #   print(SpatialFeaturePlot(data,features=c(W[j])))
    #   j=j+1
    # }
    #SpatialFeaturePlot(data,features="ITGB3")
    dev.off()
  }
  #SpatialFeaturePlot(data,features="Di_Tumor_OSC")
  #SpatialFeaturePlot(data,features="TNFRSF11A")
  #SpatialFeaturePlot(data,features="TNFSF11")
}

#####################################################################

# Find the density in common between the two ICs
# Find the density in common between the two ICs
Diversity_IC_final=function(data=NULL,type1=NULL,type2=NULL){
  if(length(type1)>1){
    type1=IC_SUM(data,type1)
  }else{
    type1=data.frame(sum_IC=data@reductions$ica@cell.embeddings[,type1])
  }
  if(length(type2)>1){
    type2=IC_SUM(data,type2)
  }else{
    type2=data.frame(sum_IC=data@reductions$ica@cell.embeddings[,type2])
  }
  # ça c'est la formule du croisement des deux IC
  Div=NegByZero(scale(type1))^2*NegByZero(scale(type2))^2
  # sqrt(NegByZero(scale(type1$sum_IC))^2*NegByZero(scale(type2$sum_IC))^2)
  # NegByZero(scale(type1$sum_IC))*NegByZero(scale(type2$sum_IC))
  # NegByZero(type1$sum_IC)*NegByZero(type2$sum_IC)
  
  #GL=glmnet(t(data@assays$SCT@scale.data),as.matrix(Div),family = "gaussian",alpha=0.5)#,nlambda=1000)
  #Coef_Genes=coef(GL, s = 0.1) %>% as.matrix %>% as.data.frame %>% arrange('1')
  return(Div)
}

Diversity_gene_final=function(data=NULL,gene1=NULL,gene2=NULL){
  M_gene1 = data@assays[["SCT"]]@data[gene1,]
  M_gene2 = data@assays[["SCT"]]@data[gene2,]
  
  # ça c'est la formule du croisement des deux IC
  Div_gen=NegByZero(scale(M_gene1))^2*NegByZero(scale(M_gene2))^2
  
  Div=Div_gen
  
  #GL=glmnet(data@reductions$ica@cell.embeddings,as.matrix(Div_gen),family = "gaussian",alpha=0.5)
  #Coef_Genes=coef(GL, s = 0.1) %>% as.matrix %>% as.data.frame %>% arrange('1')
  return(Div)
}

# croisement entre les expressions
print_receptor_ligand_interaction_final=function(data=NULL,LR_pair=NULL){
  # dev.off()
  
  # IC cross
  IC_numbers = as.integer(str_remove(names(data@misc)[grep("IC_", names(data@misc))], "IC_"))
  combinations = expand.grid(IC_numbers,IC_numbers)
  
  DIV_IC = list()
  
  for (i in 1:10){
    if (combinations[i,1] != combinations[i,2]){
      DIV_IC[[paste0(combinations[i,1],"_",combinations[i,2])]]=Diversity_IC_final(data, combinations[i,1],combinations[i,2])
      data@meta.data$Di_IC_1_IC_2=DIV_IC$`2_1`
    }
  }
  
  for (i in 1:10){
    pdf(paste0(as.data.frame(LR_pair[i,2])[1,],as.data.frame(LR_pair[i,3])[1,],".pdf"))
    
    # genes cross
    
    data@meta.data$Di_gen=DIV_gen
    for (i in 1:length(LR_pair$ligand_gene_symbol)) {
      
      if (as.data.frame(LR_pair[i,2])[1,] %in% rownames(data@assays[["SCT"]]@data) && as.data.frame(LR_pair[i,3])[1,] %in% rownames(data@assays[["SCT"]]@data)){
        
        DIV_gen=Diversity_gene_final(data, as.data.frame(LR_pair[i,2])[1,],as.data.frame(LR_pair[i,3])[1,])
        
        data@meta.data$Di_gen = DIV_gen
        
        if (sum(DIV_gen) != 0) {
          if (cor(data@meta.data$Di_gen,data@meta.data$Di_IC_1_IC_2) > 0.01 || cor(data@meta.data$Di_gen,data@meta.data$Di_IC_1_IC_2) < -0.01){
            pdf(paste0('!',as.data.frame(LR_pair[i,2])[1,],'_',as.data.frame(LR_pair[i,3])[1,],".pdf"))
            print(as.data.frame(LR_pair[i,2])[1,])
            print(as.data.frame(LR_pair[i,3])[1,])
            print(cor(data@meta.data$Di_gen,data@meta.data$Di_IC_1_IC_2))
          } else {
            pdf(paste0(as.data.frame(LR_pair[i,2])[1,],'_',as.data.frame(LR_pair[i,3])[1,],".pdf"))
          }
          print(SpatialFeaturePlot(data,features=c("Di_gen","Di_IC_1_IC_2",as.data.frame(LR_pair[i,2])[1,],as.data.frame(LR_pair[i,3])[1,])))
          print(ggplot() + annotate("text", x=6, y=10, label= cor(data@meta.data$Di_gen,data@meta.data$Di_IC_1_IC_2))) + theme_bw()
          
          dev.off()
        }
      }
      
    }
    
    # Coef<-data.frame(S1=DIV$Coef_Genes) %>% rownames_to_column("IC")
    # Coef_lig<-Coef %>% right_join(data@reductions$ica@cell.embeddings, by=c("IC"="ligand_gene_symbol") )  %>% dplyr::select(IC:lr_pair)
    # Coef_rec<-Coef %>% right_join(data@reductions$ica@cell.embeddings, by=c("IC"="receptor_gene_symbol") )  %>% dplyr::select(IC:lr_pair)
    # Coef=Coef_lig %>% inner_join(Coef_rec,by="lr_pair")
    # W=Coef$s1
    # W=data.frame(Coef,W) %>%  filter(s1 > 0) %>% arrange(desc(W))
    # j=1
    # while(j<=dim(W)[1]){
    #   print(SpatialFeaturePlot(data,features=c(W[j])))
    #   j=j+1
    # }
    #SpatialFeaturePlot(data,features="ITGB3")
    dev.off()
  }
  #SpatialFeaturePlot(data,features="Di_Tumor_OSC")
  #SpatialFeaturePlot(data,features="TNFRSF11A")
  #SpatialFeaturePlot(data,features="TNFSF11")
}

