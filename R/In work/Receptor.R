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
  Div=scale(type1$sum_IC)^2+scale(type2$sum_IC)^2
  
  GL=glmnet(t(data@assays$SCT@scale.data),as.matrix(Div),family = "gaussian",alpha=0.5)#,nlambda=1000)
  Coef_Genes=coef(GL, s = 0.1) %>% as.matrix %>% as.data.frame %>% arrange('1')
  return(list(Diversity=Div,Coef_Genes=Coef_Genes))
}

# return negative values as 0
NegByZero=function(x=NULL){
  return(ifelse(x<0,0,x))
}

# croisement entre les expressions
print_receptor_ligand_interaction=function(data=NULL,LR_pair=NULL){
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
