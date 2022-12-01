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
  Div=scale(type1$sum_IC)*scale(type2$sum_IC)
  GL=glmnet(t(data@assays$SCT@scale.data),as.matrix(Div),family = "gaussian",alpha=0.5)#,nlambda=1000)
  Coef_Genes=coef(GL, s = 0.1) %>% as.matrix %>% as.data.frame %>% arrange('1')
  return(list(Diversity=Div,Coef_Genes=Coef_Genes))
}
