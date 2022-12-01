UMAPScatterPie_ICS=function(data=NULL,type=NULL){
  ic_types=data@reductions$ica@cell.embeddings[,type]
  
  l=length(type)  
  if(l>1){
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    ic_types<-cbind(data@reductions$umap@cell.embeddings ,t(ic_types)) %>%  cbind(.,sum_IC) %>% as.tibble
    
    p<-   ggplot( data = ic_types,aes(x = UMAP_1, y = UMAP_2)) +
      geom_scatterpie(data = ic_types,
                      aes(x = UMAP_1, y = UMAP_2,
                          r=sum_IC*0.2),  
                      cols = paste0("IC_",type),
                      color=NA) +
      theme_void()
  }
  else{
    
  }
}