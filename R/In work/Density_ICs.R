Density_ICs=function(data=NULL,type=NULL){
  ic_types=data@reductions$ica@cell.embeddings[,type]
  l=length(type)  
  if(l>1){
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    ic_types<-cbind(data@images$slice1@coordinates,t(ic_types)) %>%  cbind(.,sum_IC) %>% as.tibble
    grid=interp(ic_types$imagecol,ic_types$imagerow,ic_types$sum_IC)
    griddf <- data.frame(x = rep(grid$x, ncol(grid$z)), 
                         y = rep(grid$y, each = nrow(grid$z)), 
                         z = as.numeric(grid$z))    
    griddf$z2=ifelse(griddf$z<quantile(griddf$z,na.rm = TRUE,probs = seq(0, 1, 1/10))[2],0,griddf$z)
  }
}
