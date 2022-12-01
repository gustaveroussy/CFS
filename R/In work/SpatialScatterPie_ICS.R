SpatialScatterPie_ICS=function(data=NULL,type=NULL){
  ic_types=data@reductions$ica@cell.embeddings[,type]
  img<-grDevices::as.raster(data@images$slice1@image)
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
    p<-   ggplot( data = ic_types,aes(x = imagecol, y = imagerow)) +
      annotation_raster(img,xmin = 0,xmax = Inf,ymin = 0,ymax = Inf) +
      geom_scatterpie(data = ic_types,
                      aes(x = imagecol, y = (-imagerow),
                          r=sum_IC*120),  
                      cols = paste0("IC_",type),
                      color=NA) +
      coord_equal()+
      theme_void()
    
    p1<-   ggplot( data = ic_types,aes(x = imagecol, y = imagerow)) +
      annotation_raster(img,xmin = 0,xmax = Inf,ymin = 0,ymax = Inf) +
      geom_scatterpie(data = ic_types,
                      aes(x = imagecol, y = (-imagerow),
                          r=sum_IC*120),  
                      cols = paste0("IC_",type),
                      color=NA)+ geom_contour(data = griddf,aes(x = x, y = (-y) , z=z2),binwidth = 0.4)  +
      coord_equal()+
      theme_void()
    
    
    p2<- ggplot(data = ic_types,aes(x = imagecol, y = imagerow))+
      annotation_raster(img,xmin = 0,xmax = Inf,ymin = 0,ymax = Inf) + geom_contour_filled(data = griddf,aes(x = x, y = (-y) , z=z)) +
      coord_equal()+
      theme_void()
    p3<- ggplot(data = griddf,aes(x = x, y = (-y) , z=z2))+
      annotation_raster(img,xmin = 0,xmax = Inf,ymin = 0,ymax = Inf) + geom_contour(binwidth = 0.4) +
      coord_equal()+
      theme_void()    
    
  }else{
    p<-SpatialFeaturePlot(BREAST,features=paste0("IC_",type))
    #  p1<- ggplot(aes(data = ic_types,aes(x = imagecol, y = imagerow))) + geom_contour(aes(z=ic_types[,3]))
  }
}
