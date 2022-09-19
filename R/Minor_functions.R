detect_sign=function(x){
  if(abs(min(x))>max(x)){
    x=(-x)
  }
  return(x)
}

correct_sign=function(x){
  y<-apply(x,2,detect_sign)
  
  return(y)
}



Geo_distance=function(x=NULL,y=NULL,coords=NULL,perm=500){
  Out=spatialEco::crossCorrelation(x,y,coords,k=perm,type = c("LSCI", "GSCI"),scale.xy=F)
  return(Out)
}

Build_Geo_Matrix_And_Graph=function(Out_geo=NULL,val=NULL){
  Comb=combn(length(val),2)
  Comb_list= c() #vector(mode="list", length=dim(Comb)[2])
  for(key in 1:dim(Comb)[2]){ 
    Comb_list <- rbind(Comb_list, c(val[Comb[1,key]],val[Comb[2,key]],((test[[key]]$I)),(test[[key]]$range.p),test[[key]]$local.p,test[[key]]$global.p))
  }
  colnames(Comb_list)=c("X","Y","I","prange","lp","gp")
  g<-Comb_list %>%as.tibble %>% dplyr::filter(I>0 & gp<1) %>% dplyr::select(X,,Y,weight =I) %>% graph_from_data_frame(directed = F) 
  co<-layout_with_fr(g)
  return(Comb_list)
}


Geo_main=function(data=NULL,val=NULL){
  Comb=combn(length(val),2)
  Comb=data.frame(val[Comb[1,]],val[Comb[2,]])
  Inter=intersect(rownames(data@images$slice1@coordinates),rownames(data@reductions$ica@cell.embeddings))
  All_out<-map2(Comb[,1],Comb[,2],~Geo_distance(data@reductions$ica@cell.embeddings[Inter,.x],data@reductions$ica@cell.embeddings[Inter,.y],as.matrix(data@images$slice1@coordinates[Inter,c("imagerow","imagecol")])))
  Build_Geo_Matrix_And_Graph(All_out)
  return(All_out)
}

ScatterPie=function(data=NULL,cell_type_all=NULL,image=NULL){
  SPOTlight::spatial_scatterpie(se_obj = data,
                                cell_types_all = cell_type_all,
                                img_path = image,
                                pie_scale = 0.4)   
}

