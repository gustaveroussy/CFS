#SpatialScatterPie from ICs

#je n'arrive pas a afficher l'image du tissu en dessous :


library("magick")

# Here we provide the ICC of interest in a shiny App, we could imagine to select the one to show
Embole=c(48,13,18,26)
type=Embole

# we catch the IC considered there as cell type or activities
ic_types=data@reductions$ica@cell.embeddings[,type]
# we put negative vvalue to 0
ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
#We normalize by the sum
sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
#we calculate the factor of size to reduce pie size where IC are low
sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))

#ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
# We build the final object
ic_types<-cbind(data@images$slice1@coordinates,ic_types) %>%  cbind(.,sum_IC) %>% as.tibble

#We extract the image
img<-grDevices::as.raster(data@images$slice1@image)
#We build the plot
p<-   ggplot( data = ic_types,aes(x = imagecol, y = imagerow)) +
  annotation_raster(img,xmin = 0,xmax = Inf,ymin = 0,ymax = Inf) +
  geom_scatterpie(data = ic_types,
                  aes(x = imagecol, y = (-imagerow),
                      r=sum_IC*120),  
                  cols = paste0("IC_",type),
                  color=NA) +
  coord_equal()+
  theme_void()
p
# we save the plot
pdf("Embole_Test_sccatter_pie.pdf",width=20,height=20)
print(p)
dev.off()
