######################

Spatial_correlation=function(data=NULL, IC=NULL){
  
  # determine weight graph.
  
  data <- FindNeighbors(data, reduction = "ica",dims = c(2,3,4,5,7,10,11,12,15,17,18), return.neighbor = TRUE)
  
  table_1 = data@neighbors$SCT.nn@nn.idx
  table_2 = data@neighbors$SCT.nn@nn.dist
  
  summary(w)
  
  w <- 
  
  ###
  
  coordinates <- GetTissueCoordinates(data)
  x <- coordinates$imagerow
  xbar <- mean(x)
  y <- coordinates$imagecol
  ybar <- mean(y)
  
  n <- length(y)
  
  dy <- y - ybar
  g <- expand.grid(dy, dy)
  yiyj <- g[,1] * g[,2]
  
  pm <- matrix(yiyj, ncol=n)
  
  return(data)
}

