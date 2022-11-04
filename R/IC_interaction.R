IC_interaction=function(data_h = NULL, data_m = NULL){
  
  data_h <- readRDS('/home/c_thuilliez/Desktop/Work/output/Spatial_PDX/MAP177_PDX_PT_S17_h/data.RDS')
  data_m <- readRDS('/home/c_thuilliez/Desktop/Work/output/Spatial_PDX/MAP177_PDX_PT_S17_m/data.RDS')
  
  table_h <- cbind(x = GetTissueCoordinates(data_h), y = data_h@reductions$ica@cell.embeddings[,1])
  table_m <- cbind(x = GetTissueCoordinates(data_m), y = data_m@reductions$ica@cell.embeddings[,1])
  
  x = data_h@reductions$ica@cell.embeddings[,1]
  y = data_m@reductions$ica@cell.embeddings[,1]
  
  table_h[which(table_h[,-1:-2] < 0),-1:-2] <- 0
  table_m[which(table_m[,-1:-2] < 0),-1:-2] <- 0
  
  for(i in 1:length(x)){
    if (x[[i]] < 0){
      x[[i]] <- 0
    }
  }
  
  for(i in 1:length(y)){
    if (y[[i]] < 0){
      y[[i]] <- 0
    }
  }
  
  x2 <- x[which(x != 0)]
  y2 <- y[which(x != 0)]
  x2 <- x2[which(y2 != 0)]
  y2 <- y2[which(y2 != 0)]
  
  color = x2/(x2+y2)-y2/(x2+y2)
  
  color <- abs(color)
  
  plotly::plot_ly(type = "scatter", x = as.double(unlist(GetTissueCoordinates(data_h)[names(color),2])),
                  y = as.double(unlist(GetTissueCoordinates(data_h)[names(color),1])),
                  text = x2,
                  meta = y2,
                  marker = list(color = color,showscale = TRUE),
                  hovertemplate = "Humain : %{text}<br>mice: %{meta}<extra></extra>")
                  )
  
  r <- density(data@reductions$ica@cell.embeddings[,1], kernel = c("gaussian"))
  plot(r)
  ###
  
  data_h <- readRDS('/home/c_thuilliez/Desktop/Work/output/Spatial_PDX/MAP177_PDX_PT_S17_h/data.RDS')
  data_m <- readRDS('/home/c_thuilliez/Desktop/Work/output/Spatial_PDX/MAP177_PDX_PT_S17_m/data.RDS')
  
  IC_1 = as.data.frame(data_h@reductions$ica@cell.embeddings[,1])
  IC_2 = as.data.frame(data_m@reductions$ica@cell.embeddings[,1])
  IC_3 = as.data.frame(data_m@reductions$ica@cell.embeddings[,4])
  
  IC_1<-apply(IC_1,2,function(x){x=ifelse(x<=0,0,x); return(x)})
  IC_2<-apply(IC_3,2,function(x){x=ifelse(x<=0,0,x); return(x)})
  IC_2<-apply(IC_2,2,function(x){x=ifelse(x<=0,0,x); return(x)})
  
  
  #We normalize by the sum
  sum_IC_1<-apply(IC_1,2,function(x){x=x/sum(x); return(x)})
  sum_IC_2<-apply(IC_2,2,function(x){x=x/sum(x); return(x)})
  
  sum_IC_1 <- as.double(unlist(sum_IC_1))
  sum_IC_2 <- as.double(unlist(sum_IC_2))
  
  #we calculate the factor of size to reduce pie size where IC are low
  sum_IC_1=(sum_IC_1)/max(sum_IC_1)
  sum_IC_2=(sum_IC_2)/max(sum_IC_2)
  
  # We build the final object
  IC_1<-cbind(data_h@images$slice1@coordinates,IC_1) %>%  cbind(.,sum_IC_1) %>% as.tibble
  IC_2<-cbind(data_h@images$slice1@coordinates,IC_2) %>%  cbind(.,sum_IC_2) %>% as.tibble
  
  c <- IC_1["sum_IC_1"]-IC_2["sum_IC_2"]
  
  IC_All <- IC_1[1:5]
  IC_All$All <- unlist(c)
  IC_All$cell_names <- rownames(data_h@meta.data)
  IC_All$All[which(IC_All$All == Inf | IC_All$All == 0)] <- NaN
  IC_All$All[which(IC_1$sum_IC_1 == Inf | IC_1$sum_IC_1 == 0)] <- NaN
  IC_All$All[which(IC_2$sum_IC_2 == Inf | IC_2$sum_IC_2 == 0)] <- NaN
  
  #We normalize by the sum
  sum <- sum(IC_All$All[!is.nan(IC_All$All)])
  
  for (k in 1:length(IC_All$All)) {
    if (!is.nan(IC_All$All[k])) {
      IC_All$All[k] <- IC_All$All[k]/sum
    }
  }
  
  cells_without_NA <- unlist(IC_All['cell_names'])[!is.nan(IC_All$All)]
  
  View(as.data.table(data_h@assays$Spatial@counts[,cells_without_NA]))
  
  cor()
  ###
  
  plotly::plot_ly(type = "scatter", x = as.double(unlist(IC_All$imagecol)),
                  y = as.double(unlist(IC_All$imagerow)),
                  text = IC_1$sum_IC_1,
                  meta = IC_2$sum_IC_2,
                  marker = list(color = as.double(IC_All$All),showscale = TRUE),
                  hovertemplate = "Humain : %{text}<br>mice: %{meta}<extra></extra>")
  )
  
  ###
  IC <- data_frame(x = unlist(data_h@reductions$ica@cell.embeddings[,1]), y = unlist(data_m@reductions$ica@cell.embeddings[,1]))
  
  weight <- getSpatialNeighbors(IC)
  
  return(data)
}
