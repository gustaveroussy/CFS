IC_interaction=function(data_h = NULL, data_m = NULL){
  
  View(data_h@reductions$ica@cell.embeddings[1])
  
  ###
  
  set.seed(0)
  x1 <- sort(as.double(unlist(GetTissueCoordinates(data_h)["imagerow"])))
  x2 <- sort(as.double(unlist(GetTissueCoordinates(data_h)["imagecol"])))
  mean <- c(0, 0)
  cov <- matrix(c(2, -1, -1, 2), nrow=2)
  f <- function(x1, x2) dmnorm(cbind(x1, x2), mean, cov)
  y <- data_h@reductions$ica@cell.embeddings[,1]
  #create surface plot
  persp(x1, x2, y, theta=-20, phi=20, col = 'blue',
        expand=0.8, ticktype='detailed')
  
  
  
  ###
  
  table_h <- data_h@reductions$ica@cell.embeddings
  table_m <- data_m@reductions$ica@cell.embeddings
  
  colnames(table_h) <- paste0(colnames(table_h),"_h")
  colnames(table_m) <- paste0(colnames(table_m),"_m")
  
  table <- merge(x = table_h, y = table_m, by = 'row.names', all = TRUE)
  
  return(data)
}
