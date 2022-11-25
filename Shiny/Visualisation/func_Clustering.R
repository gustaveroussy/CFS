##----------------------------------------------------------------------------##
## Clustering
##----------------------------------------------------------------------------##

Clustering_UMAP <- reactive({
  
  if (is.null(input$Plot_display_IC_choice)) {
    return(values$data)
  }

  data <- values$data
  
  data=Cluster_ICA(adata=data,ICs=as.integer(gsub('[IC_]','',input$Plot_display_IC_choice)),res=input$Plot_resolution)
  
  if ("aneuploid" %in% colnames(data@meta.data)) {
    data@meta.data$aneuploid <- as.character(data@meta.data$aneuploid)
    data@meta.data$aneuploid[which(is.na(data@meta.data$aneuploid))] = "unknown"
    data@meta.data$aneuploid <- as.factor(data@meta.data$aneuploid)
    
    data <- Spatial_pseudotime(data)
  }
  
  return(data)
  
})