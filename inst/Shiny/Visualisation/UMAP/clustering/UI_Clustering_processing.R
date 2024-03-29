##----------------------------------------------------------------------------##
## Get clustering when called
##----------------------------------------------------------------------------##
# data <- FindNeighbors(data, reduction = "ica",dims = ICs)
# data <- FindClusters(data, verbose = FALSE,resolution=res)

marker_table <- observeEvent(input$start_cluster_plot, {
  req(values$data)
  
  type = NULL
  
  withProgress(message = 'Pre-processing', value = 0, {
  
    if (!is.null(input$Plot_cluster_IC_choice)) {
      if (!is.null(input$Plot_cluster_type_UMAP_choice)){
        
        incProgress(0.1, detail = "Determining IC associated types")
        
        if(length(input$Plot_cluster_type_UMAP_choice) != 1){
          for (n_cell_type in input$Plot_cluster_type_UMAP_choice) {
            if(is.null(type)) {
              type = values$annotation_for_output[[n_cell_type]]
            } else {
              type = append(type, values$annotation_for_output[[n_cell_type]])
            }
          }
          type = unique(type)
        } else {
          type = values$annotation_for_output[[input$Plot_cluster_type_UMAP_choice]]
        }
        
        type = type[type %in% rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"]]
        
        incProgress(0.4, detail = "Finding neighbors")
        values$data = FindNeighbors(values$data, reduction = "ica",dims = as.integer(gsub('[IC_]','',unique(c(type,input$Plot_cluster_IC_choice)))))
        incProgress(0.4, detail = "Finding clusters")
        values$data = FindClusters(values$data, verbose = FALSE, resolution=input$Clustering_resolution, method = "igraph", algorithm = as.integer(input$select_algorithm_clusterisation), cluster.name = input$cluster_named_by_user)
        incProgress(0.1, detail = "Done")
        values$data@meta.data[,input$cluster_named_by_user] = as.factor(values$data@meta.data[,input$cluster_named_by_user])
      } else {
        incProgress(0.5, detail = "Finding neighbors")
        values$data = FindNeighbors(values$data, reduction = "ica",dims = as.integer(gsub('[IC_]','',unique(c(type,input$Plot_cluster_IC_choice)))))
        incProgress(0.4, detail = "Finding clusters")
        values$data = FindClusters(values$data, verbose = FALSE, resolution=input$Clustering_resolution, method = "igraph", algorithm = as.integer(input$select_algorithm_clusterisation), cluster.name = input$cluster_named_by_user)
        incProgress(0.1, detail = "Done")
        values$data@meta.data[,input$cluster_named_by_user] = as.factor(values$data@meta.data[,input$cluster_named_by_user])
      }
      
      
    } else if (!is.null(input$Plot_cluster_type_UMAP_choice)){
      
      incProgress(0.1, detail = "Determining IC associated types")
      
      if(length(input$Plot_cluster_type_UMAP_choice) != 1){
        name = paste(input$Plot_cluster_type_UMAP_choice,collapse = ",")
        for (n_cell_type in input$Plot_cluster_type_UMAP_choice) {
          if(is.null(type)) {
            type = values$annotation_for_output[[n_cell_type]]
          } else {
            type = append(type, values$annotation_for_output[[n_cell_type]])
          }
        }
        type = unique(type)
      } else {
        type = values$annotation_for_output[[input$Plot_cluster_type_UMAP_choice]]
      }
      
      type = type[type %in% rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"]]
      
      incProgress(0.4, detail = "Finding neighbors")
      values$data = FindNeighbors(values$data, reduction = "ica",dims = as.integer(gsub('[IC_]','',type)))
      incProgress(0.4, detail = "Finding clusters")
      values$data = FindClusters(values$data, verbose = FALSE, resolution=input$Clustering_resolution, method = "igraph", algorithm = as.integer(input$select_algorithm_clusterisation), cluster.name = input$cluster_named_by_user)
      incProgress(0.1, detail = "Done")
      
      values$data@meta.data[,input$cluster_named_by_user] = as.factor(values$data@meta.data[,input$cluster_named_by_user])
      
    } else {
      shinyalert("Clustering error", "No clustering can be calculated", type = "error")
    }
  })
})

