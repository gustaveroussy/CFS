##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##
# make Annotation table
output[["Marker_table_or_message"]] <- renderDT({
  
  req(values$data)
  req(values$marker_gene)
  
  DT = Prepare_table_marker(table = values$marker_gene[[input$marker_cluster_choice]], log = input$Volcano_plot_log_fold_change, pvalue = input$Volcano_plot_p_value)
  
  datatable(DT, options = list(pageLength = 100), class = 'cell-border stripe')#, colnames = c('IC' = 1))
})

# loading time/button
marker_table <- observeEvent(input$start_marker, {
  
  #check that the clustering has been done.
  req(values$data)
  
  withProgress(message = 'Preparing to find markers', value = 0, {
  
    if(length(values$data@assays$SCT@SCTModel.list) > 1){
      # prepare differential analysis for integrated spatial
      if(slot(object = values$data@assays$SCT@SCTModel.list[[2]], name="umi.assay") != "Spatial"){
        for(i in 1:length(values$data@images)){
          slot(object = values$data@assays$SCT@SCTModel.list[[i]], name="umi.assay") = "Spatial"
        }
      }
      
      # check if PrepSCT needs to be applied
      apply_SCTprep = FALSE
      old_value = slot(values$data@assays$SCT@SCTModel.list[[1]],"median_umi")

      for(i in 2:length(values$data@assays$SCT@SCTModel.list)){
        if(old_value != slot(values$data@assays$SCT@SCTModel.list[[i]],"median_umi")){
          apply_SCTprep = TRUE
        }
      }
      
      if(apply_SCTprep){
        values$data = PrepSCTFindMarkers(values$data, assay = "SCT", verbose = FALSE)
      }
    }

    incProgress(0.1, detail = "Preparing table")
  
    if (!is.null(values$data)){
      if(is.null(values$marker_gene)){
        values$marker_gene = list()
      }
      
      
      
      if(length(input$volcano_plot_clusters_to_compare) == 1){
        
        selected_cluster = input$volcano_plot_clusters_to_compare
        other_clusters = unique(values$data@meta.data[,input$volcano_plot_clusters_list_to_compare])[!(unique(values$data@meta.data[,input$volcano_plot_clusters_list_to_compare]) %in% selected_cluster)]
        other_clusters = as.numeric(as.character(other_clusters[order(other_clusters)]))
        

        if(nrow(values$data@meta.data[values$data@meta.data[,input$volcano_plot_clusters_list_to_compare] == (selected_cluster),]) > 3){
          
          incProgress(0.9, detail = paste(paste0("Working on cluster ",selected_cluster)))
          
          values$marker_gene[[paste0(input$volcano_plot_clusters_list_to_compare," : ",selected_cluster,"/", paste(other_clusters,collapse=","))]] =
            FindMarkers(values$data,
                        ident.1 = rownames(values$data@meta.data)[which(values$data@meta.data[,input$volcano_plot_clusters_list_to_compare] == (selected_cluster))],
                        ident.2 = rownames(values$data@meta.data)[which(values$data@meta.data[,input$volcano_plot_clusters_list_to_compare] %in% (other_clusters))],
                        logfc.threshold = 0.25,
                        test.use = input$clustering_method_volcano_plot)
        }

      } else if (length(input$volcano_plot_clusters_to_compare) > 1) {
        
        selected_clusters = input$volcano_plot_clusters_to_compare
        
        for (i in selected_clusters){
          if(nrow(values$data@meta.data[values$data@meta.data[,input$volcano_plot_clusters_list_to_compare] == i,]) > 3){
            
            # incProgress((0.9/length(selected_clusters)),
            #             detail = paste(paste0("Working on cluster ",i)))
            
            values$marker_gene[[paste0(input$volcano_plot_clusters_list_to_compare," : ",i,"/", paste(selected_clusters[which(selected_clusters != i)],collapse=","))]] =
              FindMarkers(values$data,
                          ident.1 = rownames(values$data@meta.data)[which(values$data@meta.data[,input$volcano_plot_clusters_list_to_compare] == i)],
                          ident.2 = rownames(values$data@meta.data)[which(values$data@meta.data[,input$volcano_plot_clusters_list_to_compare] %in% (selected_clusters[which(selected_clusters != i)]))],
                          logfc.threshold = 0.25,
                          test.use = input$clustering_method_volcano_plot)
          }
        }
      }
    }
  })
})