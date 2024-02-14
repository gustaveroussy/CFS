##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##
# make Annotation table
output[["Marker_table_or_message"]] <- renderDT({
  
  req(values$UMAP)
  req(values$marker_gene)
  
  DT = Prepare_table_marker(table = values$marker_gene[[input$marker_cluster_choice]], log = input$Volcano_plot_log_fold_change, pvalue = input$Volcano_plot_p_value)
  
  datatable(DT, options = list(pageLength = 100), class = 'cell-border stripe')#, colnames = c('IC' = 1))
})

# loading time/button
marker_table <- observeEvent(input$start_marker, {
  
  #check that the clustering has been done.
  req(values$UMAP)
  
  withProgress(message = 'Preparing to find markers', value = 0, {
  
    if(length(values$UMAP@images) > 1){
      # prepare differential analysis for integrated spatial
      if(slot(object = values$UMAP@assays$SCT@SCTModel.list[[2]], name="umi.assay") != "Spatial"){
        for(i in 1:length(values$UMAP@images)){
          slot(object = values$UMAP@assays$SCT@SCTModel.list[[i]], name="umi.assay") = "Spatial"
        }
      }
      
      values$UMAP = PrepSCTFindMarkers(values$UMAP, assay = "SCT", verbose = FALSE)
    }
  
    incProgress(0.1, detail = "Preparing table")
  
    if (!is.null(values$UMAP)){
      if(is.null(values$marker_gene)){
        values$marker_gene = list()
      }
      for (i in 1:length(input$volcano_plot_clusters_to_compare)){
        if(nrow(values$UMAP@meta.data[values$UMAP@meta.data$seurat_clusters == (input$volcano_plot_clusters_to_compare[i]),]) > 3){
          incProgress((0.9/length(input$volcano_plot_clusters_to_compare)), detail = paste(paste0("Working on cluster ",(input$volcano_plot_clusters_to_compare[i]))))
          values$marker_gene[[paste0(input$volcano_plot_clusters_to_compare[i],"/", paste(input$volcano_plot_clusters_to_compare[which(input$volcano_plot_clusters_to_compare != input$volcano_plot_clusters_to_compare[i])],collapse=","))]] =
            FindMarkers(values$UMAP,
                        ident.1 = rownames(values$UMAP@meta.data)[which(values$UMAP@meta.data$seurat_clusters == (input$volcano_plot_clusters_to_compare[i]))],
                        ident.2 = rownames(values$UMAP@meta.data)[which(values$UMAP@meta.data$seurat_clusters %in% (input$volcano_plot_clusters_to_compare[which(input$volcano_plot_clusters_to_compare != input$volcano_plot_clusters_to_compare[i])]))],
                        logfc.threshold = 0.25)
        }
      }
    }
  })
})