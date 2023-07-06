##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##
# make Annotation table
output[["Marker_table_or_message"]] <- renderDT({
  
  req(values$UMAP)
  
  if(is.null(values$marker_gene)){
    values$marker_gene = marker_table()
  }
  
  req(values$marker_gene)
  
  DT = Prepare_table_marker(table = values$marker_gene[[(as.integer(input$marker_cluster_choice)+1)]], log = input$Volcano_plot_log_fold_change, pvalue = input$Volcano_plot_p_value)
  
  datatable(DT, options = list(pageLength = 100), class = 'cell-border stripe')#, colnames = c('IC' = 1))
})

# loading time/button
marker_table <- eventReactive(input$start_marker, {
  req(values$UMAP)
  list_marker = list()
  withProgress(message = 'Preparing table', value = 0, {
    if(is.null(values$marker_gene)){
      if (!is.null(values$UMAP)){
        for (i in 1:length(unique(values$UMAP@meta.data$seurat_clusters))){
          if(length(rownames(values$UMAP@meta.data)[which(values$UMAP@meta.data$seurat_clusters == (i-1))]) > 3){
            incProgress((1/as.integer(length(unique(values$UMAP@meta.data$seurat_clusters)))), detail = paste(paste0("Working on cluster ",(i-1))))
            list_marker[[i]] = FindMarkers(values$UMAP, ident.1 = rownames(values$UMAP@meta.data)[which(values$UMAP@meta.data$seurat_clusters == (i-1))], logfc.threshold = 0.25)
          }
        }
      }
    } else {
      incProgress(1, detail = paste("Done"))
      list_marker = values$marker_gene
    }
  })
  return(list_marker)
})