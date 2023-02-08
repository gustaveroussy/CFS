output[["marker_cluster_choice_UI"]] <- renderUI({
  selectInput(
    "marker_cluster_choice",
    label = "Choose cluster marker gene",
    choices = (as.integer(sort(unique(values$UMAP@meta.data$seurat_clusters)))-1),
    width = '100%'
  )
})