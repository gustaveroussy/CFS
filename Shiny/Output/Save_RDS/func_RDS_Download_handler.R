##----------------------------------------------------------------------------##
## Handler for the download for RDS
##----------------------------------------------------------------------------##

output$download_RDS <- downloadHandler(
  filename = function() {
    paste("data", ".RDS", sep = "")
  },
  content = function(file) {
    withProgress(message = 'Preparing RDS', value = 0, {
      
      incProgress(0.25, detail = paste("Preparing base data"))
        
      data <- Launch_analysis()
      
      incProgress(0.25, detail = paste("Preparing UMAP"))
      
      if(input$output_UMAP_RDS == TRUE){
        data@meta.data <- values$UMAP@meta.data
        data@active.ident <- values$UMAP@active.ident
        data@graphs <- values$UMAP@graphs
        data@reductions$umap <- values$UMAP@reductions$umap
        data@commands <- values$UMAP@commands
      }
      
      incProgress(0.25, detail = paste("Preparing annotation"))
      
      if(input$output_annotation_RDS == TRUE){
        data@misc$annotation <- values$Annotation
      }
      
      incProgress(0.20, detail = paste("Saving RDS"))
      saveRDS(data, file)
      incProgress(0.05, detail = paste("Done"))
    })
  }
)

output$download_subcluster_RDS <- downloadHandler(
  filename = function() {
    paste("data", ".RDS", sep = "")
  },
  content = function(file) {

      data <- Launch_analysis()
      
      if(input$output_UMAP_RDS == TRUE){
        data@meta.data <- values$UMAP@meta.data
        data@active.ident <- values$UMAP@active.ident
        data@graphs <- values$UMAP@graphs
        data@reductions$umap <- values$UMAP@reductions$umap
        data@commands <- values$UMAP@commands
      }
      
      if(input$output_annotation_RDS == TRUE){
        data@misc$annotation <- values$Annotation
      }
      
      data <- subset(
        x = data,
        idents = input$subclustering_cluster_export_choose
      )
      
      saveRDS(data, file)
  }
)