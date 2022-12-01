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

# search for the cells that were selected while in density
selected_cells_subcluster <- reactive({
  return(plotly::event_data(c("plotly_selected"), source = "B"))
})

output$download_subcluster_RDS <- downloadHandler(
  filename = function() {
    paste("data", ".RDS", sep = "")
  },
  content = function(file) {
    if (input$export_sub_IC == "IC Cell types"){
      
      data <- Launch_analysis()
      threshold = input$Cell_type_subclustering_density_export_choose
      Cell_type = input$Cell_type_subclustering_IC_export_choose
      
      if (input$Plot_display_type_density_manual == "Manual"){
        selected_cells = selected_cells_subcluster()$customdata
        ## if cells were manually selected
        # filter object
        data <- subset(
          x = data,
          cells = selected_cells
        )
        
        if(input$output_annotation_RDS == TRUE){
          data@misc$annotation <- values$Annotation
        }
        
        saveRDS(data, file)
        
      } else if (input$Plot_display_type_density_manual == "Automated"){
        
        shinyalert("Oops!", "This function isn't available yet.", type = "error")
        
      }
      
    } else if (input$export_sub_IC == "UMAP Cluster"){
      
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
  }
)