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
      
      if(input$output_UMAP_RDS == TRUE && !is.null(values$UMAP)){
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
      
      if(input$output_markers_RDS == TRUE){
        data@misc$markers <- values$marker_gene
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
        
        data <- Launch_analysis()
        
        threshold = input$Cell_type_subclustering_density_export_choose
        Cell_type = input$Cell_type_subclustering_IC_export_choose
        
        if(length(Cell_type) > 1){
          for (n in 1:length(Cell_type)) {
            if(n == 1) {
              type = values$annotation_for_output[[n]]
            } else {
              type = append(type, values$annotation_for_output[[n]])
            }
          }
          type = unique(type)
        } else if (length(Cell_type) == 1) {
          type = values$annotation_for_output[[Cell_type]]
        } else {
          shinyalert("Oops!", "Enter a cell type to sub.", type = "error")
        }
        
        if (length(type) >= 1){
          ic_types=data@reductions$ica@cell.embeddings[,type]
          ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
          sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
          sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
          sum_IC = GetTissueCoordinates(data) %>%  cbind(.,sum_IC)
          identity = rownames(sum_IC)[which(sum_IC[,'sum_IC'] >= input$Cell_type_subclustering_density_export_choose)]
        } else {
          identity = rownames(data@reductions$ica@cell.embeddings)
        }
        
        if(input$output_UMAP_RDS == TRUE && !is.null(values$UMAP)){
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
          cells = identity
        )
        
        saveRDS(data, file)
        
      }
      
    } else if (input$export_sub_IC == "UMAP Cluster"){
      
      data <- Launch_analysis()
      
      if(input$output_UMAP_RDS == TRUE && !is.null(values$UMAP)){
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