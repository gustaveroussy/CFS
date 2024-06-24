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
      
      data = values$data
      
      incProgress(0.5, detail = paste("Preparing annotation"))
      
      if(input$output_annotation_RDS == TRUE){
        data@misc$annotation <- values$Annotation
      }
      
      if(input$output_markers_RDS == TRUE){
        data@misc$markers <- values$marker_gene
      }
      
      if(input$output_distance_RDS == TRUE){
        data@misc$distances <- values$distances
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
    if (input$export_sub_IC == "IC annotation"){
      
      data <- values$data
      threshold = input$Cell_type_subclustering_density_export_choose
      Cell_type = input$Cell_type_subclustering_IC_export_choose
      
      if(length(Cell_type) > 1){
        for (n in 1:length(Cell_type)) {
          if(n == 1) {
            type = values$annotation_for_output[[input$Annotation_type_subclustering_export_choose]][[Cell_type[n]]]
          } else {
            type = append(type, values$annotation_for_output[[input$Annotation_type_subclustering_export_choose]][[Cell_type[n]]])
          }
        }
        type = unique(type)
      } else if (length(Cell_type) == 1) {
        type = values$annotation_for_output[[input$Annotation_type_subclustering_export_choose]][[Cell_type]]
      } else {
        type = NULL
      }
      
      type = type[type %in% rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"]]
      
      type = unique(c(type,input$Cell_type_subclustering_IC_def_export_choose))
      
      if(is.null(type)){
        shinyalert("Oops!", "Enter a cell type or IC to extract", type = "error")
      }
      
      if (length(type) > 1){
        ic_types=data@reductions$ica@cell.embeddings[,type]
        ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
        sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
        sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
        sum_IC = GetTissueCoordinates(data) %>%  cbind(.,sum_IC)
        identity = rownames(sum_IC)[which(sum_IC[,'sum_IC'] >= mean(sum_IC[,'sum_IC']) + sd(sum_IC[,'sum_IC'])*input$Cell_type_subclustering_density_export_choose)]
      } else {
        identity = rownames(data@reductions$ica@cell.embeddings)[which(data@reductions$ica@cell.embeddings[,type] >= mean(data@reductions$ica@cell.embeddings[,type]) + sd(data@reductions$ica@cell.embeddings[,type])*input$Cell_type_subclustering_density_export_choose)]
      }
      
      data@misc$annotation <- values$Annotation
      
      data@misc$markers <- values$marker_gene
      
      data <- subset(
        x = data,
        cells = identity
      )
      
      saveRDS(data, file)
      
    } else if (input$export_sub_IC == "Metadata"){
      
      data <- values$data
      
      # if(input$output_UMAP_RDS == TRUE && !is.null(values$UMAP)){
      #   data@meta.data <- values$UMAP@meta.data
      #   data@active.ident <- values$UMAP@active.ident
      #   data@graphs <- values$UMAP@graphs
      #   data@reductions$umap <- values$UMAP@reductions$umap
      #   data@commands <- values$UMAP@commands
      # }
      
      data@misc$annotation <- values$Annotation
      
      data@misc$markers <- values$marker_gene
      
      data <- subset(
        x = data,
        cells = rownames(values$data@meta.data[values$data@meta.data[,input$subclustering_metadata_export_choose] %in% input$subclustering_cluster_export_choose,])
      )
      
      saveRDS(data, file)
      
    } else if (input$export_sub_IC == "Manual Selection"){
      
      withProgress(message = 'Preparing RDS', value = 0, {
      
        incProgress(0.25, detail = paste("Preparing base data"))
        data <- values$data
        
        incProgress(0.25, detail = paste("Preparing annotation"))
        
        data@misc$annotation <- values$Annotation
  
        data@misc$markers <- values$marker_gene
  
        data@misc$distances <- values$distances
        
        table = plotly::event_data(c("plotly_selected"), source = "C")
        
        incProgress(0.25, detail = paste("subsetting"))
        
        data <- subset(
          x = data,
          cells = table$customdata
        )
        
        incProgress(0.20, detail = paste("Saving RDS"))
        
        saveRDS(data, file)
        
        incProgress(0.05, detail = paste("Done"))
        
        data = values$data
        
      })
    }
  }
)