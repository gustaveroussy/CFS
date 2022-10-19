########################################
# reactive of the image to plot by plotly
########################################

palette <- reactive({
  palette = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')
  return(palette)
})

##----------------------------------------------------------------------------##
## Clustering
##----------------------------------------------------------------------------##

Clustering_UMAP <- reactive({
  
  data <- Launch_analysis()
  
  data=Cluster_ICA(adata=data,ICs=as.integer(gsub('[IC_]','',input$gene_projection_gene_choice)),res=input$Plot_resolution)
  if ("aneuploid" %in% colnames(data@meta.data$aneuploid)) {
    data@meta.data$aneuploid <- as.character(data@meta.data$aneuploid)
    data@meta.data$aneuploid[which(is.na(data@meta.data$aneuploid))] = "unknown"
    data@meta.data$aneuploid <- as.factor(data@meta.data$aneuploid)
  }

  
  return(data)
  
})

##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

current_plot_umap <- reactive({
  data <- Clustering_UMAP()
  
  fig <- plot_ly(type = 'scatter',
                 mode='markers'
  )
  
  if (input$Plot_analysis_type == "UMAP"){
    if (input$Plot_display_type == "Clustering"){
      for (i in 0:length(summary(data@meta.data[["seurat_clusters"]]))-1){
        fig <- fig %>%
          add_trace(
            x = data[["umap"]]@cell.embeddings[which(data@meta.data[["seurat_clusters"]]==i),1],
            y = data[["umap"]]@cell.embeddings[which(data@meta.data[["seurat_clusters"]]==i),2],
            marker = list(
              color = palette()[i+1],
              size = 10
            ),
            showlegend = T
          )
      }
    } else if (input$Plot_display_type == "Ploïdie"){
      c = 1
      for (i in unique(data@meta.data[["aneuploid"]])){
        fig <- fig %>%
          add_trace(
            x = data[["umap"]]@cell.embeddings[which(data@meta.data[["aneuploid"]]==i),1],
            y = data[["umap"]]@cell.embeddings[which(data@meta.data[["aneuploid"]]==i),2],
            name = i,
            marker = list(
              color = palette()[c],
              size = 10
            ),
            showlegend = T
          )
        c = c+1
        }
      }
    }
  return(fig)
})

plots <- reactiveValues(button_check = 1, umap = NULL, spatial = NULL)

observeEvent(input$start_plot, {
  if (input$start_plot == plots$button_check) {
    plots$umap = current_plot_umap()
    plots$spatial = current_plot_spatial()
    plots$button_check <- input$start_plot + 1
  }
})

output[["Plot"]] <- plotly::renderPlotly({
  plots$umap
})

##----------------------------------------------------------------------------##
## Spatial clustering
##----------------------------------------------------------------------------##

current_plot_spatial <- reactive({
  data <- Clustering_UMAP()
  
  fig <- plot_ly(type = 'scatter',
                 mode='markers'
  )
  
  fig <- fig %>% add_trace(type="image", source = raster2uri(raster::as.raster(data@images$slice1@image)), hoverinfo = "skip")
  
  if (input$Plot_analysis_type == "UMAP"){
    if (input$Plot_display_type == "Clustering"){
      for (i in 0:length(summary(data@meta.data[["seurat_clusters"]]))-1){
        fig <- fig %>%
          add_trace(
            x = TissueCoordinates()[,"imagecol"][which(data@meta.data[["seurat_clusters"]]==i)],
            y = TissueCoordinates()[,"imagerow"][which(data@meta.data[["seurat_clusters"]]==i)],
            marker = list(
              color = palette()[i+1],
              size = 10
            ),
            showlegend = T,
            customdata = rownames(data@meta.data)[which(data@meta.data[["seurat_clusters"]]==i)],
            hovertemplate = paste("Cell : %{customdata}<br>",
                                  "<extra></extra>")
          )
    } 
  } else if (input$Plot_display_type == "Ploïdie"){
      c = 1
      for (i in unique(data@meta.data[["aneuploid"]])){
        fig <- fig %>%
          add_trace(
            x = TissueCoordinates()[,"imagecol"][which(data@meta.data[["aneuploid"]]==i)],
            y = TissueCoordinates()[,"imagerow"][which(data@meta.data[["aneuploid"]]==i)],
            name = i,
            marker = list(
              color = palette()[c],
              size = 10
            ),
            showlegend = T,
            text = data@meta.data[["aneuploid"]],
            customdata = rownames(data@meta.data)[which(data@meta.data[["aneuploid"]]==i)],
            hovertemplate = paste("Cell : %{customdata}<br>",
                                  "Ploïdie : %{text}<br>",
                                  "<extra></extra>")
          )
        c = c + 1
      }
    }
  }
  
  fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                 yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                 autosize = TRUE
  )
  
  return(fig)
})

output[["Plot_Spatial"]] <- plotly::renderPlotly({
  plots$spatial
})

##----------------------------------------------------------------------------##
## Handler for the download for RDS
##----------------------------------------------------------------------------##

output$download_RDS <- downloadHandler(
  filename = function() {
    paste("data", ".RDS", sep = "")
  },
  content = function(file) {
    saveRDS(Clustering_UMAP(), file)
  }
)
