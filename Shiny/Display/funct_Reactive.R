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
  
  if (is.null(input$gene_projection_gene_choice)) {
    return(Launch_analysis())
  }

  data <- Launch_analysis()
  
  data=Cluster_ICA(adata=data,ICs=as.integer(gsub('[IC_]','',input$gene_projection_gene_choice)),res=input$Plot_resolution)
  
  if ("aneuploid" %in% colnames(data@meta.data)) {
    data@meta.data$aneuploid <- as.character(data@meta.data$aneuploid)
    data@meta.data$aneuploid[which(is.na(data@meta.data$aneuploid))] = "unknown"
    data@meta.data$aneuploid <- as.factor(data@meta.data$aneuploid)
  }

  data <- Spatial_pseudotime(data)
  
  return(data)
  
})

##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

current_plot_umap <- reactive({
  
  req(Launch_analysis()@reductions[["umap"]])
  data <- Clustering_UMAP()
  
  
  fig <- plot_ly(type = 'scatter',
                 mode='markers',
                 source = "A"
  )
  
  if (input$Plot_analysis_type == "UMAP"){
    if (input$Plot_display_type == "Clustering"){
      for (i in 0:length(summary(data@meta.data[["seurat_clusters"]]))-1){
        fig <- fig %>%
          add_trace(
            x = data[["umap"]]@cell.embeddings[which(data@meta.data[["seurat_clusters"]]==i),1],
            y = data[["umap"]]@cell.embeddings[which(data@meta.data[["seurat_clusters"]]==i),2],
            name = i,
            marker = list(
              color = palette()[i+1],
              size = 10
            ),
            showlegend = T,
            text = i,
            customdata = rownames(data@meta.data)[which(data@meta.data[["seurat_clusters"]]==i)],
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Cluster : %{text}",
                                   "<extra></extra>")
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
            showlegend = T,
            text = i,
            customdata = rownames(data@meta.data)[which(data@meta.data[["aneuploid"]]==i)],
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                   "Cluster : %{text}",
                                   "<extra></extra>")
          )
        c = c+1
        }
      }
    
    fig <- fig %>% event_register('plotly_selected')
    
    } else if (input$Plot_analysis_type == "sub UMAP") { # En chantier : voir si on fait ça avec un traitement additionnel ou directement dans shiny
      
      data <- sub_UMAP_plot()
      
      table <- data[["umap"]]@cell.embeddings
      table <- table[(row.names(table) %in% selected_cells_plot()$customdata),]
      
      metadata <- data@meta.data
      metadata <- metadata[(row.names(metadata) %in% selected_cells_plot()$customdata),]
      
      if (input$Plot_display_type == "Clustering"){
        for (i in 0:length(summary(metadata[["seurat_clusters"]]))-1){
          fig <- fig %>%
            add_trace(
              x = table[which(metadata[["seurat_clusters"]]==i),1],
              y = table[which(metadata[["seurat_clusters"]]==i),2],
              name = i,
              marker = list(
                color = palette()[i+1],
                size = 10
              ),
              showlegend = T,
              text = i,
              customdata = rownames(metadata)[which(metadata[["seurat_clusters"]]==i)],
              hovertemplate = paste0("Cell : %{customdata}<br>",
                                     "Cluster : %{text}",
                                     "<extra></extra>")
            )
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
  
  req(Launch_analysis()@reductions[["umap"]])
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
            name = i,
            marker = list(
              color = palette()[i+1],
              size = 10
            ),
            showlegend = T,
            text = i,
            customdata = rownames(data@meta.data)[which(data@meta.data[["seurat_clusters"]]==i)],
            hovertemplate = paste0("Cell : %{customdata}<br>",
                                  "Cluster : %{text}",
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
              color = palette()[sub_UMAP_plotc],
              size = 10
            ),
            showlegend = T,
            text = data@meta.data[["aneuploid"]][which(data@meta.data[["aneuploid"]]==i)],
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

##----------------------------------------------------------------------------##
## trajectory
##----------------------------------------------------------------------------##

current_plot_trajectory <- reactive({

  req(Launch_analysis()@reductions[["umap"]])
  data <- Clustering_UMAP()
  
  data <- Spatial_pseudotime(data,input$gene_projection_gene_choice)
  
  if (input$trajectory_dimension_type == "2D"){
    
    fig <- plot_ly(type = 'scatter',
                   mode='markers'
    )
    
    if (input$trajectory_color_by == "Clustering"){
      for (i in 0:length(summary(data@meta.data[["seurat_clusters"]]))-1){
        
        fig <- fig %>% add_trace(
            x = data@misc$dpt@dm@eigenvectors[which(data$seurat_clusters[which(data@meta.data$aneuploid == "aneuploid")] == i),c(1)],
            y = data@misc$dpt@dm@eigenvectors[which(data$seurat_clusters[which(data@meta.data$aneuploid == "aneuploid")] == i),c(2)],
            name = i,
            marker = list(
              color = palette()[i+1],
              size = 10
            ),
            showlegend = T,
            hovertemplate = paste0("DC1 : %{x}<br>",
                                   "DC2 : %{y}",
                                   "<extra></extra>")
          )
      }
    } else {
    }
  } else if (input$trajectory_dimension_type == "3D") {
    
    fig <- plot_ly(type = 'scatter3d', mode = "markers")
    
    if (input$trajectory_color_by == "Clustering"){
      for (i in 0:length(summary(data@meta.data[["seurat_clusters"]]))-1){
        
        fig <- fig %>% add_trace(
          x = data@misc$dpt@dm@eigenvectors[which(data$seurat_clusters[which(data@meta.data$aneuploid == "aneuploid")] == i),c(1)],
          y = data@misc$dpt@dm@eigenvectors[which(data$seurat_clusters[which(data@meta.data$aneuploid == "aneuploid")] == i),c(2)],
          z = data@misc$dpt@dm@eigenvectors[which(data$seurat_clusters[which(data@meta.data$aneuploid == "aneuploid")] == i),c(3)],
          name = i,
          marker = list(
            color = palette()[i+1],
            size = 2
          ),
          showlegend = T,
          hovertemplate = paste0("DC1 : %{x}<br>",
                                "DC2 : %{y}<br>",
                                "DC3 : %{z}",
                                "<extra></extra>")
        )
      }
      
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'DC1'), yaxis = list(title = 'DC2'), zaxis = list(title = 'DC3')))
      
    } else {
    }
  }
  return(fig)
})

current_plot_spatial_trajectory <- reactive({
  
  req(Launch_analysis()@reductions[["umap"]])
  data <- Clustering_UMAP()
  
  data <- Spatial_pseudotime(data,input$gene_projection_gene_choice)
  
  DC = input$trajectory_dimension_choice
  
  fig <- plot_ly()
  
  fig <- fig %>% add_trace(type="image", source = raster2uri(raster::as.raster(data@images$slice1@image)), hoverinfo = 'skip')
  
  if (input$trajectory_dimension_choice == "dm") {
    fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                             x = TissueCoordinates()[which(data@meta.data$aneuploid == "aneuploid"),"imagecol"],
                             y = TissueCoordinates()[which(data@meta.data$aneuploid == "aneuploid"),"imagerow"],
                             marker = list(color = data@meta.data$dm[which(data@meta.data$aneuploid == "aneuploid")],
                                           colorscale = "viridis"),
                             text = data@meta.data$dm[which(data@meta.data$aneuploid == "aneuploid")],
                             customdata = rownames(TissueCoordinates()[which(data@meta.data$aneuploid == "aneuploid"),]),
                             hovertemplate = paste("Cell : %{customdata}<br>",
                                                   "Expression: %{text}",
                                                   "<extra></extra>")
    )
  } else {
    fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                             x = TissueCoordinates()[which(data@meta.data$aneuploid == "aneuploid"),"imagecol"],
                             y = TissueCoordinates()[which(data@meta.data$aneuploid == "aneuploid"),"imagerow"],
                             marker = list(color = data@misc$dpt[[DC]],
                                           colorscale = "viridis"),
                             text = data@misc$dpt[[DC]],
                             customdata = rownames(TissueCoordinates()[which(data@meta.data$aneuploid == "aneuploid"),]),
                             hovertemplate = paste("Cell : %{customdata}<br>",
                                                   "Expression: %{text}",
                                                   "<extra></extra>")
    )
  }
  

  
  
  
  fig <- fig %>% layout(title = input$trajectory_dimension_choice, xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                        yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                        showlegend = FALSE)
  
  return(fig)
})

trajectory_plots <- reactiveValues(button_check = 1, trajectory = NULL, spatial_trajectory = NULL)

observeEvent(input$start_plot_trajectory, {
  if (input$start_plot_trajectory == trajectory_plots$button_check) {
    trajectory_plots$trajectory = current_plot_trajectory()
    trajectory_plots$spatial_trajectory = current_plot_spatial_trajectory()
    trajectory_plots$button_check <- input$start_plot_trajectory + 1
  }
})

output[["trajectory"]] <- plotly::renderPlotly({
  trajectory_plots$trajectory
})

output[["trajectory_Spatial"]] <- plotly::renderPlotly({
  trajectory_plots$spatial_trajectory
})

##----------------------------------------------------------------------------##
## sub-clustering
##----------------------------------------------------------------------------##

observeEvent(input$start_plot, {
  updateSelectInput(session, "Plot_analysis_type", label = "Select method to use", 
                    choices = list("UMAP","sub UMAP"), 
                    selected = "UMAP")
})

selected_cells_plot <- reactive({
  return(plotly::event_data(c("plotly_selected"), source = "A"))
})

sub_UMAP_plot <- reactive({
  
  data <- Clustering_UMAP()
  
  data <- data[,(colnames(data@assays$Spatial) %in% selected_cells_plot()$customdata)]
  
  return(data)
})