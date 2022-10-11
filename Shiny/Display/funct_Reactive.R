########################################
# reactive of the image to plot by plotly
########################################

palette <- reactive({
  palette = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')
  return(palette)
})

##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

current_plot_umap <- reactive({
  data <- Launch_analysis()
  
  data=Cluster_ICA(adata=data,ICs=as.integer(gsub('[IC_]','',input$gene_projection_gene_choice)),res=input$Plot_resolution)
  
  fig <- plot_ly(type = 'scatter',
                 mode='markers'
  )
  if (input$Plot_analysis_type == "UMAP"){
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
  }
  
  return(fig)
})

plots <- reactiveValues(button_check = 1, umap = NULL)

observeEvent(input$start_plot, {
  if (input$start_plot == plots$button_check) {
    plots$umap = current_plot_umap()
    plots$button_check <- input$start_plot + 1
  }
})

output[["Plot"]] <- plotly::renderPlotly({
  plots$umap
})