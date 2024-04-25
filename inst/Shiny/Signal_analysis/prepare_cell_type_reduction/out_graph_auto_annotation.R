##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["output_graph_auto_annotation"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("cell_graph_auto_annotation_plot",
                         width = "auto",
                         height = "85vh"),
    actionButton("start_add_annotation", "Add annotation"),
    textInput("new_col_name_annotation", "Name", value = "New_column", width = NULL,
              placeholder = NULL)
  )
})

output[["cell_graph_auto_annotation_plot"]] <- plotly::renderPlotly({
  return(figure_and_tree_cutoff_annotation()$figure)
})

select_tree_create_annotation = reactive({
  x = plotly::event_data(c("plotly_click"), source = "I")
  return(x)
})

figure_and_tree_cutoff_annotation = reactive({
  req(values$data)
  req(values$data@reductions$ica)
  
  dist.mat <- dist(t(values$data@reductions$ica@feature.loadings), method = "euclidean")
  true.dist.mat <- as.dist(dist.mat)
  clust.res <- hclust(true.dist.mat, method = "ward.D")
  
  p <- ggdendrogram(clust.res, rotate = FALSE, size = 2)
  
  p = ggplotly(p, source = "I")
  
  if(!is.null(select_tree_create_annotation())){
    l_row = as.double(select_tree_create_annotation()["y"])
    
    p[["x"]][["layout"]][["shapes"]][[2]] = list(type = "line",
                                                 line = list(color="Red", width=3, dash="dashdot"), opacity = 1,
                                                 x0 = 0,
                                                 x1 = 1, xref = "paper",
                                                 y0 = l_row, y1 = l_row, yref = "y")
    
    Ks = sum(clust.res[["height"]] > l_row) + 1
    cut = cutree(clust.res,Ks)
    
  }
  
  return(list(figure = p, clusters = cut))
})

observeEvent(input$start_add_annotation,{
  values$Annotation = cbind(values$Annotation,figure_and_tree_cutoff_annotation()$clusters)
  colnames(values$Annotation)[ncol(values$Annotation)] = input$new_col_name_annotation
})


