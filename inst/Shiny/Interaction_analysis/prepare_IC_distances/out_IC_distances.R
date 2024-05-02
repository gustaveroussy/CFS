##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["IC_distances_plot"]] <- plotly::renderPlotly({
  req(values$distances[[input$choose_sample_for_distances]][[input$choose_method_for_distances]])
  
  return(fig_distance_graph_IC())
})

output[["IC_distances_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("IC_distances_plot",
                         width = "auto",
                         height = "85vh")
  )
})

observeEvent(input$start_distance_IC,{
  req(values$data)
  req(values$data@reductions$ica)
  
  data = values$data
  method = input$choose_method_for_distances
  sample = input$choose_sample_for_distances
  
  lee_function = function(x){
    n = lee(table_sample[,x[1]], table_sample[,x[2]], listw, nrow(table_sample), zero.policy=attr(listw, "zero.policy"))
    return(n$L)
  }
  
  withProgress(message = 'Calculating distances', value = 0, {
  
    if(method == "Lee"){
      table = data@reductions$ica@cell.embeddings
      
      table_sample = table[grepl(paste0(sample,"_[ACGT]"), rownames(table)),]
      
      incProgress(0.1, detail = "Finding neighbors")
      
      knn = knearneigh(GetTissueCoordinates(data, sample), k=6, longlat = NULL, use_kd_tree=TRUE)
      neighbours = knn2nb(knn, row.names = NULL, sym = FALSE)
      listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)
      
      incProgress(0.2, detail = "Calculating Lee")
      
      df = t(combn(colnames(table_sample),2))
      df = as.data.frame(df)
      x = apply(df,1,lee_function)
      
      incProgress(0.7, detail = "Finished")
      
      df[,"Lee"] = x
      
      df[,"Lee"] = scale(as.double(df[,"Lee"]))
      
      df = df[!(as.double(df[,"Lee"]) <= 0),]
      
      df = df[!(as.double(df[,"Lee"]) <= sd(as.double(df[,"Lee"]))),]
      
      values$distances[[sample]][[method]] = df
      
    }
    
  })
  
})


fig_distance_graph_IC <- reactive({
  tree_table = values$distances[[input$choose_sample_for_distances]][[input$choose_method_for_distances]]
  
  G = graph_from_data_frame(tree_table, directed = FALSE)
  
  layout <- layout_with_fr(G)
  
  # create nodes
  Xn <- layout[,1]
  Yn <- layout[,2]
  
  network <- plot_ly(type = "scatter", x = ~Xn, y = ~Yn, mode = "markers",
                     text = names(V(G)), hoverinfo = "text",
                     color = values$Annotation[names(V(G)),"Type"])
  
  # create edges
  Ne <- length(df[,1])
  edge_shapes <- list()
  for(i in 1:Ne) {
    v0 <- df[i,1]
    v1 <- df[i,2]
    
    rownames(layout) = names(V(G))
    
    edge_shape = list(
      type = "line",
      line = list(color = "#030303", width = 0.3),
      x0 = as.double(layout[v0,1]),
      y0 = as.double(layout[v0,2]),
      x1 = as.double(layout[v1,1]),
      y1 = as.double(layout[v1,2])
    )
    
    edge_shapes[[i]] <- edge_shape
  }
  
  #create graph
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  
  fig <- layout(
    network,
    title = 'IC distance graph',
    shapes = edge_shapes,
    xaxis = axis,
    yaxis = axis
  )
  
  return(fig)
})
