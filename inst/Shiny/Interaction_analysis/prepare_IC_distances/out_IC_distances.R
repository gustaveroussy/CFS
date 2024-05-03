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
      
      if(length(data@images) > 1){
        table_sample = table[grepl(paste0(sample,"_[ACGT]"), rownames(table)),]
      } else {
        table_sample = table
      }
      
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
      
      df[,"Lee"] = scale(df[,"Lee"])
      
      df = df[!(as.double(df[,"Lee"]) <= 0),]
      
      df = df[!(as.double(df[,"Lee"]) <= (sd(as.double(df[,"Lee"])) * input$Z_score_for_distances)),]
      
      df[,"Lee"] = 1/df[,"Lee"]
      
      values$distances[[sample]][[method]] = df
      
    }
    
  })
  
})


fig_distance_graph_IC <- reactive({
  tree_table = values$distances[[input$choose_sample_for_distances]][[input$choose_method_for_distances]]
  
  req(tree_table)
  
  G = graph_from_data_frame(tree_table, directed = FALSE)
  
  layout <- layout_with_fr(G)
  
  Xn <- layout[,1]
  Yn <- layout[,2]
  
  #create graph
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  
  fig = plot_ly()
  
  # create edges
  for(i in 1:length(tree_table[,1])) {
    v0 <- tree_table[i,1]
    v1 <- tree_table[i,2]
    
    rownames(layout) = names(V(G))

    fig = fig %>% add_segments(x = as.double(layout[v0,1]),
                 xend = as.double(layout[v1,1]),
                 y = as.double(layout[v0,2]),
                 yend = as.double(layout[v1,2]),
                 line=list(color="black",
                           width = input$choose_edges_size_for_distances
                           ))
      
  }

  
  # create vertices colors
  colors = rep(base_palette(),ceiling(length(unique(values$Annotation[names(V(G)),"Type"]))/length(base_palette())))[as.double(as.factor(values$Annotation[names(V(G)),"Type"]))]
  
  #create edges
  fig = fig %>%
    add_markers(x = ~Xn, y = ~Yn,
                text = names(V(G)), hoverinfo = "text",
                marker = list(
                  color = colors,
                  size = input$choose_vertices_size_for_distances
                ),
                opacity = 1
    ) %>% layout(
    title = 'Distance graph',
    xaxis = axis,
    yaxis = axis
  ) %>%
  hide_legend()
  
  return(fig)
})
