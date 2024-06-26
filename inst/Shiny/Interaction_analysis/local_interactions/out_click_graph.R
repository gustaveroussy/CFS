##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["plot_interactions_from_graph"]] <- renderPlotly({
  req(current_plot_graph_interactions())
  return(current_plot_graph_interactions())
})

output[["plot_interactions_from_graph_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("plot_interactions_from_graph",
                         width = "auto",
                         height = "85vh")
  )
})

graph_click_interactions <- reactive({
  return(plotly::event_data(c("plotly_click"), source = "S"))
})


current_plot_graph_interactions <- reactive({
  table = graph_click_interactions()
  
  req(table)
  sample = input$choose_sample_for_distances
  
  
  ICs = str_split(table$customdata,"<br>") |> unlist() |> str_split(" <-> ")
  ICs = ICs[[1]]
  
  TissueCoordinates = TissueCoordinates()[[sample]]
  meta.data = values$data@meta.data[(rownames(values$data@meta.data) %in% rownames(TissueCoordinates)),]
  
  if(input$choose_distances_to_determine == "IC"){
    
    table_sample = values$data@reductions$ica@cell.embeddings
    
    if(input$use_positive_values_for_distances){
      table_sample[table_sample < 0] = 0
    }
    
  } else if (input$choose_distances_to_determine == "Genes") {
    table_sample = raster::t(GetAssayData(values$data))
  }
  
  if(length(values$data@images) > 1){
    table_sample = table_sample[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample)),]
  }
  
  knn = knearneigh(GetTissueCoordinates(values$data, sample), k=6, longlat = NULL, use_kd_tree=TRUE)
  neighbours = knn2nb(knn, row.names = NULL, sym = FALSE)
  listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)
  
  x = lee(table_sample[,ICs[1]], table_sample[,ICs[2]], listw, nrow(table_sample), zero.policy=attr(listw, "zero.policy"))
  
  if(input$choose_distances_to_determine == "IC"){
    
    datatable <- data.frame("x" = as.vector(TissueCoordinates[,"imagecol"]),
                            "y" = as.vector(TissueCoordinates[,"imagerow"]),
                            "cell_name" = as.vector(rownames(meta.data)),
                            "local" = x$localL,
                            "IC_1" = values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates)),ICs[1]],
                            "IC_2" = values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates)),ICs[2]]
    )
  } else if (input$choose_distances_to_determine == "Genes"){
    datatable <- data.frame("x" = as.vector(TissueCoordinates[,"imagecol"]),
                            "y" = as.vector(TissueCoordinates[,"imagerow"]),
                            "cell_name" = as.vector(rownames(meta.data)),
                            "local" = x$localL,
                            "IC_1" = values$data@assays$SCT@data[ICs[1],(colnames(values$data@assays$SCT@data) %in% rownames(TissueCoordinates))],
                            "IC_2" = values$data@assays$SCT@data[ICs[2],(colnames(values$data@assays$SCT@data) %in% rownames(TissueCoordinates))]
    )
  }
  
  if (!is.null(values$HD_image)) {
    image <- values$HD_image
  } else {
    if(length(values$low_image) != 0){
      image <- raster2uri(raster::as.raster(values$data@images[[sample]]@image))
    }
  }
  
  fig1 = plot_ly(type = "scatter", mode = "markers", source = "T")
  fig2 = plot_ly(type = "scatter", mode = "markers", source = "T")
  fig3 = plot_ly(type = "scatter", mode = "markers", source = "T")
  
  fig1 <- fig1 %>% add_trace(type="image", source = image, hoverinfo = 'skip')
  fig2 <- fig2 %>% add_trace(type="image", source = image, hoverinfo = 'skip')
  fig3 <- fig3 %>% add_trace(type="image", source = image, hoverinfo = 'skip')
  
  fig1 <- fig1 %>%
    add_trace(data = datatable,
              x = ~x,
              y = ~y,
              marker = list(
                color = datatable$IC_1,
                colorscale = colorscale_interactions(),
                size = input$plot_interactions_size,
                opacity = if(input$transparency_interactions_choice == 1){input$transparency_interactions_range}else{alpha_color_scale(values = datatable$IC_1, slider_1 = min(datatable$IC_1), slider_2 =max(datatable$IC_1), alpha = input$transparency_interactions_range)}
              ),
              showlegend = T,
              text = datatable$cell_name,
              customdata = datatable$IC_1,
              hovertemplate = if(input$choose_distances_to_determine == "IC"){"Cell : %{text}<br>IC : %{customdata}<extra></extra>"} else if (input$choose_distances_to_determine == "Genes"){"Cell : %{text}<br>Gene : %{customdata}<extra></extra>"}
    ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                 yaxis = list(showgrid = FALSE, showticklabels=FALSE))
  
  fig2 <- fig2 %>%
    add_trace(data = datatable,
              x = ~x,
              y = ~y,
              marker = list(
                color = datatable$local,
                colorscale = colorscale_interactions(),
                size = input$plot_interactions_size,
                opacity = if(input$transparency_interactions_choice == 1){input$transparency_interactions_range}else{alpha_color_scale(values = datatable$local, slider_1 = min(datatable$local), slider_2 =max(datatable$local), alpha = input$transparency_interactions_range)}
              ),
              showlegend = T,
              text = datatable$cell_name,
              customdata = datatable$local,
              hovertemplate = if(input$choose_distances_to_determine == "IC"){"Cell : %{text}<br>IC : %{customdata}<extra></extra>"} else if (input$choose_distances_to_determine == "Genes"){"Cell : %{text}<br>Gene : %{customdata}<extra></extra>"}
    ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                 yaxis = list(showgrid = FALSE, showticklabels=FALSE))
  
  fig3 <- fig3 %>%
    add_trace(data = datatable,
              x = ~x,
              y = ~y,
              marker = list(
                color = datatable$IC_2,
                colorscale = colorscale_interactions(),
                size = input$plot_interactions_size,
                opacity = if(input$transparency_interactions_choice == 1){input$transparency_interactions_range}else{alpha_color_scale(values = datatable$IC_2, slider_1 = min(datatable$IC_2), slider_2 =max(datatable$IC_2), alpha = input$transparency_interactions_range)}
              ),
              showlegend = T,
              text = datatable$cell_name,
              customdata = datatable$IC_2,
              hovertemplate = if(input$choose_distances_to_determine == "IC"){"Cell : %{text}<br>IC : %{customdata}<extra></extra>"} else if (input$choose_distances_to_determine == "Genes"){"Cell : %{text}<br>Gene : %{customdata}<extra></extra>"}
    ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                  yaxis = list(showgrid = FALSE, showticklabels=FALSE))
  
  fig <- subplot(list(fig1, fig2, fig3),
                 nrows = 1
                 ) %>% hide_legend()
  
  fig = fig %>% plotly::add_annotations(text = paste0("<i><b>", ICs[1], "</i></b>"),
                                        x = 0.15,
                                        y = 0.2,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "center",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 34))
  
  fig = fig %>% plotly::add_annotations(text = paste0("<i><b>", paste0(ICs[1], " ", ICs[2]), "</i></b>"),
                                        x = 0.5,
                                        y = 0.2,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "center",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 34))
  
  fig = fig %>% plotly::add_annotations(text = paste0("<i><b>", ICs[2], "</i></b>"),
                                        x = 0.85,
                                        y = 0.2,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "center",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 34))
  
  return(fig)
  
})


##----------------------------------------------------------------------------##
## Create the colorscale for IC spatial
##----------------------------------------------------------------------------##
colorscale_interactions <- reactive({
  if(input$select_color_interactions %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
    return(input$select_color_interactions)
  } else {
    #prepare colorscales
    l = list()
    se = seq(0, 1, (1/(ncol(values$data)-1)))
    col = viridis_pal(option = input$select_color_interactions)(ncol(values$data))
    for(i in 1:length(se)){
      l[[i]] = c(se[i],col[i])
    }
    
    return(l)
  }
})
