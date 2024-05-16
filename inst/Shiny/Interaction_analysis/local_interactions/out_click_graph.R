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
  
  ICs = str_split(table$customdata,"<br>") |> unlist() |> str_split(" <-> ")
  ICs = ICs[[1]]
  
  ICs = paste0(ICs[1], " ", ICs[2])
  
  if(!is.null(values$HD_image)) {
    c <- values$data@images[[input$choose_sample_for_distances]]@coordinates * values$data@images[[input$choose_sample_for_distances]]@scale.factors$hires
    names(c)[names(c) == "x"] <- "imagerow"
    names(c)[names(c) == "y"] <- "imagecol"
    
    if(input$spatial_mirror_X){
      c$imagecol = c$imagecol * (-1)
    }
    if(input$spatial_mirror_Y){
      c$imagerow = c$imagerow * (-1)
    }
    if(input$spatial_flip){
      imagerow = c$imagerow
      c$imagerow = c$imagecol
      c$imagecol = imagerow
    }
    
  } else {
    c <- GetTissueCoordinates(values$data, image = input$choose_sample_for_distances)
    names(c)[names(c) == "x"] <- "imagerow"
    names(c)[names(c) == "y"] <- "imagecol"
    
    if(input$spatial_mirror_X){
      c$imagecol = c$imagecol * (-1)
    }
    if(input$spatial_mirror_Y){
      c$imagerow = c$imagerow * (-1)
    }
    if(input$spatial_flip){
      imagerow = c$imagerow
      c$imagerow = c$imagecol
      c$imagecol = imagerow
    }
  }
  
  TissueCoordinates = c
  meta.data = values$data@meta.data[(rownames(values$data@meta.data) %in% rownames(TissueCoordinates)),]
  
  datatable <- data.frame("x" = as.vector(TissueCoordinates[,"imagecol"]),
                          "y" = as.vector(TissueCoordinates[,"imagerow"]),
                          "cell_name" = as.vector(rownames(meta.data)),
                          "local" = values$distances[[input$choose_sample_for_distances]][[input$choose_method_for_distances]][["local"]][[ICs]],
                          "IC_1" = values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates)),unlist(str_split(ICs," "))[1]],
                          "IC_2" = values$data@reductions$ica@cell.embeddings[(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates)),unlist(str_split(ICs," "))[2]]
                          )
  
  if (!is.null(values$HD_image)) {
    image <- values$HD_image
  } else {
    if(length(values$low_image) != 0){
      image <- raster2uri(raster::as.raster(values$data@images[[input$choose_sample_for_distances]]@image))
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
              hovertemplate = paste0("Cell : %{text}<br>",
                                     "IC : %{customdata}",
                                     "<extra></extra>")
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
              hovertemplate = paste0("Cell : %{text}<br>",
                                     "local : %{customdata}",
                                     "<extra></extra>")
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
              hovertemplate = paste0("Cell : %{text}<br>",
                                     "IC : %{customdata}",
                                     "<extra></extra>")
    ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                  yaxis = list(showgrid = FALSE, showticklabels=FALSE))
  
  fig <- subplot(list(fig1, fig2, fig3),
                 nrows = 1
                 ) %>% hide_legend()
  
  fig = fig %>% plotly::add_annotations(text = paste0("<i><b>", unlist(str_split(ICs," "))[1], "</i></b>"),
                                        x = 0.15,
                                        y = 0.2,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "center",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 34))
  
  fig = fig %>% plotly::add_annotations(text = paste0("<i><b>", ICs, "</i></b>"),
                                        x = 0.5,
                                        y = 0.2,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "center",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 34))
  
  fig = fig %>% plotly::add_annotations(text = paste0("<i><b>", unlist(str_split(ICs," "))[2], "</i></b>"),
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
