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
  
  fig1 = plot_ly(type = "scatter", mode = "markers")
  fig2 = plot_ly(type = "scatter", mode = "markers")
  fig3 = plot_ly(type = "scatter", mode = "markers")
  
  fig1 <- fig1 %>% add_trace(type="image", source = image, hoverinfo = 'skip')
  fig2 <- fig2 %>% add_trace(type="image", source = image, hoverinfo = 'skip')
  fig3 <- fig3 %>% add_trace(type="image", source = image, hoverinfo = 'skip')
  
  fig1 <- fig1 %>%
    add_trace(data = datatable,
              x = ~x,
              y = ~y,
              marker = list(
                color = datatable$IC_1,
                size = 5
              ),
              showlegend = T,
              text = datatable$cell_name,
              customdata = datatable$IC_1,
              hovertemplate = paste0("Cell : %{text}<br>",
                                     "IC : %{customdata}",
                                     "<extra></extra>")
    )
  
  fig2 <- fig2 %>%
    add_trace(data = datatable,
              x = ~x,
              y = ~y,
              marker = list(
                color = datatable$local,
                size = 5
              ),
              showlegend = T,
              text = datatable$cell_name,
              customdata = datatable$local,
              hovertemplate = paste0("Cell : %{text}<br>",
                                     "local : %{customdata}",
                                     "<extra></extra>")
    )
  
  fig3 <- fig3 %>%
    add_trace(data = datatable,
              x = ~x,
              y = ~y,
              marker = list(
                color = datatable$IC_2,
                size = 5
              ),
              showlegend = T,
              text = datatable$cell_name,
              customdata = datatable$IC_2,
              hovertemplate = paste0("Cell : %{text}<br>",
                                     "IC : %{customdata}",
                                     "<extra></extra>")
    )
  
  fig <- subplot(fig1, fig2, fig3,
                 nrows=1,
                 shareX=TRUE,
                 shareY=TRUE
                 ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                        yaxis = list(showgrid = FALSE, showticklabels=FALSE)
  ) %>% hide_legend()
  
  return(fig)
  
})