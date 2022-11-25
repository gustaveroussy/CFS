##----------------------------------------------------------------------------##
## trajectory
##----------------------------------------------------------------------------##
current_plot_spatial_trajectory <- reactive({
  
  req(Launch_analysis()@reductions[["umap"]])
  data <- values$data
  
  data <- Spatial_pseudotime(data,input$Plot_display_IC_choice)
  
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