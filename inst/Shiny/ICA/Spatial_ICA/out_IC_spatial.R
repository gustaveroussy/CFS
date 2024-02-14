##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

output[["Spatial_IC_plot"]] <- plotly::renderPlotly({
  return(IC_spatial_output_react())
})

IC_spatial_output_react <- reactive({
  req(values$data)
  
  data <- values$data
  
  IC_C = input[["IC_choice"]]
  
  out = list()
  
  n = 0
  
  for(sample in input$Plot_image_spatial){
    n = n+1
    
    fig <- plot_ly(source = "E")
    
    if (!is.null(values$HD_image)) {
      fig <- fig %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
    } else {
      if(!is.null(values$low_image)){
        fig <- fig %>% add_trace(type="image", source = values$low_image[[n]], hoverinfo = 'skip')
      }
    }
    
    #prepare trace
    fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                             x = TissueCoordinates()[[n]][,"imagecol"], y = TissueCoordinates()[[n]][,"imagerow"],
                             marker = list(color = data@reductions$ica@cell.embeddings[, IC_C][rownames(TissueCoordinates()[[n]])],
                                           colorscale = colorscale_IC_spatial(),
                                           cmin = input$slider_IC_spatial_range[1], cmax=input$slider_IC_spatial_range[2],
                                           size = input$Plot_spatial_IC_size,
                                           showscale = TRUE,
                                           opacity = if(input$transparency_IC_spatial_choice == 1){input$transparency_IC_spatial_range}else{alpha_color_scale(values = data@reductions$ica@cell.embeddings[, IC_C][rownames(TissueCoordinates()[[n]])], slider_1 = input$slider_IC_spatial_range[1], slider_2 =input$slider_IC_spatial_range[2], alpha = input$transparency_IC_spatial_range)},
                                           reversescale=input$invert_color_ICA_projection
                                           ),
                             text = data@reductions$ica@cell.embeddings[, IC_C][rownames(TissueCoordinates()[[n]])],
                             customdata = names(data@reductions$ica@cell.embeddings[, IC_C][rownames(TissueCoordinates()[[n]])]),
                             hovertemplate = paste0("Cell : %{customdata}<br>",
                                                    "Expression: %{text}",
                                                    "<extra></extra>")
    )
    
    fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                          yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                          showlegend = FALSE
    )
    
    out[[n]] = fig
  }
  
  output <- subplot(out, nrows = ceiling(length(TissueCoordinates())/3))
  
  return(output)
})


##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Spatial_IC_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("Spatial_IC_plot",
                         width = "auto",
                         height = "85vh")
  )
})

##----------------------------------------------------------------------------##
## Create the colorscale for IC spatial
##----------------------------------------------------------------------------##
colorscale_IC_spatial <- reactive({
  if(input$select_color_IC_projection %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
    return(input$select_color_IC_projection)
  } else {
    #prepare colorscales
    l = list()
    se = seq(0, 1, (1/(ncol(values$data)-1)))
    col = viridis_pal(option = input$select_color_gene_projection)(ncol(values$data))
    for(i in 1:length(se)){
      l[[i]] = c(se[i],col[i])
    }
    
    return(l)
  }
})




