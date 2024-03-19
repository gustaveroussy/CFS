##----------------------------------------------------------------------------##
## Spatial plot of gene expression output
##----------------------------------------------------------------------------##

output[["Spatial_gene_plot"]] <- plotly::renderPlotly({
  return(spatial_gene_react())
})

spatial_gene_react <- reactive({
  data <- values$data
  
  IC_C = input[["IC_choice"]]

  contrib_byIC <- data@misc$GeneAndStat$Contrib_gene
  contrib_byIC_pos <- lapply(contrib_byIC, function(obj) obj[obj$Sig >0,])
    
  plotList <- list()
  i = 1
  for ( x in input$gene_projection_gene_choice ) {
    plotList[[i]] <- plot_ly()
    
    # if (!is.null(values$HD_image_2)){
    #   plotList[[i]] <- plotList[[i]] %>% add_trace(type="image", source = values$HD_image_2, hoverinfo = 'skip')
    # }
    if (!is.null(values$HD_image)) {
      plotList[[i]] <- plotList[[i]] %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
    } else {
      if(!is.null(values$low_image)){
        plotList[[i]] <- plotList[[i]] %>% add_trace(type="image", source = values$low_image[[1]], hoverinfo = 'skip')
      }
    }
    
    g = names(contrib_byIC_pos[contrib_byIC_pos %like% x])
    
    if(length(g) > 5){
      numb = floor(length(g)/5)
      for(nu in 1:numb){
        g[5*nu] = paste0(g[5*nu],"<br>")
      }
    }
    
    plotList[[i]] <- plotList[[i]] %>% add_trace(x = TissueCoordinates()[[1]][,"imagecol"], y = TissueCoordinates()[[1]][,"imagerow"],
                                                 marker = list(color = data@assays$SCT@scale.data[x,][rownames(TissueCoordinates()[[1]])],
                                                               size = input$Plot_spatial_gene_size,
                                                               colorscale = colorscale_gene_spatial(),
                                                               reversescale=input$invert_color_gene_projection),
                                                 opacity = input$transparency_gene_projection,
                                                 type = 'scatter', mode = "markers",
                                                 text = data@assays$SCT@scale.data[x,][rownames(TissueCoordinates()[[1]])],
                                                 customdata = names(data@assays$SCT@scale.data[x,][rownames(TissueCoordinates()[[1]])]),
                                                 hovertemplate = paste0("Cell : %{customdata}<br>",
                                                                        "Expression: %{text}<br>",
                                                                        "Associated IC:<br>",
                                                                        paste0(g, collapse=","),
                                                                        "<extra></extra>")
    ) %>% layout(title = input$gene_projection_gene_choice, xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                 yaxis = list(showgrid = FALSE, showticklabels=FALSE))

    
    i = i+1
  }
    
  subplot(plotList, nrows = ceiling(length(input$gene_projection_gene_choice)/3)) %>% layout(showlegend = FALSE)
})


##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Spatial_gene_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("Spatial_gene_plot",
                         width = "auto",
                         height = "85vh")
  )
})

##----------------------------------------------------------------------------##
## Create the colorscale for gene spatial
##----------------------------------------------------------------------------##
colorscale_gene_spatial <- reactive({
  if(input$select_color_gene_projection %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
    return(input$select_color_gene_projection)
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
