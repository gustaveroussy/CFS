##----------------------------------------------------------------------------##
## Spatial plot of gene expression output
##----------------------------------------------------------------------------##

output[["Spatial_gene_plot_interactive"]] <- plotly::renderPlotly({
  return(spatial_gene_react())
})

output[["Spatial_gene_plot"]] <- shiny::renderPlot({
  plot(spatial_gene_react())
})

spatial_gene_react <- reactive({
  req(values$data)
  req(values$data@misc$GeneAndStat)
  req(input$gene_projection_gene_choice)
  
  data <- values$data
  
  IC_C = input[["IC_choice"]]

  contrib_byIC <- data@misc$GeneAndStat$Contrib_gene
  contrib_byIC_pos <- lapply(contrib_byIC, function(obj) obj[obj$Sig >0,])
    
  plotList <- list()
  i = 1
  
  if(input$interactive_gene_projection){
    for ( x in input$gene_projection_gene_choice ) {
      plotList[[i]] <- plot_ly()
      
      # if (!is.null(values$HD_image_2)){
      #   plotList[[i]] <- plotList[[i]] %>% add_trace(type="image", source = values$HD_image_2, hoverinfo = 'skip')
      # }
      if (!is.null(values$HD_image)) {
        plotList[[i]] <- plotList[[i]] %>% add_trace(type="image", source = values$HD_image, hoverinfo = 'skip')
      } else {
        if(length(values$low_image) != 0){
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
      
      plotList[[i]] <- plotList[[i]] %>% add_trace(x = TissueCoordinates()[[input$Plot_image_spatial[[1]]]][,"imagecol"], y = TissueCoordinates()[[input$Plot_image_spatial[[1]]]][,"imagerow"],
                                                   marker = list(color = data@assays$SCT@data[x,][rownames(TissueCoordinates()[[input$Plot_image_spatial[[1]]]])],
                                                                 size = input$Plot_spatial_gene_size,
                                                                 colorscale = colorscale_gene_spatial(),
                                                                 reversescale=input$invert_color_gene_projection),
                                                   opacity = input$transparency_gene_projection,
                                                   type = 'scatter', mode = "markers",
                                                   text = data@assays$SCT@data[x,][rownames(TissueCoordinates()[[input$Plot_image_spatial[[1]]]])],
                                                   customdata = names(data@assays$SCT@data[x,][rownames(TissueCoordinates()[[input$Plot_image_spatial[[1]]]])]),
                                                   hovertemplate = paste0("Cell : %{customdata}<br>",
                                                                          "Expression: %{text}<br>",
                                                                          "Associated IC:<br>",
                                                                          paste0(g, collapse=","),
                                                                          "<extra></extra>")
      ) %>% layout(title = input$gene_projection_gene_choice, xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                   yaxis = list(showgrid = FALSE, showticklabels=FALSE))
  
      
      i = i+1
    }
    
    return(subplot(plotList, nrows = ceiling(length(input$gene_projection_gene_choice)/3)) %>% layout(showlegend = FALSE))
  } else {
    
    coordinates = lapply(input$Plot_image_spatial,function(n){return(GetTissueCoordinates(data,n))})
    names(coordinates) = input$Plot_image_spatial
    
    img = lapply(data@images,function(n){return(n@image)})
    img = img[[input$Plot_image_spatial[1]]]
    
    if(length(input$gene_projection_gene_choice) > 1){
      coordinates = lapply(input$Plot_image_spatial, function(sample){coordinates[[sample]] = cbind(coordinates[[sample]],t(data@assays$SCT@data[input$gene_projection_gene_choice, rownames(coordinates[[sample]])]));return(coordinates[[sample]])})
    } else {
      coordinates = lapply(input$Plot_image_spatial, function(sample){coordinates[[sample]] = cbind(coordinates[[sample]],data@assays$SCT@data[input$gene_projection_gene_choice, rownames(coordinates[[sample]])]);colnames(coordinates[[sample]]) = c("imagerow", "imagecol", input$gene_projection_gene_choice);return(coordinates[[sample]])})
    }
    
    names(coordinates) = input$Plot_image_spatial
    
    coordinates = coordinates[[input$Plot_image_spatial[1]]]
    
    for(genes in input$gene_projection_gene_choice){
      
      gene = enquo(genes)
      
      fig = ggplot(coordinates, aes(imagecol, -imagerow)) +
        background_image(img) +
        geom_point(data = coordinates, aes(color=coordinates[,!!gene]), size=input$Plot_spatial_gene_size) +
        ggplot2::scale_color_gradientn(name = genes,
                                       colours = viridis_pal(option = if(input$select_color_gene_projection %in% c("A","B","C","D","E","F","G","H")){input$select_color_gene_projection}else{"D"})(ncol(values$data)), oob=squish) +
        guides(size = "none") +
        theme_void() +
        xlim(0,ncol(img)) +
        ylim(-nrow(img),0) 
      
      
      plotList[[genes]] = fig
    }
    
    output = ggarrange(plotlist=plotList,
                       labels = input$gene_projection_gene_choice)
    
    return(output)
  }
})


##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Spatial_gene_plot_or_message"]] <- renderUI({
  if(input$interactive_gene_projection){
    tagList(
      plotly::plotlyOutput("Spatial_gene_plot_interactive",
                           width = "auto",
                           height = "85vh")
    )
  } else {
    if(length(input$gene_projection_gene_choice) > 0){
      tagList(
        shiny::plotOutput("Spatial_gene_plot",
                          width = "auto",
                          height = "85vh")
      )
    }
  }
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
