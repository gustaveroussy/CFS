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
  
  IC_C = input[["IC_choice"]]

  contrib_byIC <- values$data@misc$GeneAndStat$Contrib_gene
  contrib_byIC_pos <- lapply(contrib_byIC, function(obj) obj[obj$Sig >0,])
    
  plotList <- list()
  i = 1
  
  if(input$interactive_gene_projection){
    for(k in input$Plot_image_spatial){
      for (x in input$gene_projection_gene_choice) {
        plotList[[i]] <- plot_ly()
        
        if(length(values$low_image) != 0){
          plotList[[i]] <- plotList[[i]] %>% add_trace(type="image", source = values$low_image[[k]], hoverinfo = 'skip')
        }
        
        g = names(contrib_byIC_pos[contrib_byIC_pos %like% x])
        
        if(length(g) > 5){
          numb = floor(length(g)/5)
          for(nu in 1:numb){
            g[5*nu] = paste0(g[5*nu],"<br>")
          }
        }
        
        print(names(values$data@assays$SCT@data[x,][rownames(TissueCoordinates()[[k]])]))
        
        plotList[[i]] <- plotList[[i]] %>% add_trace(x = TissueCoordinates()[[k]][,"imagecol"], y = TissueCoordinates()[[k]][,"imagerow"],
                                                     marker = list(color = values$data@assays$SCT@data[x,][rownames(TissueCoordinates()[[k]])],
                                                                   size = input$Plot_spatial_gene_size,
                                                                   colorscale = colorscale_gene_spatial(),
                                                                   coloraxis="coloraxis",
                                                                   reversescale=input$invert_color_gene_projection),
                                                     opacity = input$transparency_gene_projection,
                                                     type = 'scatter', mode = "markers",
                                                     text = values$data@assays$SCT@data[x,][rownames(TissueCoordinates()[[k]])],
                                                     customdata = names(values$data@assays$SCT@data[x,][rownames(TissueCoordinates()[[k]])]),
                                                     hovertemplate = paste0("Cell : %{customdata}<br>",
                                                                            "Expression: %{text}<br>",
                                                                            "Associated IC:<br>",
                                                                            paste0(g, collapse=","),
                                                                            "<extra></extra>")
        ) %>% layout(title = input$gene_projection_gene_choice, xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                     yaxis = list(showgrid = FALSE, showticklabels=FALSE))
    
        
        i = i+1
        
      }
    }
    
    return(subplot(plotList, nrows = length(input$Plot_image_spatial)) %>% layout(showlegend = FALSE, coloraxis=list(colorscale=colorscale_gene_spatial())))
  } else {
    
    img = img_ggplot()
    
    coordinates = TissueCoordinates_ggplot()
    
    coordinates = lapply(coordinates, function(c){
      if(length(input$gene_projection_gene_choice) > 1){
        c = cbind(c,t(values$data@assays$SCT@data[input$gene_projection_gene_choice,rownames(c)]));
      } else {
        c = cbind(c,values$data@assays$SCT@data[input$gene_projection_gene_choice, rownames(c)]);
        colnames(c)[length(colnames(c))] = c(input$gene_projection_gene_choice);
      }
      
      return(c)
    })
    
    names(coordinates) = images_names()
    
    plotList <- list()
    
    for(k in input$Plot_image_spatial){
      for(genes in input$gene_projection_gene_choice){
        
        print(img[[k]])
        print(coordinates[[k]])
        
        gene = enquo(genes)
        
        if(!is.null(img[[k]])){
          
          plotList[[paste0(k,"_",genes)]] = ggplot(coordinates[[k]], aes(imagecol, -imagerow)) +
            background_image(img[[k]]) +
            geom_point(data = coordinates[[k]], aes(color=coordinates[[k]][,!!gene]), size=input$Plot_spatial_gene_size) +
            ggplot2::scale_color_gradientn(name = genes,
                                           colours = viridis_pal(option = if(input$select_color_gene_projection %in% c("A","B","C","D","E","F","G","H")){input$select_color_gene_projection}else{"D"})(ncol(values$data)), oob=squish) +
            guides(size = "none") +
            theme_void()
          
          if(("image" %in% slotNames(values$data@images[[k]])) && as.logical(sum(dim(values$data@images[[k]]@image) > 1000))){
            plotList[[paste0(k,"_",genes)]] = plotList[[paste0(k,"_",genes)]] + xlim((25/values$data@images[[k]]@scale.factors$lowres * values$data@images[[k]]@scale.factors$hires),ncol(img[[k]])-(25/values$data@images[[k]]@scale.factors$lowres * values$data@images[[k]]@scale.factors$hires)) +
              ylim(-nrow(img[[k]])+(25/values$data@images[[k]]@scale.factors$lowres * values$data@images[[k]]@scale.factors$hires),-(25/values$data@images[[k]]@scale.factors$lowres * values$data@images[[k]]@scale.factors$hires))
          } else {
            plotList[[paste0(k,"_",genes)]] = plotList[[paste0(k,"_",genes)]] + xlim(25,ncol(img[[k]])-25) +
              ylim(-nrow(img[[k]])+25,-25)
          }
          
        } else {
          
          plotList[[paste0(k,"_",genes)]] = ggplot(coordinates[[k]], aes(imagecol, -imagerow)) +
            geom_point(data = coordinates[[k]], aes(color=coordinates[[k]][,!!gene]), size=input$Plot_spatial_gene_size) +
            ggplot2::scale_color_gradientn(name = genes,
                                           colours = viridis_pal(option = if(input$select_color_gene_projection %in% c("A","B","C","D","E","F","G","H")){input$select_color_gene_projection}else{"D"})(ncol(values$data)), oob=squish) +
            guides(size = "none") +
            theme_void()
          
        }
      }
    }
    
    output = ggarrange(plotlist=plotList,
                       labels = rep(input$gene_projection_gene_choice,length(input$Plot_image_spatial))
                       )
    
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
