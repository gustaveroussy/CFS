##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["plot_interactions_IC_genes_from_graph"]] <- renderPlotly({
  req(current_plot_graph_interactions_IC_genes())
  return(current_plot_graph_interactions_IC_genes())
})

output[["plot_interactions_IC_genes_from_graph_or_message"]] <- renderUI({
  req(interactions_IC_genes_top_by_ICs())
  tagList(
    plotly::plotlyOutput("plot_interactions_IC_genes_from_graph",
                         width = "auto",
                         height = "85vh"),
    renderText({HTML(paste0("Lee value = ", interactions_IC_genes_top_by_ICs()[["L"]],"<br>p-value = "))})
  )
})

current_plot_graph_interactions_IC_genes <- reactive({
  table = interactions_IC_genes_top_by_ICs()[["local"]]
  
  req(table)
  req((input$choose_distances_to_determine == "IC"))
  
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
  
  if(input$select_Genes_interactions_IC_genes_type == "All genes"){
    datatable <- data.frame("x" = as.vector(TissueCoordinates[,"imagecol"]),
                            "y" = as.vector(TissueCoordinates[,"imagerow"]),
                            "cell_name" = as.vector(rownames(meta.data)),
                            "local" = interactions_IC_genes_top_by_ICs()[["local"]],
                            "gene_1" = values$data@assays$SCT@data[input$select_Genes_interactions_IC_genes_choice_1,(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates))],
                            "gene_2" = values$data@assays$SCT@data[input$select_Genes_interactions_IC_genes_choice_2,(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates))]
    )
  } else if (input$select_Genes_interactions_IC_genes_type == "LR"){
    genes = unlist(str_split(input$select_Genes_interactions_IC_genes_choice,"_"))
    
    datatable <- data.frame("x" = as.vector(TissueCoordinates[,"imagecol"]),
                            "y" = as.vector(TissueCoordinates[,"imagerow"]),
                            "cell_name" = as.vector(rownames(meta.data)),
                            "local" = interactions_IC_genes_top_by_ICs()[["local"]],
                            "gene_1" = values$data@assays$SCT@data[genes[1],(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates))],
                            "gene_2" = values$data@assays$SCT@data[genes[2],(rownames(values$data@reductions$ica@cell.embeddings) %in% rownames(TissueCoordinates))]
    )
  }
  
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
                color = datatable$gene_1,
                colorscale = colorscale_interactions_ICs_genes(),
                size = input$plot_interactions_IC_genes_size,
                opacity = if(input$transparency_interactions_IC_genes_choice == 1){input$transparency_interactions_IC_genes_range}else{alpha_color_scale(values = datatable$gene_1, slider_1 = min(datatable$gene_1), slider_2 =max(datatable$gene_1), alpha = input$transparency_interactions_IC_genes_range)}
              ),
              showlegend = T,
              text = datatable$cell_name,
              customdata = datatable$gene_1,
              hovertemplate = "Cell : %{text}<br>Gene : %{customdata}<extra></extra>"
    ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                 yaxis = list(showgrid = FALSE, showticklabels=FALSE))
  
  fig2 <- fig2 %>%
    add_trace(data = datatable,
              x = ~x,
              y = ~y,
              marker = list(
                color = datatable$local,
                colorscale = colorscale_interactions_ICs_genes(),
                size = input$plot_interactions_IC_genes_size,
                opacity = if(input$transparency_interactions_IC_genes_choice == 1){input$transparency_interactions_IC_genes_range}else{alpha_color_scale(values = datatable$gene_1, slider_1 = min(datatable$gene_1), slider_2 =max(datatable$gene_1), alpha = input$transparency_interactions_IC_genes_range)}
              ),
              showlegend = T,
              text = datatable$cell_name,
              customdata = datatable$local,
              hovertemplate = "Cell : %{text}<br>Gene : %{customdata}<extra></extra>"
    ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                 yaxis = list(showgrid = FALSE, showticklabels=FALSE))
  
  fig3 <- fig3 %>%
    add_trace(data = datatable,
              x = ~x,
              y = ~y,
              marker = list(
                color = datatable$gene_2,
                colorscale = colorscale_interactions_ICs_genes(),
                size = input$plot_interactions_IC_genes_size,
                opacity = if(input$transparency_interactions_IC_genes_choice == 1){input$transparency_interactions_IC_genes_range}else{alpha_color_scale(values = datatable$gene_1, slider_1 = min(datatable$gene_1), slider_2 =max(datatable$gene_1), alpha = input$transparency_interactions_IC_genes_range)}
              ),
              showlegend = T,
              text = datatable$cell_name,
              customdata = datatable$gene_2,
              hovertemplate = "Cell : %{text}<br>Gene : %{customdata}<extra></extra>"
    ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                  yaxis = list(showgrid = FALSE, showticklabels=FALSE))
  
  fig <- subplot(list(fig1, fig2, fig3),
                 nrows = 1
                 ) %>% hide_legend()
  
  fig = fig %>% plotly::add_annotations(text = if(input$select_Genes_interactions_IC_genes_type == "All genes"){paste0("<i><b>", input$select_Genes_interactions_IC_genes_choice_1, "</i></b>")}
                                        else{paste0("<i><b>", genes[1], "</i></b>")},
                                        x = 0.15,
                                        y = 0.2,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "center",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 34))
  
  fig = fig %>% plotly::add_annotations(text = if(input$select_Genes_interactions_IC_genes_type == "All genes"){paste0("<i><b>", paste0(input$select_Genes_interactions_IC_genes_choice_1," ",input$select_Genes_interactions_IC_genes_choice_2), "</i></b>")}
                                        else{paste0("<i><b>", paste0(genes, collapse="_"), "</i></b>")},
                                        x = 0.5,
                                        y = 0.2,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "center",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 34))
  
  fig = fig %>% plotly::add_annotations(text = if(input$select_Genes_interactions_IC_genes_type == "All genes"){paste0("<i><b>", input$select_Genes_interactions_IC_genes_choice_2, "</i></b>")}
                                        else{paste0("<i><b>", genes[2], "</i></b>")},
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
colorscale_interactions_ICs_genes <- reactive({
  if(input$select_color_interactions_IC_genes %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
    return(input$select_color_interactions_IC_genes)
  } else {
    #prepare colorscales
    l = list()
    se = seq(0, 1, (1/(ncol(values$data)-1)))
    col = viridis_pal(option = input$select_color_interactions_IC_genes)(ncol(values$data))
    for(i in 1:length(se)){
      l[[i]] = c(se[i],col[i])
    }
    
    return(l)
  }
})
