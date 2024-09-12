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

observeEvent(plotly::event_data(c("plotly_click"), source = "S"),{
  table = plotly::event_data(c("plotly_click"), source = "S")
  
  ICs = str_split(table$customdata,"<br>") |> unlist() |> str_split(" <-> ")
  ICs = ICs[[1]]
  
  updateSelectizeInput(session,"select_interaction_1", selected = ICs[1])
  
  updateSelectizeInput(session,"select_interaction_2", selected = ICs[2])
})

current_plot_graph_interactions <- eventReactive(input$interaction_plot_start, {

  sample = input$choose_sample_for_distances
  TissueCoordinates = TissueCoordinates()[[sample]]
  meta.data = values$data@meta.data[(rownames(values$data@meta.data) %in% rownames(TissueCoordinates)),]

  if (input$choose_distances_to_determine == "Genes") {table_sample_1 = raster::t(GetAssayData(values$data))} else if(input$choose_distances_to_determine != "Genes"){table_sample_1 = values$data@reductions[[input$choose_distances_to_determine]]@cell.embeddings;if(input$use_positive_values_for_distances){table_sample_1[table_sample_1 < 0] = 0};if(input$choose_distances_to_determine != "ica"){colnames(table_sample_1) = values$data@misc$reduction_names[[input$choose_distances_to_determine]]}}
  if (input$choose_distances_to_determine_2 == "Genes") {table_sample_2 = raster::t(GetAssayData(values$data))} else if(input$choose_distances_to_determine_2 != "Genes"){table_sample_2 = values$data@reductions[[input$choose_distances_to_determine_2]]@cell.embeddings;if(input$use_positive_values_for_distances){table_sample_2[table_sample_2 < 0] = 0};if(input$choose_distances_to_determine_2 != "ica"){colnames(table_sample_2) = values$data@misc$reduction_names[[input$choose_distances_to_determine_2]]}}

  if(length(values$data@images) > 1){
    table_sample_1 = table_sample_1[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample_1)),]
    table_sample_2 = table_sample_2[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample_2)),]
  }

  knn = knearneigh(GetTissueCoordinates(values$data, sample), k=6, longlat = NULL, use_kd_tree=TRUE)
  neighbours = knn2nb(knn, row.names = NULL, sym = FALSE)
  listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)

  x = lee(table_sample_1[,input$select_interaction_1], table_sample_2[,input$select_interaction_2], listw, nrow(table_sample_1), zero.policy=attr(listw, "zero.policy"))
  
  randomization = lee.mc(table_sample_1[,input$select_interaction_1], table_sample_2[,input$select_interaction_2], listw, input$plot_interactions_n_rando, zero.policy=NULL, alternative=input$select_alternative_interactions, na.action=na.fail, spChk=NULL, return_boot=FALSE)
  
  datatable <- data.frame("x" = as.vector(TissueCoordinates[,"imagecol"]),
                          "y" = as.vector(TissueCoordinates[,"imagerow"]),
                          "cell_name" = as.vector(rownames(meta.data)),
                          "local" = x$localL,
                          "value_1" = if (input$choose_distances_to_determine == "Genes"){values$data@assays$SCT@data[input$select_interaction_1,(colnames(values$data@assays$SCT@data) %in% rownames(TissueCoordinates))]} else {table_sample_1[(rownames(table_sample_1) %in% rownames(TissueCoordinates)),input$select_interaction_1]},
                          "value_2" = if (input$choose_distances_to_determine_2 == "Genes"){values$data@assays$SCT@data[input$select_interaction_2,(colnames(values$data@assays$SCT@data) %in% rownames(TissueCoordinates))]} else {table_sample_2[(rownames(table_sample_2) %in% rownames(TissueCoordinates)),input$select_interaction_2]}
  )
  
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
                color = datatable$value_1,
                colorscale = colorscale_interactions(),
                size = input$plot_interactions_size,
                opacity = if(input$transparency_interactions_choice == 1){input$transparency_interactions_range}else{alpha_color_scale(values = datatable$value_1, slider_1 = min(datatable$value_1), slider_2 =max(datatable$value_1), alpha = input$transparency_interactions_range)}
              ),
              showlegend = F,
              text = datatable$cell_name,
              customdata = datatable$value_1,
              hovertemplate = if(input$choose_distances_to_determine != "Genes"){"Cell : %{text}<br>IC : %{customdata}<extra></extra>"} else if (input$choose_distances_to_determine == "Genes"){"Cell : %{text}<br>Gene : %{customdata}<extra></extra>"}
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
                showscale = TRUE,
                opacity = if(input$transparency_interactions_choice == 1){input$transparency_interactions_range}else{alpha_color_scale(values = datatable$local, slider_1 = min(datatable$local), slider_2 =max(datatable$local), alpha = input$transparency_interactions_range)}
              ),
              showlegend = F,
              text = datatable$cell_name,
              customdata = datatable$local,
              hovertemplate = "Cell : %{text}<br>Interaction : %{customdata}<extra></extra>"
    ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                 yaxis = list(showgrid = FALSE, showticklabels=FALSE))

  fig3 <- fig3 %>%
    add_trace(data = datatable,
              x = ~x,
              y = ~y,
              marker = list(
                color = datatable$value_2,
                colorscale = colorscale_interactions(),
                size = input$plot_interactions_size,
                opacity = if(input$transparency_interactions_choice == 1){input$transparency_interactions_range}else{alpha_color_scale(values = datatable$value_2, slider_1 = min(datatable$value_2), slider_2 =max(datatable$value_2), alpha = input$transparency_interactions_range)}
              ),
              showlegend = F,
              text = datatable$cell_name,
              customdata = datatable$value_2,
              hovertemplate = if(input$choose_distances_to_determine_2 != "Genes"){"Cell : %{text}<br>IC : %{customdata}<extra></extra>"} else if (input$choose_distances_to_determine_2 == "Genes"){"Cell : %{text}<br>Gene : %{customdata}<extra></extra>"}
    ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                  yaxis = list(showgrid = FALSE, showticklabels=FALSE))
  
  fig <- subplot(list(fig1, fig2, fig3),
                 nrows = 1
                 ) %>% layout(showlegend = FALSE)
  
  fig = fig %>% plotly::add_annotations(text = paste0("<i><b>", input$select_interaction_1, "</i></b>"),
                                        x = 0.15,
                                        y = 0.2,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "center",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 34))

  fig = fig %>% plotly::add_annotations(text = paste0("<i><b>", paste0(input$select_interaction_1, " ", input$select_interaction_2), "</i></b>"),
                                        x = 0.5,
                                        y = 0.2,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "center",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 34))

  fig = fig %>% plotly::add_annotations(text = paste0("<i><b>", input$select_interaction_2, "</i></b>"),
                                        x = 0.85,
                                        y = 0.2,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "center",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 34))
  
  
  fig = fig %>% plotly::add_annotations(text = paste0("Lee value: ", formatC(x$L, format = "e", digits = 2)),
                                        x = 0,
                                        y = 0.05,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "left",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 16)
                                        )
  
  n_correction_1 = length(if(input$choose_distances_to_determine == "Genes") {rownames(GetAssayData(values$data))} else if(input$choose_distances_to_determine == "ica"){colnames(values$data@reductions$ica@cell.embeddings)}else{values$data@misc$reduction_names[[input$choose_distances_to_determine]]})
  n_correction_2 = length(if(input$choose_distances_to_determine_2 == "Genes") {rownames(GetAssayData(values$data))} else if(input$choose_distances_to_determine_2 == "ica"){colnames(values$data@reductions$ica@cell.embeddings)}else{values$data@misc$reduction_names[[input$choose_distances_to_determine_2]]})
  
  fig = fig %>% plotly::add_annotations(text = paste0("Randomisation test p-value: ", formatC(randomization$p.value, format = "e", digits = 2)),
                                        x = 0,
                                        y = 0.025,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "left",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 16)
  )
  
  fig = fig %>% plotly::add_annotations(text = paste0("Randomisation test corrected p-value: ", formatC(p.adjust(randomization$p.value, method = input$select_correction_method_interactions, n = (n_correction_1*n_correction_2)), format = "e", digits = 2)),
                                        x = 0,
                                        y = 0,
                                        yref = "paper",
                                        xref = "paper",
                                        xanchor = "left",
                                        yanchor = "center",
                                        showarrow = FALSE,
                                        font = list(size = 16)
  )
  
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
