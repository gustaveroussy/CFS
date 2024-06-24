##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["output_graph_auto_annotation"]] <- renderUI({
  req(values$data)
  tagList(
    plotly::plotlyOutput("cell_graph_auto_annotation_plot",
                         width = "auto",
                         height = "85vh"),
    actionButton("start_add_annotation", "Add annotation"),
    checkboxInput("start_add_annotation_with_names", "Use Enrichment names", value = FALSE, width = NULL),
    selectInput("start_add_annotation_with_names_databases", label = "Choose database",
                choices = if(!is.null(names(values$data@misc[[names(values[["data"]]@misc)[grepl("IC_", names(values[["data"]]@misc))][1]]]))){names(values$data@misc[[names(values[["data"]]@misc)[grepl("IC_", names(values[["data"]]@misc))][1]]][["en"]])} else {NULL},
                selected = NULL, multiple = FALSE,
                selectize = TRUE, width = NULL, size = NULL),
    textInput("new_col_name_annotation", "Name", value = "New_column", width = NULL,
              placeholder = NULL)
  )
})

output[["output_graph_auto_annotation_2"]] <- renderUI({
  req(select_tree_create_annotation())
  tagList(
    lapply(1:values$tree_ks, function(n) {
      textInput(inputId = paste0("start_add_annotation_name_manual_",n), paste0("Annotation ",n), value = "", width = NULL,
                placeholder = NULL)
    })
  )
})

output[["cell_graph_auto_annotation_plot"]] <- plotly::renderPlotly({
  return(figure_and_tree_cutoff_annotation()$figure)
})

select_tree_create_annotation = reactive({
  x = plotly::event_data(c("plotly_click"), source = "I")
  return(x)
})

figure_and_tree_cutoff_annotation = reactive({
  req(values$data)
  req(values$data@reductions$ica)
  
  dist.mat <- dist(t(values$data@reductions$ica@feature.loadings), method = "euclidean")
  true.dist.mat <- as.dist(dist.mat)
  clust.res <- hclust(true.dist.mat, method = "ward.D")
  
  p <- ggdendrogram(clust.res, rotate = FALSE, size = 2)
  
  p = ggplotly(p, source = "I")
  
  if(!is.null(select_tree_create_annotation())){
    l_row = as.double(select_tree_create_annotation()["y"])
    
    p[["x"]][["layout"]][["shapes"]][[2]] = list(type = "line",
                                                 line = list(color="Red", width=3, dash="dashdot"), opacity = 1,
                                                 x0 = 0,
                                                 x1 = 1, xref = "paper",
                                                 y0 = l_row, y1 = l_row, yref = "y")
    
    Ks = sum(clust.res[["height"]] > l_row) + 1
    values$tree_ks = Ks
    cut = cutree(clust.res,Ks)
    
    p[["x"]][["layout"]][["xaxis"]][["ticktext"]] = paste0(p[["x"]][["layout"]][["xaxis"]][["ticktext"]]," ",cut[p[["x"]][["layout"]][["xaxis"]][["ticktext"]]])
    p[["x"]][["layout"]][["xaxis"]][["categoryarray"]] = paste0(p[["x"]][["layout"]][["xaxis"]][["categoryarray"]]," ",cut[p[["x"]][["layout"]][["xaxis"]][["categoryarray"]]])
    
    cut = cut[rownames(values$Annotation)]
    
    if(input$start_add_annotation_with_names){
      
      annotations = lapply(values$data@misc[names(values[["data"]]@misc)[grepl("IC_", names(values[["data"]]@misc))]],
                           function(x){return(x$en[[input$start_add_annotation_with_names_databases]])})
      
      ICs_list = lapply(1:Ks,function(k){return(names(cut[as.double(cut) == k])[names(cut[as.double(cut) == k]) %in% names(annotations)])})
      
      
      for(k in 1:Ks){
        unique_terms = NULL
        for(i in ICs_list[[k]]){
          if(is.null(unique_terms)){
            unique_terms = annotations[[i]][,c("Term","Combined.Score")]
          } else {
            unique_terms = dplyr::left_join(unique_terms,annotations[[i]][,c("Term","Combined.Score")], by = "Term")
          }
        }
        
        result = rowSums(unique_terms[-1],na.rm=TRUE)
        names(result) = unique_terms$Term
        
        cut[cut == k] = names(which.max(result))
      }
    } else {
      for(i in 1:values$tree_ks){
        if(input[[paste0("start_add_annotation_name_manual_",i)]] != ""){
          cut[cut == i] = input[[paste0("start_add_annotation_name_manual_",i)]]
        }
      }
    }
  }
  
  return(list(figure = p, clusters = cut))
})

observeEvent(input$start_add_annotation,{
  values$Annotation = cbind(values$Annotation,figure_and_tree_cutoff_annotation()$clusters)
  colnames(values$Annotation)[ncol(values$Annotation)] = input$new_col_name_annotation
  
  associate_signal_with_IC()
})


