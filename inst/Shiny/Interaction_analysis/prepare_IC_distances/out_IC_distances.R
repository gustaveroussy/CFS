##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["IC_distances_plot"]] <- plotly::renderPlotly({
  req(fig_distance_graph_IC())
  
  return(fig_distance_graph_IC())
})

output[["IC_distances_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("IC_distances_plot",
                         width = "auto",
                         height = "85vh")
  )
})

observeEvent(input$start_distance_IC,{
  req(values$data)
  req(values$data@reductions$ica)
  
  method = input$choose_method_for_distances
  sample = input$choose_sample_for_distances
  
  if(is.null(values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[method]]) & is.null(values$distances[[paste0(input$choose_distances_to_determine_2,"_",input$choose_distances_to_determine)]][[sample]][[method]])){

    values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[method]] = Calculate_distances(method = input$choose_method_for_distances, sample = input$choose_sample_for_distances)

  } else {
    shinyalert("Info", "Already calculated", type = "info")
  }
  
})

observeEvent(input$start_distance_IC_batch,{
  req(values$data)
  req(values$data@reductions$ica)
  
  method = input$choose_method_for_distances
  sample_all = names(values$data@images)
  
  for(sample in sample_all){
    values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[method]] = Calculate_distances(method = input$choose_method_for_distances, sample = sample)
  }
})

fig_distance_graph_IC <- reactive({
  l = create_distance_graph(sample = input$choose_sample_for_distances)

  if(gsize(l[["G"]]) > 0){
    
    edges_list = as.data.frame(as_edgelist(l[["G"]]))
    
    fig = create_distance_graph_plotly(G = l[["G"]], layout = input$choose_layout_for_distances, n_dim = input$choose_n_dim_for_distances, edges_list = edges_list, tree_table = l[["tree_table"]])
    
    return(fig)
  } else {
    return(NULL)
  }
})
