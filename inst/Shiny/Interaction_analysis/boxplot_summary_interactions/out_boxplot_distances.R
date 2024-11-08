##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["Boxplot_distances_plot"]] <- plotly::renderPlotly({
  req(fig_distance_boxplot())
  
  return(fig_distance_boxplot())
})

output[["Boxplot_distances_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("Boxplot_distances_plot",
                         width = "auto",
                         height = "85vh")
  )
})

fig_distance_boxplot <- reactive({
  
  if(input$boxplot_interaction_boxplot == "All values"){
    fig = fig_distance_boxplot_for_each_samples()
  } else if (input$boxplot_interaction_boxplot == "Niche analysis"){
    fig = fig_distance_boxplot_niche_analysis()
  }
  
    
  return(fig)
})
