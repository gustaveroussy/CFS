##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

distances_plots <- reactiveValues(boxplot = NULL)

output[["Boxplot_distances_plot"]] <- plotly::renderPlotly({
  req(distances_plots$boxplot)
  
  return(distances_plots$boxplot)
})

output[["Boxplot_distances_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("Boxplot_distances_plot",
                         width = "auto",
                         height = "85vh")
  )
})

observeEvent(input$start_distance_boxplot, {
  
  if(input$boxplot_interaction_boxplot == "All values"){
    distances_plots$boxplot = fig_distance_boxplot_for_each_samples()
  } else if (input$boxplot_interaction_boxplot == "Interaction analysis"){
    distances_plots$boxplot = fig_distance_boxplot_analysis()
  } else if (input$boxplot_interaction_boxplot == "Niche analysis"){
    distances_plots$boxplot = fig_distance_boxplot_niche_analysis()
  }
  
})






