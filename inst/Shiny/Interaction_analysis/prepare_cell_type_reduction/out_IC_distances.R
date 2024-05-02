##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["IC_distances_plot"]] <- plotly::renderPlotly({
  return(plotly::plot_ly())
})

output[["IC_distances_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("IC_distances_plot",
                         width = "auto",
                         height = "85vh")
  )
})