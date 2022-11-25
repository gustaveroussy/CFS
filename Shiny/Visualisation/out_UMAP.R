##----------------------------------------------------------------------------##
## Prepare the output of the UMAP
##----------------------------------------------------------------------------##

plots <- reactiveValues(button_check = 1, umap = NULL, spatial = NULL)

observeEvent(input$start_plot, {
  if (input$start_plot == plots$button_check) {
    plots$umap = current_plot_umap()
    plots$spatial = current_plot_spatial()
    plots$button_check <- input$start_plot + 1
  }
})

output[["Plot"]] <- plotly::renderPlotly({
  plots$umap
})

output[["Plot_Spatial"]] <- plotly::renderPlotly({
  plots$spatial
})