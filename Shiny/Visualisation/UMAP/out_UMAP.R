##----------------------------------------------------------------------------##
## Prepare the output of the UMAP
##----------------------------------------------------------------------------##

plots <- reactiveValues(button_check = 1, umap = NULL, spatial = NULL, density = NULL, spatial_density = NULL)

observeEvent(input$start_plot, {
  if (input$start_plot == plots$button_check) {
    if (input$Plot_analysis_type == "UMAP"){
      plots$umap = current_plot_umap()
      plots$spatial = current_plot_spatial()
    } else if (input$Plot_analysis_type == "Density") {
      plots$density = current_plot_density()
      plots$spatial_density = current_plot_spatial_density()
    }
    plots$button_check <- input$start_plot + 1
  }
})

output[["Plot"]] <- plotly::renderPlotly({
  if (input$Plot_analysis_type == "UMAP"){
    return(plots$umap)
  } else if (input$Plot_analysis_type == "Density") {
    return(plots$density)
  }
})

output[["Plot_Spatial"]] <- plotly::renderPlotly({
  if (input$Plot_analysis_type == "UMAP"){
    return(plots$spatial)
  } else if (input$Plot_analysis_type == "Density") {
    return(plots$spatial_density)
  }
})