##----------------------------------------------------------------------------##
## Prepare the output of the UMAP
##----------------------------------------------------------------------------##

plots <- reactiveValues(display = NULL, spatial = NULL)

# Rendering_plotly

output[["Plot_interactive"]] <- plotly::renderPlotly({
  return(plots$display)
})

output[["Plot_Spatial_interactive"]] <- plotly::renderPlotly({
  return(plots$spatial)
})

# Rendering_ggplot

output[["Plot"]] <- shiny::renderPlot({
  return(plots$display)
})

output[["Plot_Spatial"]] <- shiny::renderPlot({
  return(plots$spatial)
})