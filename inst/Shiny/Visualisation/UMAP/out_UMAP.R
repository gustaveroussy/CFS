##----------------------------------------------------------------------------##
## Prepare the output of the UMAP
##----------------------------------------------------------------------------##

plots <- reactiveValues(button_check = 1, umap = NULL, spatial = NULL, density = NULL, spatial_density = NULL, scatter_pie = NULL, spatial_scatter_pie = NULL)

observeEvent(input$start_plot, {
  if (input$start_plot >= plots$button_check) {
    if (input$Plot_analysis_type == "UMAP"){
      plots$umap = current_plot_umap()
      plots$spatial = current_plot_spatial()
    } else if (input$Plot_analysis_type == "Density") {
      req(input$Plot_display_type_choice)
      plots$density = current_plot_density()
      plots$spatial_density = current_plot_spatial_density()
    } else if (input$Plot_analysis_type == "Scatter pie") {
      req(values$data)
      #plots$scatter_pie = current_plot_scatter_pie()
      if(input$Spatial_use_ggplot){
        plots$spatial_scatter_pie = current_plot_spatial_scatter_pie_ggplot()
      } else {
        plots$spatial_scatter_pie = current_plot_spatial_scatter_pie()
      }
    }
    plots$button_check <- input$start_plot + 1
  }
})

output[["Plot"]] <- plotly::renderPlotly({
  if (input$Plot_analysis_type == "UMAP"){
    return(plots$umap)
  } else if (input$Plot_analysis_type == "Density") {
    return(plots$density)
  } else if (input$Plot_analysis_type == "Scatter pie") {
    return(NULL)
    #plots$scatter_pie
  }
})

output[["Plot_Spatial"]] <- plotly::renderPlotly({
  if (input$Plot_analysis_type == "UMAP"){
    return(plots$spatial)
  } else if (input$Plot_analysis_type == "Density") {
    return(plots$spatial_density)
  } else if (input$Plot_analysis_type == "Scatter pie") {
    return(plots$spatial_scatter_pie)
  }
})

output[["Plot_Spatial_ggplot"]] <- shiny::renderPlot({
  if (input$Plot_analysis_type == "Scatter pie") {
    return(plots$spatial_scatter_pie)
  }
})