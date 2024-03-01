##----------------------------------------------------------------------------##
## Prepare the output of the UMAP
##----------------------------------------------------------------------------##

plots <- reactiveValues(button_check = 1, display = NULL, spatial = NULL)

# Display UMAP
observeEvent(input$start_display_UMAP, {
    if (input$Plot_analysis_display_type == "Dimentional reduction"){
      plots$display = current_plot_umap()
    } else if (input$Plot_analysis_display_type == "Dimentional reduction" & input$Visualisation_selected_dimred_to_display == "tsne") {
      plots$display = current_plot_tSNE()
    } else if (input$Plot_analysis_display_type == "Density") {
      req(input$Plot_display_type_choice)
      if(input$Spatial_use_ggplot){
        #plots$density = current_plot_density()
      } else {
        plots$display = current_plot_density()
      }
    } else if (input$Plot_analysis_display_type == "Scatter pie") {
      req(values$data)
      plots$display = current_plot_scatter_pie()
    }
})

# Display spatial
observeEvent(input$start_display_Spatial, {
  if (input$Plot_analysis_display_type == "Dimentional reduction"){
    plots$spatial = current_plot_spatial()
  } else if (input$Plot_analysis_display_type == "Dimentional reduction" & input$Visualisation_selected_dimred_to_display == "tsne") {
    plots$spatial = current_plot_spatial()
  } else if (input$Plot_analysis_display_type == "Density") {
    req(input$Plot_display_type_choice)
    if(input$Spatial_use_ggplot){
      plots$spatial = current_plot_spatial_density_ggplot()
    } else {
      plots$spatial = current_plot_spatial_density()
    }
  } else if (input$Plot_analysis_display_type == "Scatter pie") {
    req(values$data)
    if(input$Spatial_use_ggplot){
      plots$spatial = current_plot_spatial_scatter_pie_ggplot()
    } else {
      plots$spatial = current_plot_spatial_scatter_pie()
    }
  }
})

# Display both
observeEvent(input$start_display, {
  if (input$start_display >= plots$button_check) {
    if (input$Plot_analysis_display_type == "Dimentional reduction"){
      plots$display = current_plot_umap()
      plots$spatial = current_plot_spatial()
    } else if (input$Plot_analysis_display_type == "Dimentional reduction" & input$Visualisation_selected_dimred_to_display == "tsne") {
      plots$display = current_plot_tSNE()
      plots$spatial = current_plot_spatial()
    } else if (input$Plot_analysis_display_type == "Density") {
      req(input$Plot_display_type_choice)
      if(input$Spatial_use_ggplot){
        #plots$density = current_plot_density()
        plots$spatial = current_plot_spatial_density_ggplot()
      } else {
        plots$display = current_plot_density()
        plots$spatial = current_plot_spatial_density()
      }
    } else if (input$Plot_analysis_display_type == "Scatter pie") {
      req(values$data)
      plots$display = current_plot_scatter_pie()
      if(input$Spatial_use_ggplot){
        plots$spatial = current_plot_spatial_scatter_pie_ggplot()
      } else {
        plots$spatial = current_plot_spatial_scatter_pie()
      }
    }
    plots$button_check <- input$start_display + 1
  }
})

output[["Plot"]] <- plotly::renderPlotly({
  return(plots$display)
})

output[["Plot_Spatial"]] <- plotly::renderPlotly({
  return(plots$spatial)
})

output[["Plot_Spatial_ggplot"]] <- shiny::renderPlot({
  return(plots$spatial)
})