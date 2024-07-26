##----------------------------------------------------------------------------##
## Prepare the output of the UMAP
##----------------------------------------------------------------------------##

plots <- reactiveValues(button_check = 1, display = NULL, spatial = NULL)

# Display UMAP
# Display spatial
observeEvent(input$start_display_Spatial, {
  req(values$data)
  req(input$Plot_analysis_display_type)
  
  if (input$Plot_analysis_display_type == "Dimentional reduction"){
    req(input$Visualisation_selected_dimred_to_display)
    if (values$data@reductions[[input$Visualisation_selected_dimred_to_display]]@key == "umap_"){
      plots$spatial = current_plot_spatial()
    } else if (values$data@reductions[[input$Visualisation_selected_dimred_to_display]]@key == "tsne_") {
      plots$spatial = current_plot_spatial()
    }
  }
  
  if (input$Plot_analysis_display_type == "Density") {
    req(input$Plot_display_type_choice)
    if(input$interactive_display_visualisation_spatial){
      plots$spatial = current_plot_spatial_density()
    } else {
      plots$spatial = current_plot_spatial_density_ggplot()
    }
  }
  
  if (input$Plot_analysis_display_type == "Scatter pie") {
    if(input$interactive_display_visualisation_spatial){
      plots$spatial = current_plot_spatial_scatter_pie()
    } else {
      plots$spatial = current_plot_spatial_scatter_pie_ggplot()
    }
  }
  
})