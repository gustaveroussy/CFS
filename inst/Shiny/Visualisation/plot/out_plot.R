##----------------------------------------------------------------------------##
## Prepare the output of the UMAP
##----------------------------------------------------------------------------##

plots <- reactiveValues(button_check = 1, display = NULL, spatial = NULL)

# Display UMAP
observeEvent(input$start_display_UMAP, {
    req(values$data)
    req(input$Plot_analysis_display_type)
    
    if (input$Plot_analysis_display_type == "Dimentional reduction"){
      req(input$Visualisation_selected_dimred_to_display)
      if (values$data@reductions[[input$Visualisation_selected_dimred_to_display]]@key == "umap_"){
        plots$display = current_plot_umap()
      } else if (values$data@reductions[[input$Visualisation_selected_dimred_to_display]]@key == "tsne_") {
        plots$display = current_plot_tSNE()
      }
    }
    
    if (input$Plot_analysis_display_type == "Density") {
      req(input$Plot_display_type_choice)
      if(input$Spatial_use_ggplot){
        #plots$density = current_plot_density()
      } else {
        plots$display = current_plot_density()
      }
    }
      
    if (input$Plot_analysis_display_type == "Scatter pie") {
      plots$display = current_plot_scatter_pie()
    }
    
})