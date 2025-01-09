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
      plots$display = current_plot_umap()
    }
    
    if (input$Plot_analysis_display_type == "Density") {
      req(input$Plot_display_type_choice)
      if(input$interactive_display_visualisation_UMAP){
        plots$display = current_plot_density()
      } else {
        plots$display = NULL
      }
    }
      
    if (input$Plot_analysis_display_type == "Scatter pie") {
      if(input$interactive_display_visualisation_UMAP){
        plots$display = current_plot_scatter_pie()
      } else {
        plots$display = NULL
      }
    }
    
})