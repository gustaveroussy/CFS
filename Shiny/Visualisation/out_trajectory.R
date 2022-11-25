##----------------------------------------------------------------------------##
## trajectory
##----------------------------------------------------------------------------##
trajectory_plots <- reactiveValues(button_check = 1, trajectory = NULL, spatial_trajectory = NULL)

observeEvent(input$start_plot_trajectory, {
  if (input$start_plot_trajectory == trajectory_plots$button_check) {
    trajectory_plots$trajectory = current_plot_trajectory()
    trajectory_plots$spatial_trajectory = current_plot_spatial_trajectory()
    trajectory_plots$button_check <- input$start_plot_trajectory + 1
  }
})

output[["trajectory"]] <- plotly::renderPlotly({
  trajectory_plots$trajectory
})

output[["trajectory_Spatial"]] <- plotly::renderPlotly({
  trajectory_plots$spatial_trajectory
})