##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

# search for the cells that were selected while in density
clicked_interaction_plot <- reactive({
  req(values$HD_image_2)
  if(!is.null(current_plot_graph_interactions())){
    return(plotly::event_data(c("plotly_click"), source = "T"))
  }
})

observeEvent(clicked_interaction_plot(), {
  req(values$HD_image_2)
  table = clicked_interaction_plot()
  
  if (is.null(values$HD_image)) {
    table$x = table$x * (1/values$data@images[[input$choose_sample_for_distances]]@scale.factors[["lowres"]])
    table$y = table$y * (1/values$data@images[[input$choose_sample_for_distances]]@scale.factors[["lowres"]])
  } else {
    table$x = table$x * (1/values$data@images[[input$choose_sample_for_distances]]@scale.factors[["hires"]])
    table$y = table$y * (1/values$data@images[[input$choose_sample_for_distances]]@scale.factors[["hires"]])
  }
  
  min_x = table$x-ceiling(1/(values$data@images[[input$choose_sample_for_distances]]@spot.radius))*2
  min_y = table$y-ceiling(1/(values$data@images[[input$choose_sample_for_distances]]@spot.radius))*2
  max_x = table$x+ceiling(1/(values$data@images[[input$choose_sample_for_distances]]@spot.radius))*2
  max_y = table$y+ceiling(1/(values$data@images[[input$choose_sample_for_distances]]@spot.radius))*2
  
  cropped_image = values$HD_image_2[min_y:max_y,min_x:max_x,]

  values$cropped_image = raster2uri(raster::as.raster(cropped_image))
  
  shinyalert(html = TRUE, text = tagList(
    plotlyOutput('mini_plot_interactions')
  ))
})

output[["mini_plot_interactions"]] <- renderPlotly({
  fig = plot_ly()
  fig <- fig %>% add_trace(type="image", source = values$cropped_image, hoverinfo = 'skip')
  # add shapes to the layout
  fig <- layout(fig,
                shapes = list(
                  list(type = "circle",
                       fillcolor = NULL, line = list(color = "black"), opacity = 0.5,
                       x0 = ceiling(1/(values$data@images[[input$choose_sample_for_distances]]@spot.radius)),
                       x1 = ceiling(1/(values$data@images[[input$choose_sample_for_distances]]@spot.radius))*3, xref = "x",
                       y0 = ceiling(1/(values$data@images[[input$choose_sample_for_distances]]@spot.radius)),
                       y1 = ceiling(1/(values$data@images[[input$choose_sample_for_distances]]@spot.radius))*3, yref = "y")))
})

