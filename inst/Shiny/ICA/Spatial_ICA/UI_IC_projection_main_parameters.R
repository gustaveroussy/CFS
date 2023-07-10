##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output$pie_chart_check <- renderUI({
  req(values$data)
  req(input$IC_choice)
  IC_C = input[["IC_choice"]]
  tagList(
    sliderInput("slider_IC_spatial_range", label = "Color range",
                min = round(min(values$data@reductions$ica@cell.embeddings[, IC_C]), digits = 0), 
                max = round(max(values$data@reductions$ica@cell.embeddings[, IC_C]), digits = 0),
                value = c(round(min(values$data@reductions$ica@cell.embeddings[, IC_C]),digits = 0),
                          round(max(values$data@reductions$ica@cell.embeddings[, IC_C]), digits = 0)),
                step = 0.01),
    radioButtons("transparency_IC_spatial_choice", label = "Alpha type",
                 choices = list("Constant" = 1, "Scaling" = 2), 
                 selected = 2),
    sliderInput("transparency_IC_spatial_range", "Alpha",
                min = 0, max = 1,
                value = 1, step = 0.01),
    numericInput("Plot_spatial_IC_size", "Spot size", 7, min = 0, max = NA),
    selectInput("select_color_IC_projection", label = "Select color", 
                choices = list("Viridis", "Blues", "Reds","YlGnBu","YlOrRd","Range"), 
                selected = "Viridis")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_projection_main_parameters_info"]], {
  showModal(
    modalDialog(
      IC_projection_main_parameters_info[["text"]],
      title = IC_projection_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_projection_main_parameters_info <- list(
  title = "Main parameters for projection",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
    <li><b>Color range:</b> Color range of the plot</li>
    <li><b>Alpha:</b> Change spot transparency</li>
    <li><b>Select color:</b> Type of color scale of the plot</li>
    </ul>
    "
  )
)