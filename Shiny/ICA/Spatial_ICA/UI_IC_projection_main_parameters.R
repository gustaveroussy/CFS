##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output$plot_ICA_confirm <- renderUI({
  checkboxInput("select_plot_ICA_spatial", label = "Plot", value = TRUE)
})

output$pie_chart_confirm <- renderUI({
  checkboxInput("pie_plot", label = "Scatter Pie", value = FALSE)
})

observeEvent(input$IC_choice, {
  updateSliderInput(session, "slider_IC_spatial_range", label = "Color range",
                    min = round(min(Launch_analysis()@misc[[input$IC_choice]]$IC_weight), digits = 0), 
                    max = round(max(Launch_analysis()@misc[[input$IC_choice]]$IC_weight), digits = 0),
                    value = c(round(min(Launch_analysis()@misc[[input$IC_choice]]$IC_weight),digits = 0),
                              round(max(Launch_analysis()@misc[[input$IC_choice]]$IC_weight), digits = 0)))
})

observeEvent(input$pie_plot, {
  if (input$pie_plot == FALSE) {
    updateSliderInput(session, "slider_IC_spatial_range", label = "Color range",
                      min = round(min(Launch_analysis()@misc[[input$IC_choice]]$IC_weight), digits = 0), 
                      max = round(max(Launch_analysis()@misc[[input$IC_choice]]$IC_weight), digits = 0),
                      value = c(round(min(Launch_analysis()@misc[[input$IC_choice]]$IC_weight),digits = 0),
                                round(max(Launch_analysis()@misc[[input$IC_choice]]$IC_weight), digits = 0)))
  } else if (input$pie_plot == TRUE) {
    updateSliderInput(session, "slider_IC_spatial_range", label = "Color range",
                      min = 0, 
                      max = 1,
                      value = c(0,1)
                      )
  }
})

output$pie_chart_check <- renderUI({
  if (input$pie_plot == FALSE){
    tagList(
      sliderInput("slider_IC_spatial_range", label = "Color range",step = 1, min = 0,
                  max = 1, value = c(0,1)),
      
      selectInput("select_color_IC_projection", label = "Select color", 
                  choices = list("Viridis", "Blues", "Reds","YlGnBu","YlOrRd","Range"), 
                  selected = "Viridis")
    )
  } else {
    tagList(
      numericInput(
        "pieplot_size",
        label = "Pie size",
        value = 50,
        min = 1,
        max = 1000,
        step = 1
      )
    )
  }
})

output$select_all_input_control <- renderUI({
  if (input$pie_plot == TRUE){
    tagList(
      selectizeInput("All_IC_chosen_projection", label = "Choose IC to plot", choices = names(Launch_analysis()@misc)[-1],
                     selected = NULL, multiple = TRUE,
                     options = NULL),
      actionButton("start_pieplot", "Start plot")
    )
  }
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
    <li><b>Plot:</b> This option allows to plot the IC over the spatial data</li>
    <li><b>Scatter pie:</b> This option allows to plot a scatter pie of multiple ICs at once</li>
    <li><b>Color range:</b> Color range of the plot</li>
    <li><b>Select color:</b> Type of color scale of the plot</li>
    </ul>
    "
  )
)