##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["Plot_type_UI"]] <- renderUI({
  tagList(
    selectInput("Plot_analysis_type", label = "Select method to use", 
                choices = list("UMAP","Density"),
                selected = "UMAP")
    )
})

output[["Plot_main_parameters_UI"]] <- renderUI({
  if (input$Plot_analysis_type == "UMAP"){
    tagList(
      selectInput("Plot_display_type", label = "Select what to color", 
                  choices = list("Clustering", "Ploïdie"), 
                  selected = "Clustering"),
      selectizeInput("Plot_display_IC_choice", label = "Choose IC to plot",
                     choices = values$IC_names,
                     selected = input$Ic_list,
                     multiple = TRUE,
                     options = NULL),
      numericInput("Plot_resolution", "Enter Plot resolution", 1.2,
                   min = 0.1, max = 2, step = 0.1
      )
    )
  } else if (input$Plot_analysis_type == "Density") {
    tagList(
      selectizeInput("Plot_display_type_choice", label = "Choose IC to plot",
                     choices = unique(names(values$annotation_for_output)),
                     selected = NULL,
                     multiple = TRUE,
                     options = NULL),
      checkboxInput("Plot_contour_density", label = "Contour", value = FALSE),
      checkboxInput("Plot_show_image_density", label = "Display image", value = TRUE),
      numericInput("Plot_thresh_density", label = "threshold", value = 0.3, min = 0, step = 0.1),
      numericInput("Plot_thresh_alpha_density", label = "alpha", value = 0.5, min = 0, max = 1, step = 0.1)
    )
  }
})

output[["start_plot_UI"]] <- renderUI({
  tagList(
    actionButton("start_plot", "Start plot")
  )
})


##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["Plot_main_parameters_info"]], {
  showModal(
    modalDialog(
      Plot_main_parameters_info[["text"]],
      title = Plot_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
Plot_main_parameters_info <- list(
  title = "Main parameters for total gene heatmap",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li>
      <li><b>Color cells by:</b> Select which variable, categorical or continuous, from the meta data should be used to color the cells.</li>
    </ul>
    "
  )
)