##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["trajectory_main_parameters_UI"]] <- renderUI({
  tagList(
    selectInput("trajectory_dimension_type", label = "Select dimension to use", 
                choices = list("2D", "3D"), 
                selected = "2D"),
    selectInput("trajectory_color_by", label = "Select how to color", 
                choices = list("Clustering"), 
                selected = "Clustering"),
    selectizeInput("trajectory_dimension_choice", label = "Choose dimension to spatial plot",
                   choices = c("dm","Branch",if(!is.null(values$data@misc$dpt)){colnames(values$data@misc$dpt@dm@eigenvectors)}),
                   selected = NULL,
                   multiple = FALSE,
                   options = NULL),
    actionButton("start_plot_trajectory", "Start plot")
    )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["trajectory_main_parameters_info"]], {
  showModal(
    modalDialog(
      trajectory_main_parameters_info[["text"]],
      title = trajectory_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
trajectory_main_parameters_info <- list(
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