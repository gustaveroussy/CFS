##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##
observeEvent(input[["plot_export"]], {
  req(values$data)
  ## open dialog to select where plot should be saved and how the file should
  ## be named
  
  available_storage_volumes <- getVolumes()
  
  ## open dialog to select where plot should be saved and how the file should
  ## be named
  shinyFiles::shinyFileSave(
    input,
    id = "plot_export",
    roots = available_storage_volumes,
    session = session,
    restrictions = system.file(package = "base")
  )
  
  ## retrieve info from dialog
  save_file_input <- shinyFiles::parseSavePath(
    available_storage_volumes,
    input[["plot_export"]]
  )
  
  ## only proceed if a path has been provided
  req(nrow(save_file_input) > 0)
  ## ggplot2 functions are necessary to create the plot
  # require("reticulate")
  ## extract specified file path
  save_file_path <- as.character(save_file_input$datapath[1])
  ##
  ## check if selection projection consists of 2 or 3 dimensions
  ## ... selection projection consists of 2 dimensions
  
  if (input$Plot_analysis_display_type == "Dimentional reduction"){
    plot = current_plot_umap()
  } else if (input$Plot_analysis_display_type == "Dimentional reduction" & input$Visualisation_selected_dimred_to_display == "tsne") {
    plot = current_plot_tSNE()
  } else if (input$Plot_analysis_display_type == "Density") {
    req(input$Plot_display_type_choice)
    if(input$Spatial_use_ggplot){
      #plots$density = current_plot_density()
    } else {
      plot = current_plot_density()
    }
  } else if (input$Plot_analysis_display_type == "Scatter pie") {
    req(values$data)
    plot = current_plot_scatter_pie()
  }
  
  ## save plot
  
  save_image(plot, save_file_path, width = input$plot_export_width, height = input$plot_export_height, scale = input$plot_export_scale)
  
  ## check if file was succesfully saved
  ## ... successful
  if ( file.exists(save_file_path) ) {
    ## give positive message
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Success!",
      text = paste0("Heatmap saved successfully as: ", save_file_path),
      type = "success"
    )
    ## ... failed
  } else {
    ## give negative message
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error!",
      text = "Sorry, it seems something went wrong...",
      type = "error"
    )
  }
  ## ... selection projection consists of 3 dimensions
})