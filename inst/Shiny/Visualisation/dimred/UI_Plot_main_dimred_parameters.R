##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["Plot_type_UI"]] <- renderUI({
  tagList(
    selectInput("Plot_umap_reduction_to_use", label = "Select reduction", 
                choices = names(values$data@reductions)[!(names(values$data@reductions) %in% c("umap","tsne"))],
                selected = "ica"),
    selectInput("Plot_analysis_type", label = "Select method to use", 
                choices = list("UMAP", "3D UMAP","tSNE"),
                selected = "UMAP")
    )
})

output[["Plot_main_parameters_UI"]] <- renderUI({
    req(input$Plot_analysis_type)
    req(values$annotation_for_output)
    tagList(
      textInput("reddim_named_by_user", "Dimred name", value = "umap"),
      if(input$Plot_umap_reduction_to_use == "ica"){
        selectizeInput("Plot_display_type_UMAP_choice", label = "Choose annotation for reduction",
                       choices = unique(names(values$annotation_for_output[["Type"]])),
                       selected = NULL,
                       multiple = TRUE,
                       options = NULL)
      },
      selectizeInput("Plot_display_IC_choice", label = "Choose values for reduction",
                     choices = if(input$Plot_umap_reduction_to_use == "ica"){values$IC_names} else {values$data@misc$reduction_names[[input$Plot_umap_reduction_to_use]]},
                     selected = if(input$Plot_umap_reduction_to_use == "ica"){input$Ic_list} else {NULL},
                     multiple = TRUE,
                     options = NULL),
      if (input$Plot_analysis_type == "UMAP" | input$Plot_analysis_type == "3D UMAP"){
        numericInput("Plot_n.neighbors", "n.neighbors", 20,
                     min = 1, max = Inf, step = 1
      )},
      if (input$Plot_analysis_type == "UMAP" | input$Plot_analysis_type == "3D UMAP"){
        numericInput("Plot_min.dist", "min.dist", 0.1,
                     min = 0, max = Inf, step = 0.01
      )},
      if (input$Plot_analysis_type == "UMAP" | input$Plot_analysis_type == "3D UMAP"){
        numericInput("Plot_spread", "Spread", 3,
                     min = 0.1, step = 0.1
      )},
      if (input$Plot_analysis_type == "tSNE") {
        numericInput("Plot_perplexity", "Perplexity", 30,
                     min = 1, max = Inf, step = 1
        )
      }
      
    )
})

output[["start_plot_UI"]] <- renderUI({
  tagList(
    actionButton("Select_all_ICs_visualisation", "Select all"),
    actionButton("start_UMAP", "Calculate DimRed")
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
      <li><b>Select method to use:</b> Select which reduction you want to create from the data.</li>
      <li><b>Dimred name:</b> Name the reduction being generated to display in the visual options.</li>
      <li><b>Choose annotation for reduction:</b> Select ICs associated to the selected annotation for the reduction.</li>
      <li><b>Choose IC for reduction:</b> Select specific ICs for the reduction.</li>
      <li><b>UMAP/3D UMAP:</b></li>
      <ul>
        <li><b>n.neighbors:</b> Number of neighbor for graph construction</li>
        <li><b>min.dist:</b> Controls how tightly the embedding is allowed compress points together.</li>
        <li><b>Spread:</b> Determines how clustered/clumped the embedded points are.</li>
      </ul>
      <li><b>tSNE:</b></li>
      <ul>
        <li><b>Perplexity:</b> Perplexity of the result</li>
      </ul>
    </ul>
    "
  )
)
