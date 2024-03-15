##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["Plot_type_UI"]] <- renderUI({
  tagList(
    selectInput("Plot_analysis_type", label = "Select method to use", 
                choices = list("UMAP", "3D UMAP","tSNE"),
                selected = "UMAP")
    )
})

output[["Plot_main_parameters_UI"]] <- renderUI({
  req(input$Plot_analysis_type)
  if (input$Plot_analysis_type == "UMAP"){
    req(values$annotation_for_output)
    tagList(
      textInput("reddim_named_by_user", "Dimred name", value = "umap"),
      selectizeInput("Plot_display_type_UMAP_choice", label = "Choose cell type for reduction",
                     choices = unique(names(values$annotation_for_output)),
                     selected = NULL,
                     multiple = TRUE,
                     options = NULL),
      selectizeInput("Plot_display_IC_choice", label = "Choose IC for reduction",
                     choices = values$IC_names,
                     selected = input$Ic_list,
                     multiple = TRUE,
                     options = NULL),
      numericInput("Plot_n.neighbors", "n.neighbors", 20,
                   min = 1, max = Inf, step = 1
      ),
      numericInput("Plot_min.dist", "min.dist", 0.1,
                   min = 0, max = Inf, step = 0.01
      ),
      numericInput("Plot_spread", "Spread", 3,
                   min = 0.1, step = 0.1
      )
    )
  } else if (input$Plot_analysis_type == "3D UMAP"){
    req(values$annotation_for_output)
    tagList(
      textInput("reddim_named_by_user", "Reduction name", value = "umap"),
      selectizeInput("Plot_display_type_UMAP_choice", label = "Choose cell type for reduction",
                     choices = unique(names(values$annotation_for_output)),
                     selected = NULL,
                     multiple = TRUE,
                     options = NULL),
      selectizeInput("Plot_display_IC_choice", label = "Choose IC for reduction",
                     choices = values$IC_names,
                     selected = input$Ic_list,
                     multiple = TRUE,
                     options = NULL),
      numericInput("Plot_n.neighbors", "n.neighbors", 20,
                   min = 1, max = Inf, step = 1
      ),
      numericInput("Plot_min.dist", "min.dist", 0.1,
                   min = 0, max = Inf, step = 0.01
      ),
      numericInput("Plot_spread", "Spread", 3,
                   min = 0.1, step = 0.1
      )
    )
  } else if (input$Plot_analysis_type == "tSNE") {
    req(values$annotation_for_output)
    tagList(
      textInput("reddim_named_by_user", "Reduction name", value = "tsne"),
      selectizeInput("Plot_display_type_UMAP_choice", label = "Choose cell type for reduction",
                     choices = unique(names(values$annotation_for_output)),
                     selected = NULL,
                     multiple = TRUE,
                     options = NULL),
      selectizeInput("Plot_display_IC_choice", label = "Choose IC for reduction",
                     choices = values$IC_names,
                     selected = input$Ic_list,
                     multiple = TRUE,
                     options = NULL),
      numericInput("Plot_perplexity", "Perplexity", 30,
                   min = 1, max = Inf, step = 1
      )
    )
  }
})

output[["start_plot_UI"]] <- renderUI({
  tagList(
    actionButton("Select_all_ICs_visualisation", "Select all ICs"),
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
      <li><b>Reduction name:</b> Name the reduction being generated to display in the visual options.</li>
      <li><b>Choose cell type to cluster:</b> Select ICs associated to the selected cell type for the reduction.</li>
      <li><b>Choose IC to cluster:</b> Select specific ICs for the reduction.</li>
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