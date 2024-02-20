##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["Plot_type_UI"]] <- renderUI({
  tagList(
    selectInput("Plot_analysis_type", label = "Select method to use", 
                choices = list("UMAP","tSNE"),
                selected = "UMAP")
    )
})

output[["Plot_main_parameters_UI"]] <- renderUI({
  req(input$Plot_analysis_type)
  if (input$Plot_analysis_type == "UMAP"){
    req(values$annotation_for_output)
    tagList(
      textInput("reddim_named_by_user", "Clustering name", value = "umap"),
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
      numericInput("Plot_n.neighbors", "n.neighbors", 30,
                   min = 1, max = Inf, step = 1
      ),
      numericInput("Plot_min.dist", "min.dist", 0.3,
                   min = 0, max = Inf, step = 0.01
      ),
      numericInput("Plot_spread", "Spread", 3,
                   min = 0.1, step = 0.1
      )
    )
  } else if (input$Plot_analysis_type == "tSNE") {
    req(values$annotation_for_output)
    tagList(
      textInput("reddim_named_by_user", "Clustering name", value = "tsne"),
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
      <li><b>Select method to use:</b> Select here which projection you want to see in the scatter plot on the right.</li>
      <li><b>UMAP:</b> UMAP display, if data have already been generated, leaving it empty will reagenerate the previous one.</li>
      <ul>
        <li><b>Select what to color:</b> Select which category, to use to color the spots.</li>
        <li><b>Choose cell type to plot:</b> Select which IC associated with cell types from annotation to use for UMAP generation.</li>
        <li><b>Choose IC to plot:</b> Select which IC to use for UMAP generation.</li>
        <li><b>Enter Plot resolution:</b> Select clustering resolution to use.</li>
        <li><b>Spread:</b> Select clustering resolution to use.</li>
      </ul>
      <li><b>Density:</b></li>
      <ul>
        <li><b>Choose cell type to plot:</b> Select which IC associated with cell types from annotation to use for density generation.</li>
        <li><b>Contour:</b> Only display density area limits.</li>
        <li><b>Display image:</b> Display the histological image under the density display.</li>
        <li><b>threshold:</b> Select the limits of density display.</li>
        <li><b>alpha:</b> Select the density display transparency.</li>
      </ul>
      <li><b>Scatter pie:</b></li>
      <ul>
        <li><b>Choose cell type to plot:</b> Select which IC associated with cell types from annotation to use for density generation.</li>
        <li><b>Choose IC to plot:</b> Select which IC to use for density generation.</li>
      </ul>
    </ul>
    "
  )
)