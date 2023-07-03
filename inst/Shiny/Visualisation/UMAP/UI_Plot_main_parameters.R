##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["Plot_type_UI"]] <- renderUI({
  tagList(
    selectInput("Plot_analysis_type", label = "Select method to use", 
                choices = list("UMAP","Density","Scatter pie"),
                selected = "UMAP")
    )
})

output[["Plot_main_parameters_UI"]] <- renderUI({
  req(input$Plot_analysis_type)
  if (input$Plot_analysis_type == "UMAP"){
    req(values$annotation_for_output)
    tagList(
      selectInput("Plot_display_type", label = "Select what to color", 
                  choices = if(!is.null(values$UMAP)){unique(c("seurat_clusters","gene","IC",colnames(values$UMAP@meta.data)))}else{unique(c("seurat_clusters","gene",colnames(values$data@meta.data)))}, 
                  selected = "seurat_clusters"),
      selectInput("select_color_visualisation_projection", label = "Select color", 
                  choices = list("Viridis", "Blues", "Reds","YlGnBu","YlOrRd","Range"), 
                  selected = "Viridis"),
      selectizeInput("Plot_display_type_UMAP_choice", label = "Choose cell type to plot",
                     choices = unique(names(values$annotation_for_output)),
                     selected = NULL,
                     multiple = TRUE,
                     options = NULL),
      selectizeInput("Plot_display_IC_choice", label = "Choose IC to plot",
                     choices = values$IC_names,
                     selected = input$Ic_list,
                     multiple = TRUE,
                     options = NULL),
      numericInput("Plot_resolution", "Plot resolution", 1.2,
                   min = 0.1, max = 2, step = 0.1
      ),
      numericInput("Plot_spread", "Spread", 3,
                   min = 0.1, step = 0.1
      )
    )
  } else if (input$Plot_analysis_type == "Density") {
    req(values$annotation_for_output)
    tagList(
      selectizeInput("Plot_display_type_choice", label = "Choose cell type to plot",
                     choices = unique(names(values$annotation_for_output)),
                     selected = NULL,
                     multiple = TRUE,
                     options = NULL),
      checkboxInput("Plot_contour_density", label = "Contour", value = FALSE),
      checkboxInput("Plot_show_image_density", label = "Display image", value = TRUE),
      numericInput("Plot_thresh_density", label = "threshold", value = 0.3, min = 0, step = 0.1),
      numericInput("Plot_thresh_alpha_density", label = "alpha", value = 0.5, min = 0, max = 1, step = 0.1)
    )
  } else if (input$Plot_analysis_type == "Scatter pie") {
    req(values$annotation_for_output)
    tagList(
      selectizeInput("Scatter_pie_cell_type", label = "choose cell type",
                     choices = unique(names(values$annotation_for_output)),
                     selected = NULL, multiple = TRUE, options = NULL),
      selectizeInput("Scatter_pie__IC_chosen_projection", label = "Choose IC to plot", choices = values$IC_names,
                     selected = NULL, multiple = TRUE,
                     options = NULL)
    )
  }
})

output[["Plot_main_parameters_2_UI"]] <- renderUI({
  if(input$Plot_display_type == "gene"){
    tagList(
      selectizeInput("gene_UMAP_choice", label = "Choose gene",
                     choices = rownames(values$data@assays$SCT@scale.data),
                     selected = NULL,
                     multiple = FALSE,
                     options = NULL)
    )
  } else if (input$Plot_display_type == "IC"){
    selectizeInput("IC_UMAP_choice", label = "Choose ICs",
                   choices = values$IC_names,
                   selected = NULL,
                   multiple = FALSE,
                   options = NULL)
  }
})

output[["start_plot_UI"]] <- renderUI({
  tagList(
    actionButton("Select_all_ICs_visualisation", "Select all ICs"),
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