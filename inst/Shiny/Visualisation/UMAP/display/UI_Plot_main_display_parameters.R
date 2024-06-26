##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["Plot_type_display_UI"]] <- renderUI({
  tagList(
    selectInput("Plot_analysis_display_type", label = "Select method to use", 
                choices = c("Dimentional reduction","Density","Scatter pie"),
                selected = "Dimentional reduction"),
    selectInput(
      "Visualisation_selected_dimred_to_display",
      "Reduction to Display",
      names(values$data@reductions)[!(names(values$data@reductions) %in% c("ica","pca"))],
      selected = names(values$data@reductions)[!(names(values$data@reductions) %in% c("ica","pca"))][1],
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  )
})

output[["Plot_main_parameters_display_UI"]] <- renderUI({
  tagList(
    selectInput("Plot_display_type", label = "Select what to color", 
                choices = unique(c("gene","IC",colnames(values$data@meta.data))),
                selected = if("seurat_clusters" %in% colnames(values$data@meta.data)){"seurat_clusters"}else{"gene"}),
    selectInput("select_color_visualisation_projection", label = "Select color", 
                choices = list("Magma" = "A", "Inferno" = "B", "Plasma" = "C", "Viridis" = "D", "Cividis" = "E", "Rocket" = "F", "Mako" = "G", "Turbo" = "H", "Blues", "Reds","YlGnBu","YlOrRd"), 
                selected = "D")
  )
})

output[["Plot_main_parameters_display_2_UI"]] <- renderUI({
  if(input$Plot_analysis_display_type == "Dimentional reduction"){
    if(input$Plot_display_type == "gene"){
      tagList(
        selectizeInput("gene_UMAP_choice", label = "Choose gene",
                       choices = rownames(GetAssay(values$data, assay = values$data@active.assay)),
                       selected = NULL,
                       multiple = FALSE,
                       options = NULL)
      )
    } else if (input$Plot_display_type == "IC"){
      tagList(
        selectizeInput("IC_UMAP_choice", label = "Choose IC",
                       choices = values$IC_names,
                       selected = NULL,
                       multiple = FALSE,
                       options = NULL)
      )
    }
  } else if (input$Plot_analysis_display_type == "Density") {
    req(values$annotation_for_output)
    tagList(
      selectizeInput("Plot_display_type_choice", label = "Choose cell type for density",
                     choices = unique(names(values$annotation_for_output[["Type"]])),
                     selected = NULL,
                     multiple = TRUE,
                     options = NULL),
      numericInput("Plot_thresh_density", label = "threshold", value = 0, min = 0, step = 0.1),
      numericInput("Plot_thresh_alpha_density", label = "alpha", value = 0.5, min = 0, max = 1, step = 0.1)
    )
  } else if (input$Plot_analysis_display_type == "Scatter pie") {
    req(values$annotation_for_output)
    tagList(
      selectInput("Scatter_pie_values_selected",
                     "Values for Scatterpie",
                     c("IC","Metadata"),
                     selected = "IC"),
      shinyWidgets::awesomeCheckbox(
        inputId = "Scatter_pie_size_activate",
        label = "Pie chart sized by signal intensity",
        value = TRUE
      )
    )
  }
})

output[["Plot_main_parameters_display_3_UI"]] <- renderUI({
  if(input$Plot_analysis_display_type == "Dimentional reduction"){
    if(input$Plot_display_type == "gene"){
      tagList(
        sliderInput("slider_visual_spatial_range", label = "Color range",
                    min = round(min(GetAssayData(values$data, assay = values$data@active.assay)[input$gene_UMAP_choice,]), digits = 2), 
                    max = round(max(GetAssayData(values$data, assay = values$data@active.assay)[input$gene_UMAP_choice,]), digits = 2),
                    value = c(round(min(GetAssayData(values$data, assay = values$data@active.assay)[input$gene_UMAP_choice,]),digits = 2),
                              round(max(GetAssayData(values$data, assay = values$data@active.assay)[input$gene_UMAP_choice,]), digits = 2)),
                    step = 0.01),
        radioButtons("transparency_visual_spatial_choice", label = "Alpha type",
                     choices = list("Constant" = 1, "Scaling" = 2), 
                     selected = 1),
        sliderInput("transparency_visual_spatial_range", "Alpha",
                    min = 0, max = 1,
                    value = 1, step = 0.01)
      )
    } else if (input$Plot_display_type == "IC"){
      req(input$IC_UMAP_choice)
      tagList(
        sliderInput("slider_visual_spatial_range", label = "Color range",
                    min = round(min(values$data@reductions$ica@cell.embeddings[, input$IC_UMAP_choice]), digits = 0), 
                    max = round(max(values$data@reductions$ica@cell.embeddings[, input$IC_UMAP_choice]), digits = 0),
                    value = c(round(min(values$data@reductions$ica@cell.embeddings[, input$IC_UMAP_choice]),digits = 0),
                              round(max(values$data@reductions$ica@cell.embeddings[, input$IC_UMAP_choice]), digits = 0)),
                    step = 0.01),
        radioButtons("transparency_visual_spatial_choice", label = "Alpha type",
                     choices = list("Constant" = 1, "Scaling" = 2), 
                     selected = 1),
        sliderInput("transparency_visual_spatial_range", "Alpha",
                    min = 0, max = 1,
                    value = 1, step = 0.01)
      )
    } else {
      if(is.null(values$data)){
        if (typeof(values$data@meta.data[[input$Plot_display_type]]) == "double" | grepl('nCount_|nFeature_|percent_', input$Plot_display_type)){
          tagList(
            sliderInput("slider_visual_spatial_range", label = "Color range",
                        min = round(min(values$data@meta.data[, input$Plot_display_type]), digits = 0), 
                        max = round(max(values$data@meta.data[, input$Plot_display_type]), digits = 0),
                        value = c(round(min(values$data@meta.data[, input$Plot_display_type]),digits = 0),
                                  round(max(values$data@meta.data[, input$Plot_display_type]), digits = 0)),
                        step = 0.01),
            radioButtons("transparency_visual_spatial_choice", label = "Alpha type",
                         choices = list("Constant" = 1, "Scaling" = 2), 
                         selected = 1),
            sliderInput("transparency_visual_spatial_range", "Alpha",
                        min = 0, max = 1,
                        value = 1, step = 0.01)
          )
        }
      } else {
        if (typeof(values$data@meta.data[[input$Plot_display_type]]) == "double" | grepl('nCount_|nFeature_|percent_', input$Plot_display_type)){
          tagList(
            sliderInput("slider_visual_spatial_range", label = "Color range",
                        min = round(min(values$data@meta.data[, input$Plot_display_type]), digits = 0), 
                        max = round(max(values$data@meta.data[, input$Plot_display_type]), digits = 0),
                        value = c(round(min(values$data@meta.data[, input$Plot_display_type]),digits = 0),
                                  round(max(values$data@meta.data[, input$Plot_display_type]), digits = 0)),
                        step = 0.01),
            radioButtons("transparency_visual_spatial_choice", label = "Alpha type",
                         choices = list("Constant" = 1, "Scaling" = 2), 
                         selected = 1),
            sliderInput("transparency_visual_spatial_range", "Alpha",
                        min = 0, max = 1,
                        value = 1, step = 0.01)
          )
        }
      }
    }
  } else if (input$Plot_analysis_display_type == "Scatter pie"){
    if(input$Scatter_pie_values_selected == "IC"){
      tagList(
        selectInput(
          "Visualisation_selected_dimred_to_display",
          "Reduction to Display",
          names(values$data@reductions)[!(names(values$data@reductions) %in% c("ica","pca"))],
          selected = names(values$data@reductions)[!(names(values$data@reductions) %in% c("ica","pca"))][1],
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectizeInput("Scatter_pie_cell_type", label = "choose cell type",
                       choices = unique(names(values$annotation_for_output[["Type"]])),
                       selected = NULL, multiple = TRUE, options = NULL),
        selectizeInput("Scatter_pie__IC_chosen_projection", label = "Choose IC to plot", choices = values$IC_names,
                       selected = NULL, multiple = TRUE,
                       options = NULL)
      )
    } else if (input$Scatter_pie_values_selected == "Metadata") {
      tagList(
        selectInput(
          "Visualisation_selected_dimred_to_display",
          "Reduction to Display",
          names(values$data@reductions)[!(names(values$data@reductions) %in% c("ica","pca"))],
          selected = names(values$data@reductions)[!(names(values$data@reductions) %in% c("ica","pca"))][1],
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectizeInput("Scatter_pie_metadata_select", label = "Choose metadata", choices = colnames(values$data@meta.data),
                       selected = NULL, multiple = TRUE,
                       options = NULL)
      )
    }
  }
})

output[["start_display_UI"]] <- renderUI({
  tagList(
    actionButton("start_display_UMAP", "Display UMAP"),
    actionButton("start_display_Spatial", "Display Spatial"),
    actionButton("start_display", "Display Both")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["Plot_main_parameters_display_info"]], {
  showModal(
    modalDialog(
      Plot_main_parameters_display_info[["text"]],
      title = Plot_main_parameters_display_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
Plot_main_parameters_display_info <- list(
  title = "Main parameters for total gene heatmap",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Select method to use:</b> Select here which projection you want to see.</li>
      <li><b>Dimentional Reduction:</b></li>
      <ul>
        <li><b>Reduction to Display:</b> Select which reduction to use for the scatter plot to display.</li>
        <li><b>Select what to color:</b> Select here which metadata to observe.</li>
        <li><b>Select color:</b> Select which colorscale to use for display.</li>
      </ul>
      <li><b>Density:</b></li>
      <ul>
        <li><b>Reduction to Display:</b> Select which reduction to use for the scatter plot to display.</li>
        <li><b>Select what to color:</b> Select here which metadata to observe.</li>
        <li><b>Select color:</b> Select which colorscale to use for display.</li>
        <li><b>Choose cell type for density:</b> Select which cell type density to display.</li>
        <li><b>threshold:</b> Select the threshold for which a density isn't considered as part of the selected cell type anymore.</li>
        <li><b>Alpha:</b> select how see-through spot are represented as.</li>
      </ul>
      <li><b>Scatter pie:</b></li>
      <ul>
        <li><b>Reduction to Display:</b> Select which reduction to use for the scatter plot to display.</li>
        <li><b>Values for Scatterpie:</b> Select what values the scatterpie should represent.</li>
        <li><b>Pie chart sized by signal intensity:</b> Change the size of pie charts based on signal intensity.</li>
        <li><b>Reduction to Display:</b> Select which reduction to use for scatter plot display.</li>
        <li><b>choose cell type:</b> Select which ICs signal related to a specific cell type will be used for the scatterpies.</li>
        <li><b>Choose IC to plot:</b> Select specific ICs to represent in the scatterpies.</li>
      </ul>
    </ul>
    "
  )
)