##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["Plot_type_display_UI"]] <- renderUI({
  tagList(
    selectInput("Plot_analysis_display_type", label = "Select method to use", 
                choices = list("UMAP","tSNE","Density","Scatter pie"),
                selected = "UMAP")
  )
})

output[["Plot_main_parameters_display_UI"]] <- renderUI({
  if(!(input$Plot_analysis_display_type == "Scatter pie")){
    tagList(
      selectInput("Plot_display_type", label = "Select what to color", 
                  choices = if(!is.null(values$UMAP)){unique(c("gene","IC",colnames(values$UMAP@meta.data)))}else{unique(c("gene","IC",colnames(values$data@meta.data)))},
                  selected = if("seurat_clusters" %in% colnames(values$UMAP@meta.data)){"seurat_clusters"}else{"gene"}),
      selectInput("select_color_visualisation_projection", label = "Select color", 
                  choices = list("Magma" = "A", "Inferno" = "B", "Plasma" = "C", "Viridis" = "D", "Cividis" = "E", "Rocket" = "F", "Mako" = "G", "Turbo" = "H", "Blues", "Reds","YlGnBu","YlOrRd"), 
                  selected = "D")
    )
  }
})

output[["Plot_main_parameters_display_2_UI"]] <- renderUI({
  if(input$Plot_analysis_display_type == "UMAP" || input$Plot_analysis_display_type == "tSNE"){
    if(input$Plot_display_type == "gene"){
      tagList(
        selectizeInput("gene_UMAP_choice", label = "Choose gene",
                       choices = rownames(values$data@assays$SCT@scale.data),
                       selected = NULL,
                       multiple = FALSE,
                       options = NULL)
      )
    } else if (input$Plot_display_type == "IC"){
      tagList(
        selectizeInput("IC_UMAP_choice", label = "Choose ICs",
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
                     choices = unique(names(values$annotation_for_output)),
                     selected = NULL,
                     multiple = TRUE,
                     options = NULL),
      numericInput("Plot_thresh_density", label = "threshold", value = 0.3, min = 0, step = 0.1),
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
  if(input$Plot_analysis_display_type == "UMAP" || input$Plot_analysis_display_type == "tSNE"){
    if(input$Plot_display_type == "gene"){
      tagList(
        sliderInput("slider_visual_spatial_range", label = "Color range",
                    min = round(min(values$data@assays$SCT@scale.data[input$gene_UMAP_choice,]), digits = 0), 
                    max = round(max(values$data@assays$SCT@scale.data[input$gene_UMAP_choice,]), digits = 0),
                    value = c(round(min(values$data@assays$SCT@scale.data[input$gene_UMAP_choice,]),digits = 0),
                              round(max(values$data@assays$SCT@scale.data[input$gene_UMAP_choice,]), digits = 0)),
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
      if(is.null(values$UMAP)){
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
        if (typeof(values$UMAP@meta.data[[input$Plot_display_type]]) == "double" | grepl('nCount_|nFeature_|percent_', input$Plot_display_type)){
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
        selectizeInput("Scatter_pie_cell_type", label = "choose cell type",
                       choices = unique(names(values$annotation_for_output)),
                       selected = NULL, multiple = TRUE, options = NULL),
        selectizeInput("Scatter_pie__IC_chosen_projection", label = "Choose IC to plot", choices = values$IC_names,
                       selected = NULL, multiple = TRUE,
                       options = NULL)
      )
    } else if (input$Scatter_pie_values_selected == "Metadata") {
      tagList(
        selectizeInput("Scatter_pie_metadata_select", label = "Choose metadata", choices = colnames(values$UMAP@meta.data),
                       selected = NULL, multiple = TRUE,
                       options = NULL)
      )
    }
  }
})

output[["start_display_UI"]] <- renderUI({
  tagList(
    actionButton("start_display", "Display")
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