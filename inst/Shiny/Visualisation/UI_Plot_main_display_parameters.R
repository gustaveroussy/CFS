##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

check_visual_tab = reactiveValues(slider_visual_spatial_range_ui_check = F,
                                  select_color_visualisation_projection_ui_check = F,
                                  Scatter_pie_metadata_select_ui_check = F,
                                  Scatter_pie_cell_type_ui_check = F,
                                  Plot_display_type_choice_ui_check = F
)

output[["Plot_type_display_UI"]] <- renderUI({
  
  req(values$data)
  div(id = "plot_type_display_ui_type",
    tagList(
      selectInput("Plot_analysis_display_type", label = "Select method to use", 
                  choices = c("Dimentional reduction","Density","Scatter pie"),
                  selected = "Dimentional reduction"),
      selectInput(
        "Visualisation_selected_dimred_to_display",
        "Reduction to Display",
        names(values$data@reductions)[!(names(values$data@reductions) %in% c("ica","pca",names(values$data@misc$reduction_names)))],
        selected = names(values$data@reductions)[!(names(values$data@reductions) %in% c("ica","pca",names(values$data@misc$reduction_names)))][1],
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    )
  )
})

observeEvent(input$Plot_analysis_display_type,{
  if(input$Plot_analysis_display_type == "Dimentional reduction"){
    
    ##### remove reduction
    
    shinyjs::hide(id = "Plot_display_type_choice_ui")
    
    shinyjs::hide(id = "Plot_thresh_density_ui")
    
    shinyjs::hide(id = "Plot_thresh_alpha_density_ui")
    
    shinyjs::hide(id = "Scatter_pie_values_selected_ui")
    
    shinyjs::hide(id = "Scatter_pie_size_activate_ui")
    
    shinyjs::hide(id = "select_color_visualisation_projection_density_ui")
    
    shinyjs::hide(id = "Scatter_pie_metadata_select_ui")
    shinyjs::hide(id = "Scatter_pie_cell_type_ui")
    
    ##### add reduction
    
    if(is.null(input$Plot_display_type)){
      insertUI(
        selector = "#plot_type_display_ui_type",
        ui = div(id = "Plot_display_type_ui", selectInput("Plot_display_type", label = "Select what to color", 
                                                          choices = unique(c("metadata","gene",names(values$data@reductions)[!(names(values$data@reductions) %in% c("umap","tsne"))])),
                                                          selected = "metadata"))
      )
    } else {
      shinyjs::show(id = "Plot_display_type_ui")
    }
    
  } else if (input$Plot_analysis_display_type == "Density"){
    ##### remove density
    
    shinyjs::hide(id = "Plot_display_type_ui")
    
    shinyjs::hide(id = "what_to_display_UMAP_choice_ui")
    
    shinyjs::hide(id = "Scatter_pie_values_selected_ui")
    
    shinyjs::hide(id = "Scatter_pie_size_activate_ui")
    
    shinyjs::hide(id = "select_color_visualisation_projection_ui")
    
    shinyjs::hide(id = "slider_visual_spatial_range_ui")
    
    shinyjs::hide(id = "Scatter_pie_metadata_select_ui")
    shinyjs::hide(id = "Scatter_pie_cell_type_ui")
    
    ##### add density
    if(is.null(input$Plot_display_type)){
      insertUI(
        selector = "#plot_type_display_ui_type",
        ui = div(id = "Plot_display_type_ui", selectInput("Plot_display_type", label = "Select what to color", 
                         choices = unique(c("gene","IC",unique(c("gene","IC","metadata")))),
                         selected = "metadata"))
      )
    } else {
      shinyjs::show(id = "Plot_display_type_ui")
    }
    
    if(is.null(input$select_color_visualisation_projection)){
      insertUI(
        selector = "#Plot_display_type_ui",
        ui = div(id = "select_color_visualisation_projection_density_ui", selectInput("select_color_visualisation_projection_density", label = "Select color", 
                         choices = list("Magma" = "A", "Inferno" = "B", "Plasma" = "C", "Viridis" = "D", "Cividis" = "E", "Rocket" = "F", "Mako" = "G", "Turbo" = "H", "Blues", "Reds","YlGnBu","YlOrRd"), 
                         selected = "D"))
      )
    } else {
      shinyjs::show(id = "select_color_visualisation_projection_density_ui")
    }
    
    req(values$annotation_for_output)
    
    if(!(check_visual_tab$Plot_display_type_choice_ui_check)){
      
      insertUI(
        selector = "#plot_type_display_ui_type",
        ui = div(id = "Plot_display_type_choice_ui", selectizeInput("Plot_display_type_choice", label = "Choose cell type for density",
                                                                                 choices = unique(names(values$annotation_for_output[["Type"]])),
                                                                                 selected = NULL,
                                                                                 multiple = TRUE,
                                                                                 options = NULL)
                 )
      )
      
      check_visual_tab$Plot_display_type_choice_ui_check = TRUE
      
    } else {
      shinyjs::show(id = "Plot_display_type_choice_ui")
    }
    
    if(is.null(input$Plot_thresh_density)){
      
      insertUI(
        selector = "#plot_type_display_ui_type",
        ui = div(id = "Plot_thresh_density_ui", numericInput("Plot_thresh_density", label = "threshold", value = 0, min = 0, step = 0.1)
        )
      )
      
    } else {
      shinyjs::show(id = "Plot_thresh_density_ui")
    }
    
    if(is.null(input$Plot_thresh_alpha_density)){
      
      insertUI(
        selector = "#plot_type_display_ui_type",
        ui = div(id = "Plot_thresh_alpha_density_ui", numericInput("Plot_thresh_alpha_density", label = "alpha", value = 0.5, min = 0, max = 1, step = 0.1)
        )
      )
      
    } else {
      shinyjs::show(id = "Plot_thresh_alpha_density_ui")
    }
    
    
    
  } else if (input$Plot_analysis_display_type == "Scatter pie"){
    ##### remove scatter pie
    
    shinyjs::hide(id = "Plot_display_type_ui")
    
    shinyjs::hide(id = "select_color_visualisation_projection_ui")
    
    shinyjs::hide(id = "Plot_display_type_choice_ui")
    
    shinyjs::hide(id = "Plot_thresh_density_ui")
    
    shinyjs::hide(id = "Plot_thresh_alpha_density_ui")
    
    shinyjs::hide(id = "what_to_display_UMAP_choice_ui")
    
    shinyjs::hide(id = "select_color_visualisation_projection_density_ui")
    
    shinyjs::hide(id = "slider_visual_spatial_range_ui")
    
    ##### add scatter pie
    
    req(values$annotation_for_output)
    
    if(is.null(input$Scatter_pie_values_selected)){
      
      insertUI(
        selector = "#plot_type_display_ui_type",
        ui = div(id = "Scatter_pie_values_selected_ui", selectInput("Scatter_pie_values_selected",
                                                                  "Values for Scatterpie",
                                                                  c("IC","Metadata"),
                                                                  selected = "IC")
        )
      )
      
    } else {
      shinyjs::show(id = "Scatter_pie_values_selected_ui")
    }
      
      
    if(is.null(input$Scatter_pie_size_activate)){
      
      insertUI(
        selector = "#plot_type_display_ui_type",
        ui = div(id = "Scatter_pie_size_activate_ui", shinyWidgets::awesomeCheckbox(
                                                                        inputId = "Scatter_pie_size_activate",
                                                                        label = "Pie chart sized by signal intensity",
                                                                        value = TRUE
                                                                      )
        )
      )
      
    } else {
      shinyjs::show(id = "Scatter_pie_size_activate_ui")
    }
  }
})


observeEvent(input$Plot_display_type,{
  req(input$Plot_display_type)
  
  if(is.null(input$what_to_display_UMAP_choice)){
    insertUI(
      selector = "#Plot_display_type_ui",
      ui = div(id = "what_to_display_UMAP_choice_ui", selectizeInput("what_to_display_UMAP_choice", label = paste0("Choose ",input$Plot_display_type),
                                                          choices = if(input$Plot_display_type == "ica"){colnames(values$data@reductions$ica@cell.embeddings)} else if (input$Plot_display_type == "gene"){rownames(GetAssay(values$data, assay = values$data@active.assay))} else if (input$Plot_display_type == "metadata"){colnames(values$data@meta.data)} else if (input$Plot_display_type %in% names(values$data@misc$reduction_names)){values$data@misc$reduction_names[[input$Plot_display_type]]},
                                                          selected = NULL,
                                                          multiple = FALSE,
                                                          options = NULL)
      )
    )
  } else {
    shinyjs::show(id = "what_to_display_UMAP_choice_ui")
    updateSelectInput(session,"what_to_display_UMAP_choice",
                      label = paste0("Choose ",input$Plot_display_type),
                      choices = if(input$Plot_display_type == "ica"){colnames(values$data@reductions$ica@cell.embeddings)} else if (input$Plot_display_type == "gene"){rownames(GetAssay(values$data, assay = values$data@active.assay))} else if (input$Plot_display_type == "metadata"){colnames(values$data@meta.data)} else if (input$Plot_display_type %in% names(values$data@misc$reduction_names)){values$data@misc$reduction_names[[input$Plot_display_type]]}
                      )
  }
})



observeEvent(input$what_to_display_UMAP_choice,{
  req(input$what_to_display_UMAP_choice)
  
  if(!(input$Plot_display_type == "metadata" && !is.numeric(values$data@meta.data[, input$what_to_display_UMAP_choice]))){
    
    if(!(check_visual_tab$select_color_visualisation_projection_ui_check)){
      insertUI(
        selector = "#what_to_display_UMAP_choice_ui",
        ui = div(id = "select_color_visualisation_projection_ui", selectInput("select_color_visualisation_projection", label = "Select color", 
                                                                              choices = list("Magma" = "A", "Inferno" = "B", "Plasma" = "C", "Viridis" = "D", "Cividis" = "E", "Rocket" = "F", "Mako" = "G", "Turbo" = "H", "Blues", "Reds","YlGnBu","YlOrRd"), 
                                                                              selected = "D"))
      )
      
      check_visual_tab$select_color_visualisation_projection_ui_check = TRUE
      
    } else {
        shinyjs::show(id = "select_color_visualisation_projection_ui")
    }
    
    
    #slide values
    min = if(input$Plot_display_type == "gene") {round(min(GetAssayData(values$data, assay = values$data@active.assay)[input$what_to_display_UMAP_choice,]), digits = 2)} else if (input$Plot_display_type == "ica") {round(min(values$data@reductions$ica@cell.embeddings[, input$what_to_display_UMAP_choice]), digits = 0)} else if (input$Plot_display_type == "metadata") {round(min(values$data@meta.data[, input$what_to_display_UMAP_choice]), digits = 0)} else if (input$Plot_display_type %in% names(values$data@misc$reduction_names)){round(min(values$data@reductions[[input$Plot_display_type]]@cell.embeddings[, which(values$data@misc$reduction_names[[input$Plot_display_type]] == input$what_to_display_UMAP_choice)]), digits = 0)}
    max = if(input$Plot_display_type == "gene") {round(max(GetAssayData(values$data, assay = values$data@active.assay)[input$what_to_display_UMAP_choice,]), digits = 2)} else if (input$Plot_display_type == "ica") {round(max(values$data@reductions$ica@cell.embeddings[, input$what_to_display_UMAP_choice]), digits = 0)} else if (input$Plot_display_type == "metadata") {round(max(values$data@meta.data[, input$what_to_display_UMAP_choice]), digits = 0)} else if (input$Plot_display_type %in% names(values$data@misc$reduction_names)){round(max(values$data@reductions[[input$Plot_display_type]]@cell.embeddings[, which(values$data@misc$reduction_names[[input$Plot_display_type]] == input$what_to_display_UMAP_choice)]), digits = 0)}
    
    if(!(check_visual_tab$slider_visual_spatial_range_ui_check)){
      insertUI(
        selector = "#select_color_visualisation_projection_ui",
        ui = div(id = "slider_visual_spatial_range_ui", sliderInput("slider_visual_spatial_range", label = "Color range",
                                                                    min = min, 
                                                                    max = max,
                                                                    value = c(min,max),
                                                                    step = 0.01),
                 radioButtons("transparency_visual_spatial_choice", label = "Alpha type",
                              choices = list("Constant" = 1, "Scaling" = 2), 
                              selected = 1),
                 sliderInput("transparency_visual_spatial_range", "Alpha",
                             min = 0, max = 1,
                             value = 1, step = 0.01)
        )
      )
      
      check_visual_tab$slider_visual_spatial_range_ui_check = TRUE
      
    } else {
      shinyjs::show(id = "slider_visual_spatial_range_ui")
      updateSliderInput(session,"slider_visual_spatial_range", label = "Color range",
                        min = min, 
                        max = max,
                        value = c(min,max),
                        step = 0.01
      )
    }
    
  } else {
    
    shinyjs::hide(id = "slider_visual_spatial_range_ui")
    shinyjs::hide(id = "select_color_visualisation_projection_ui")
    
  }
  
  
  
})


observeEvent(input$Scatter_pie_values_selected,{
  if(input$Scatter_pie_values_selected == "IC"){
    
    shinyjs::hide(id = "Scatter_pie_metadata_select_ui")
    
    if(!(check_visual_tab$Scatter_pie_cell_type_ui_check)){
      
      insertUI(
        selector = "#plot_type_display_ui_type",
        ui = div(id = "Scatter_pie_cell_type_ui",
                 selectizeInput("Scatter_pie_cell_type", label = "choose cell type",
                                choices = unique(names(values$annotation_for_output[["Type"]])),
                                selected = NULL, multiple = TRUE, options = NULL),
                 selectizeInput("Scatter_pie_IC_chosen_projection", label = "Choose IC to plot", choices = values$IC_names,
                                selected = NULL, multiple = TRUE,
                                options = NULL)
                 
        )
      )
      
      check_visual_tab$Scatter_pie_cell_type_ui_check = TRUE
      
    } else {
      
      shinyjs::show(id = "Scatter_pie_cell_type_ui")
    }
    
  } else if (input$Scatter_pie_values_selected == "Metadata") {
    
    shinyjs::hide(id = "Scatter_pie_cell_type_ui")
    
    if(!(check_visual_tab$Scatter_pie_metadata_select_ui_check)){
      
      insertUI(
        selector = "#plot_type_display_ui_type",
        ui = div(id = "Scatter_pie_metadata_select_ui",
        selectizeInput("Scatter_pie_metadata_select", label = "Choose metadata", choices = colnames(values$data@meta.data),
                       selected = NULL, multiple = TRUE,
                       options = NULL)
        
        )
      )
      
      check_visual_tab$Scatter_pie_metadata_select_ui_check = TRUE
      
    } else {
      shinyjs::show(id = "Scatter_pie_metadata_select_ui")
    }
    
  }
})

output[["start_display_UI"]] <- renderUI({
  tagList(
    actionButton("start_display_UMAP", "Display UMAP"),
    actionButton("start_display_Spatial", "Display Spatial")
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