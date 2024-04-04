##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Output_or_message"]] <- renderUI({

  fluidRow(
    column(width = 12,
           HTML("<b>Type of Data to add to the downloaded data :</b>"),
           checkboxInput("output_annotation_RDS", label = "Output Annotation table", value = TRUE),
           checkboxInput("output_UMAP_RDS", label = "Output UMAP + Clustering", value = TRUE),
           checkboxInput("output_markers_RDS", label = "Output marker genes", value = TRUE)
    )
  )
  
})

output[["Output_or_message_2"]] <- renderUI({
  
  fluidRow(
    column(width = 12,
           HTML("<b>Type of Data to subcluster :</b>"),
           uiOutput("subclustering_type"),
           uiOutput("subclustering_choice"),
           uiOutput("subclustering_automated")
    )
  )
  
})

##----------------------------------------------------------------------------##
## UI elements to choose de type of subclustering
##----------------------------------------------------------------------------##
output$subclustering_type <- renderUI({
  selectInput("export_sub_IC", label = "Choose type of subclustering", 
              choices = list("UMAP Cluster", "IC Cell types"), 
              selected = "UMAP Cluster")
})
  
  
output$subclustering_choice <- renderUI({
  req(input$export_sub_IC)
  if (input$export_sub_IC == "IC Cell types"){
    tagList(
      selectInput("Plot_display_type_density_manual", label = 'select method', 
                  choices = list("Manual", "Automated"), 
                  selected = "Manual")
    )
  } else if (input$export_sub_IC == "UMAP Cluster"){
    req(values$data)
    req(values$data@meta.data$seurat_clusters)
    tagList(
      selectInput("subclustering_metadata_export_choose", label = "Choose metadata to use for export",
                     choices = names(Filter(is.factor, values$data@meta.data)), selected = NULL),
      selectizeInput("subclustering_cluster_export_choose", label = "Choose cluster to export",
                     choices = NULL, selected = NULL, multiple = TRUE,
                     options = NULL)
    )
  }
})

observeEvent (input$subclustering_metadata_export_choose,{
  edit <- input$subclustering_metadata_export_choose
  updateSelectizeInput(session,"subclustering_cluster_export_choose", choices = sort(unique(values$data@meta.data[,input$subclustering_metadata_export_choose])))
})

output$subclustering_automated <- renderUI({
  if (req(input$Plot_display_type_density_manual == "Automated")){
    req(values$annotation_for_output)
    tagList(
      selectizeInput("Cell_type_subclustering_IC_export_choose", label = "Choose cell type to export",
                     choices = names(values$annotation_for_output[["Type"]]), selected = NULL, multiple = TRUE,
                     options = NULL),
      selectizeInput("Cell_type_subclustering_IC_def_export_choose", label = "Choose ICs to export",
                     choices = rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"], selected = NULL, multiple = TRUE,
                     options = NULL),
      numericInput("Cell_type_subclustering_density_export_choose", label = "standard deviation threshold", value = 1, step = 0.1)
    )
  }
})