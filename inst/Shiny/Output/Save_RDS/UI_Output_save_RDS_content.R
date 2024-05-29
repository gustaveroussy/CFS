##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Output_or_message"]] <- renderUI({

  fluidRow(
    column(width = 12,
           HTML("<b>Type of Data to add to the downloaded data :</b>"),
           checkboxInput("output_annotation_RDS", label = "Output Annotation table", value = TRUE),
           checkboxInput("output_distance_RDS", label = "Output distances", value = TRUE),
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
              choices = list("Metadata", "IC annotation", "Manual Selection"), 
              selected = "Metadata")
})
  
  
output$subclustering_choice <- renderUI({
  req(input$export_sub_IC)
  if (input$export_sub_IC == "Metadata"){
    req(values$data)
    req(values$data@meta.data)
    tagList(
      selectInput("subclustering_metadata_export_choose", label = "Choose metadata to use for export",
                     choices = names(Filter(is.factor, values$data@meta.data)), selected = NULL)
    )
  } else if (input$export_sub_IC == "IC annotation"){
    selectInput("Annotation_type_subclustering_export_choose", label = "Choose Annotation type to use for extraction",
                   choices = colnames(values$Annotation), selected = NULL)
  }
})

output$subclustering_automated <- renderUI({
  req(input$export_sub_IC)
  if (input$export_sub_IC == "IC annotation"){
    req(input$Annotation_type_subclustering_export_choose)
    tagList(
      selectizeInput("Cell_type_subclustering_IC_export_choose", label = "Choose cell type to export",
                     choices = unique(values$Annotation[,input$Annotation_type_subclustering_export_choose]), selected = NULL, multiple = TRUE,
                     options = NULL),
      selectizeInput("Cell_type_subclustering_IC_def_export_choose", label = "Choose ICs to export",
                     choices = rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"], selected = NULL, multiple = TRUE,
                     options = NULL),
      numericInput("Cell_type_subclustering_density_export_choose", label = "standard deviation threshold", value = 1, step = 0.1)
    )
  } else if (input$export_sub_IC == "Metadata"){
    req(input$subclustering_metadata_export_choose)
    tagList(
      selectizeInput("subclustering_cluster_export_choose", label = "Choose cluster to export",
                     choices = sort(unique(values$data@meta.data[,input$subclustering_metadata_export_choose])), selected = NULL, multiple = TRUE,
                     options = NULL)
    )
  }
})