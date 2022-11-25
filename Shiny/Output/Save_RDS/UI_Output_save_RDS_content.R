##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Output_or_message"]] <- renderUI({

  fluidRow(
    column(width = 4,
           HTML("<b>Type of Data to add to the downloaded data :</b>"),
           checkboxInput("output_annotation_RDS", label = "Output Annotation table", value = TRUE),
           checkboxInput("output_UMAP_RDS", label = "Output UMAP + Clustering", value = TRUE)
    ),
    column(width = 8,
           HTML("<b>Type of Data to subcluster :</b>"),
           uiOutput("subclustering_type"),
           uiOutput("subclustering_choice")
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
      selectizeInput("Cell_type_subclustering_IC_export_choose", label = "Choose cell type to export",
                     choices = unique(as.vector(values$Annotation[,'Type']))[!NA %in% unique(as.vector(values$Annotation[,'Type']))], selected = NULL, multiple = TRUE,
                     options = NULL)
    )
  } else if (input$export_sub_IC == "UMAP Cluster"){
    tagList(
      selectizeInput("subclustering_cluster_export_choose", label = "Choose cluster to export",
                     choices = sort(unique(values$data@meta.data$seurat_clusters)), selected = NULL, multiple = TRUE,
                     options = NULL)
    )
  }
})