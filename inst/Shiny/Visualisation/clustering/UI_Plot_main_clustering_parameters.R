##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["Plot_main_parameters_cluster_UI"]] <- renderUI({
  req(values$annotation_for_output)
  tagList(
    selectizeInput("Plot_cluster_type_UMAP_choice", label = "Choose cell type to cluster",
                   choices = unique(names(values$annotation_for_output[["Type"]])),
                   selected = NULL,
                   multiple = TRUE,
                   options = NULL),
    selectizeInput("Plot_cluster_IC_choice", label = "Choose IC to cluster",
                   choices = values$IC_names,
                   selected = input$Ic_list,
                   multiple = TRUE,
                   options = NULL),
    selectInput("select_algorithm_clusterisation", label = "Select algorithm", 
                choices = list("Louvain" = 1, "Louvain + multilevel refinement" = 2, "SLM" = 3, "Leiden" = 4), 
                selected = 1),
    numericInput("Clustering_resolution", "Clustering resolution", 1.2,
                 min = 0.1, max = 10, step = 0.1
    ),
    textInput("cluster_named_by_user", "Clustering name", value = "clustering_1")
  )
})

output[["start_cluster_UI"]] <- renderUI({
  tagList(
    actionButton("Select_all_ICs_cluster", "Select all ICs"),
    actionButton("start_cluster_plot", "Start Cluster")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["Plot_main_parameters_cluster_info"]], {
  showModal(
    modalDialog(
      Plot_main_parameters_cluster_info[["text"]],
      title = Plot_main_parameters_cluster_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
Plot_main_parameters_cluster_info <- list(
  title = "Main parameters for total gene heatmap",
  text = HTML("
    The elements in this panel allow you to clusterise samples.
    <ul>
      <li><b>Choose cell type to cluster:</b> Select ICs associated to the selected cell type for the clusterisation.</li>
      <li><b>Choose IC to cluster:</b> Select specific ICs for the clusterisation.</li>
      <li><b>Select algorithm:</b> Select the clustering algorithm.</li>
      <li><b>Clustering resolution:</b> Select the resolution of the clustering.</li>
      <li><b>Clustering name:</b> Name the clustering being carried out in order to be selected in display parameters.</li>
    </ul>
    "
  )
)