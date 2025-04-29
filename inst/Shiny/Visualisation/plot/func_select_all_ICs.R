##----------------------------------------------------------------------------##
## Action of the select all ICs
##----------------------------------------------------------------------------##

observeEvent(input$Select_all_ICs_visualisation, {
  if (input$Plot_analysis_type == "UMAP" || input$Plot_analysis_type == "3D UMAP" || input$Plot_analysis_type == "tSNE"){
    updateSelectizeInput(session,"Plot_display_IC_choice", selected = if(input$Plot_umap_reduction_to_use == "ica"){rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"]} else {values$data@misc$reduction_names[[input$Plot_umap_reduction_to_use]]})
  } else if (input$Plot_analysis_type == "Scatter pie") {
    updateSelectizeInput(session,"Scatter_pie__IC_chosen_projection", selected = rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"])
  }
})

observeEvent(input$Select_all_ICs_cluster, {
  if(input$Plot_cluster_reduction_to_use == "ica"){
    updateSelectizeInput(session,"Plot_cluster_IC_choice", selected = rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"])
  } else {
    updateSelectizeInput(session,"Plot_cluster_IC_choice", selected = Plot_umap_reduction_to_use)
  }
})
