##----------------------------------------------------------------------------##
## Action of the select all ICs
##----------------------------------------------------------------------------##

observeEvent(input$Select_all_ICs_visualisation, {
  if (input$Plot_analysis_type == "UMAP" || input$Plot_analysis_type == "3D UMAP" || input$Plot_analysis_type == "tSNE"){
    updateSelectizeInput(session,"Plot_display_IC_choice", selected = rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"])
  } else if (input$Plot_analysis_type == "Scatter pie") {
    updateSelectizeInput(session,"Scatter_pie__IC_chosen_projection", selected = rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"])
  }
})

observeEvent(input$Select_all_ICs_cluster, {
  updateSelectizeInput(session,"Plot_cluster_IC_choice", selected = rownames(values$Annotation)[values$Annotation[,"Use"] == "TRUE"])
})
