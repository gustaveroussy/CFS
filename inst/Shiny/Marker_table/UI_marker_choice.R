output[["marker_cluster_choice_UI"]] <- renderUI({
  req(values$data)
  selectInput(
    "marker_cluster_choice",
    label = "Choose cluster marker gene",
    choices = names(values$marker_gene),
    width = '100%'
  )
})