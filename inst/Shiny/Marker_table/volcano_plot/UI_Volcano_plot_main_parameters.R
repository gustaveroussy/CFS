##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["Volcano_plot_main_parameters_UI"]] <- renderUI({
  tagList(
    sliderInput("Volcano_plot_log_fold_change", "Log2 fold change",
                min = 0, max = round(max(c(abs(min(values$marker_gene[[(as.integer(input$marker_cluster_choice)+1)]]$avg_log2FC)),
                                           max(values$marker_gene[[(as.integer(input$marker_cluster_choice)+1)]]$avg_log2FC))),
                                     digit = 2),
                value = 0.6, step = 0.01),
    sliderInput("Volcano_plot_p_value", "P-value",
                min = 0, max = 1,
                value = 0.05, step = 0.01),
    sliderInput("Volcano_plot_alpha", "Alpha",
                min = 0, max = 1,
                value = 1, step = 0.01)
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["Volcano_plot_main_parameters_info"]], {
  showModal(
    modalDialog(
      Volcano_plot_main_parameters_info[["text"]],
      title = Volcano_plot_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
Volcano_plot_main_parameters_info <- list(
  title = "Main parameters for projection",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
    <li><b>Log2 fold change:</b> Select log2 fold change threshold (filters table)</li>
    <li><b>P-value:</b> Select p-value threshold  (filters table)</li>
    <li><b>Alpha:</b> Select plot transparency</li>
    </ul>
    "
  )
)