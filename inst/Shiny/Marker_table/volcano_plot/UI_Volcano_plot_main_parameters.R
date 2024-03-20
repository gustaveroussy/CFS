##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["Volcano_plot_main_parameters_1_UI"]] <- renderUI({
  req(values$marker_gene)
  if(!is.null(values$marker_gene)){
    tagList(
      sliderInput("Volcano_plot_log_fold_change", "Log2 fold change",
                  min = 0, max = round(max(c(abs(min(values$marker_gene[[input$marker_cluster_choice]]$avg_log2FC)),
                                             max(values$marker_gene[[input$marker_cluster_choice]]$avg_log2FC))),
                                       digit = 2),
                  value = 0.6, step = 0.01)
    )
  }
})

output[["Volcano_plot_main_parameters_2_UI"]] <- renderUI({
  if(!is.null(values$data)){
    tagList(
            selectizeInput('volcano_plot_clusters_to_compare', "Clusters to compare",
                     unique(values$data@meta.data[,input$volcano_plot_clusters_list_to_compare])[order(unique(values$data@meta.data[,input$volcano_plot_clusters_list_to_compare]))], selected = NULL, multiple = TRUE,
                     options = NULL),
      numericInput(
        "Volcano_plot_p_value", "P-value",
        0.05,
        min = 0,
        max = 1,
        step = 0.01
      ),
      numericInput(
        "Volcano_plot_top_gene", "Top genes",
        10,
        min = 0,
        step = 1
      ),
      sliderInput("Volcano_plot_alpha", "Alpha",
                  min = 0, max = 1,
                  value = 1, step = 0.01),
      actionButton("start_marker", "Start")
    )
  }
})

output[["Volcano_plot_main_parameters_3_UI"]] <- renderUI({
  tagList(
    selectInput('volcano_plot_clusters_list_to_compare', "Markers to display",
                names(Filter(is.factor, values$data@meta.data)),
                selected = names(Filter(is.factor, values$data@meta.data))[1], multiple = FALSE),
    selectInput('clustering_method_volcano_plot', "Analysis method",
                c("wilcox", "wilcox_limma", "bimod","roc","t", "negbinom", "poisson", "LR", "MAST", "DESeq2"),
                selected = "wilcox", multiple = FALSE)
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