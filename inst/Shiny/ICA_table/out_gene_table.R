output[["ICA_genes_table"]] <- renderDT({
  req(values$data)
  req(values$data@reductions$ica)
  DT = values$data@reductions$ica@feature.loadings
  datatable(DT, class = 'cell-border stripe', options = list(scrollX = T), filter = "top")
})