output[["ICA_genes_table"]] <- renderDT({
  req(data@reductions$ica)
  DT = data@reductions$ica@feature.loadings
  datatable(DT, class = 'cell-border stripe', options = list(scrollX = T), filter = "top")
})