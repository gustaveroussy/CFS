########################################
# reactive of the image to plot by plotly
########################################

base_palette <- reactive({
  palette = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4',
              '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff',
              '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1',
              '#000075', '#808080', '#000000')
  return(palette)
})

palette <- reactive({
  palette = base_palette()
  req(values$data)
  req(values$data@meta.data[["seurat_clusters"]])
  repet = ceiling((length(summary(values$data@meta.data[["seurat_clusters"]]))-1)/length(palette))
  palette = rep(palette,repet)
  return(palette)
})