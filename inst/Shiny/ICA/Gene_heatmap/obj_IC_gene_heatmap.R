##----------------------------------------------------------------------------##
## gene list
##----------------------------------------------------------------------------##

GeneList_heatmap_IC <- reactive({
  req(values$data)
  req(input$IC_choice)
  IC_C = input[["IC_choice"]]
  data <- values$data
  if(!is.null(data@misc[["GeneAndStat"]][["kurtosis_value"]])){
    GeneList <- data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>(data@misc[["GeneAndStat"]][["kurtosis_value"]])))][[IC_C]]
  } else {
    GeneList <- data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>(3)))][[IC_C]]
  }
  GeneList <- GeneList %>% as_tibble %>%arrange(desc(abs(Sig)))
  if(length(GeneList$gene) == 1){
    Gene <- data@reductions$ica@feature.loadings[GeneList$gene,][IC_C]
    names(Gene) = GeneList$gene
  } else {
    Gene <- data@reductions$ica@feature.loadings[GeneList$gene,][,IC_C]
  }
  return(Gene)
})

table_ic_gene_to_return <- reactive({
  req(values$data)
  
  Gene_names <- names(GeneList_heatmap_IC())
  
  req(input$select_number_IC_gene_heatmap)
  
  if(length(Gene_names) == 1){
    z <- as.data.frame(head(values$data@reductions$ica@feature.loadings[Gene_names,],input$select_number_IC_gene_heatmap))
    z = t(z)
    rownames(z) = Gene_names
  }else{
    z <- head(values$data@reductions$ica@feature.loadings[Gene_names,],input$select_number_IC_gene_heatmap)
  }
  
  # gene order
  if(nrow(z) > 1){
    dist = dist(z, method = 'euclidean')
    y = hclust(dist, method = "ward.D")
    row_order <- y$labels[y$order]
    z <- z[row_order,]
  }
  
  # IC order
  if (input$IC_gene_column_organization == TRUE & nrow(z) > 1){
    dist = dist(t(z), method = 'euclidean')
    y = hclust(dist, method = "ward.D")
    col_order <- y$labels[y$order]
    z <- z[,col_order]
  }
  
  return(z)
})

min_max_gene_heatmap <- reactive({
  min = round(min(table_ic_gene_to_return()), digits = 0)
  max = round(max(table_ic_gene_to_return()), digits = 0)
  return(list(min = min, max = max))
})