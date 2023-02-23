##----------------------------------------------------------------------------##
## gene list
##----------------------------------------------------------------------------##

GeneList_heatmap_IC <- reactive({
  req(values$data)
  req(input$IC_choice)
  IC_C = input[["IC_choice"]]
  data <- values$data
  GeneList <- data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>3))][[IC_C]]
  GeneList <- GeneList %>% as_tibble %>%arrange(desc(abs(Sig)))
  Gene <- data@reductions$ica@feature.loadings[GeneList$gene,][,IC_C]
  return(Gene)
})

table_ic_gene_to_return <- reactive({
  req(values$data)
  
  Gene_names <- names(GeneList_heatmap_IC())
  
  req(input$select_number_IC_gene_heatmap)
  
  z <- head(values$data@reductions$ica@feature.loadings[Gene_names,],input$select_number_IC_gene_heatmap)
  
  p <- pheatmap(z,clustering_method = "ward.D",clustering_distance_cols = "correlation")
  
  row_order <- p[["tree_row"]][["order"]]
  z <- z[row_order,]
  
  if (input$IC_gene_column_organization == TRUE){
    col_order <- p[["tree_col"]][["order"]]
    z <- z[,col_order]
  }
  
  # only keep IC of interest
  
  # z <- z[,names(Launch_analysis()@misc)[startsWith(names(Launch_analysis()@misc), "IC_")]]
  
  return(z)
})