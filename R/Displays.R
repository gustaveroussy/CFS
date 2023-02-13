Display_enrichment = function(data = NULL, dbs = c("GO_Biological_Process_2015"), IC = c('IC_1'), expression = 'none', n = 30){
  if (expression == 'positive'){
    expression = 'en_p'
  } else if (expression == 'negative') {
    expression = 'en_n'
  } else {
    expression = 'en'
  }
  table = data@misc[[IC]][[expression]][[dbs]]
  plotEnrich(table, showTerms = 30, numChar = 40, y = "Count", orderBy = "P.value",title=paste0("All gene functionnal enrichment for ",IC))
}

Display_topgenes_IC = function(data = NULL, IC = c('IC_1'), n = 10, palette = 'viridis'){
  GeneList <- data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>3))][[IC]]
  GeneList <- GeneList %>% as.tibble %>%arrange(desc(abs(Sig)))
  Gene <- data@reductions$ica@feature.loadings[GeneList$gene,][,IC]
  
  Gene_names <- names(Gene)
  
  z <- head(data@reductions$ica@feature.loadings[Gene_names,],n)
  
  paletteLength <- 256
  myColor <-  eval(parse(text=paste0(palette,"(n=256)")))
  
  myBreaks <- c(seq(min(z), 0, length.out=ceiling(paletteLength/2) + 1), 
                seq(max(z)/paletteLength, max(z), length.out=floor(paletteLength/2)))
  
  pheatmap(z,clustering_method = "ward.D",clustering_distance_cols = "correlation", color=myColor, breaks=myBreaks)
}

Display_topgenes_cells = function(data = NULL, IC = c('IC_1'), n = 10, palette = 'viridis'){
  GeneList <- data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>3))][[IC]]
  GeneList <- GeneList %>% as.tibble %>%arrange(desc(abs(Sig)))
  Gene <- data@reductions$ica@feature.loadings[GeneList$gene,][,IC]
  
  Gene_names <- names(Gene)
  
  z <- head(data@assays$SCT@scale.data[Gene_names,],n)
  
  paletteLength <- 256
  myColor <-  eval(parse(text=paste0(palette,"(n=256)")))
  
  myBreaks <- c(seq(min(z), 0, length.out=ceiling(paletteLength/2) + 1), 
                seq(max(z)/paletteLength, max(z), length.out=floor(paletteLength/2)))
  
  pheatmap(z,clustering_method = "ward.D",clustering_distance_cols = "correlation", color=myColor, breaks=myBreaks)
}

Display_spatial_ICA = function(data = NULL, IC = c('IC_1'), palette = 'viridis'){
  plot(as.raster(data@images$slice1@image))
  coordinates = GetTissueCoordinates(data)
}
