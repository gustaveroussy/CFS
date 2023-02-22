Display_enrichment = function(data = NULL, dbs = c("GO_Biological_Process_2015"), IC = c('IC_1'), expression = 'none', n = 30){
  if (expression == 'positive'){
    expression = 'en_p'
  } else if (expression == 'negative') {
    expression = 'en_n'
  } else {
    expression = 'en'
  }
  table = data@misc[[IC]][[expression]][[dbs]]
  plotEnrich(table, showTerms = n, numChar = 40, y = "Count", orderBy = "P.value",title=paste0("All gene functionnal enrichment for ",IC))
}

Display_topgenes_IC = function(data = NULL, IC = c('IC_1'),clustering_method = "ward.D", n = 10, palette = 'viridis'){
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

Display_topgenes_cells = function(data = NULL, IC = c('IC_1'),clustering_method = "ward.D", n = 10, palette = 'viridis'){
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
  image = rasterGrob(data@images$slice1@image, interpolate=TRUE)
  coordinates = GetTissueCoordinates(data)
  color = pmax(data@reductions[["ica"]]@cell.embeddings[,IC],0)
  ggplot(coordinates) + annotation_custom(image, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    geom_point(aes(x = imagecol, y = -imagerow, color = color)) +
    scale_color_gradientn(colours = eval(parse(text=paste0(palette,"(n=256)")))) +
    scale_x_continuous(expand=c(0, 0), limits=c(0, dim(data@images$slice1@image)[2])) +
    scale_y_continuous(expand=c(0, 0), limits=c(-dim(data@images$slice1@image)[1], 0)) +
    coord_fixed()
  #SpatialFeaturePlot(data, features = "IC_1")
}

Display_spatial_gene = function(data = NULL, gene = NULL, palette = 'viridis'){
  image = rasterGrob(data@images$slice1@image, interpolate=TRUE)
  coordinates = GetTissueCoordinates(data)
  color = data@assays$SCT@scale.data[gene,]
  ggplot(coordinates) + annotation_custom(image, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    geom_point(aes(x = imagecol, y = -imagerow, color = color)) +
    scale_color_gradientn(colours = eval(parse(text=paste0(palette,"(n=256)")))) +
    scale_x_continuous(expand=c(0, 0), limits=c(0, dim(data@images$slice1@image)[2])) +
    scale_y_continuous(expand=c(0, 0), limits=c(-dim(data@images$slice1@image)[1], 0)) +
    coord_fixed()
  #SpatialFeaturePlot(data, features = "IC_1")
}
