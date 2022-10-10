DoHeatmapICA_S=function(data="NULL",nics="IC_1",GeneStatICA="NULL",ngenes=10,ProjectName="Default"){
  list_gene <-  purrr::map(GeneStatICA$Contrib_gene[nics],function(.x){x<-.x %>% arrange(desc(abs(Sig))) %>% head(n=ngenes) ;return(x$gene)}) %>% unlist %>% unique 
  
  paletteLength <- 50
  myColor <- colorRampPalette(c("violet","black","yellow"))(paletteLength)
  data_heat=data@reductions$ica@feature.loadings[as.matrix(list_gene),]
  
  myBreaks <- c(seq(min(data_heat), 0, length.out=ceiling(paletteLength/2) + 1), 
                seq(max(data_heat)/paletteLength, max(data_heat), length.out=floor(paletteLength/2)))
  
  # Plot the heatmap
  
  pheatmap(data_heat,clustering_method = "ward.D",color=myColor, breaks=myBreaks,,clustering_distance_cols = "correlation",filename=paste0("../Heatmap_loadings_gene_All_IC_best,",ngenes,".pdf"),width = 15,height=30)
  return(data_heat)
}
