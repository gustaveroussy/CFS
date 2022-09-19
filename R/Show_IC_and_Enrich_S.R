Show_IC_and_Enrich_S=function(data=NULL,IC="IC_1",GeneList=NULL,dbs=c( "GO_Biological_Process_2015"),nb_genes=10,ProjectName="Default"){
  print("Start printing")
  
    DirIC=paste0(ProjectName)
    dir.create(DirIC, recursive=FALSE)
  
  dbs_size=c(1:length(dbs))
  GeneList <- GeneList %>% as.tibble %>%arrange(desc(abs(Sig)))
  
  pdf(paste0(DirIC,"/FEA_",IC,".pdf"),width=15,height=7) 
  if (websiteLive) {
    tryCatch(
      expr = {
        en=enrichr(GeneList$gene, dbs)
        for(i in dbs_size){
          print(i)
          print( plotEnrich(en[[i]], showTerms = 30, numChar = 40, y = "Count", orderBy = "P.value",title=paste0("All gene functionnal enrichment for ",IC)))
        }
      },
      error = function(e){
        p<- ggplot() +  theme_void()
      }
    ) 
    
    tryCatch(
      expr = {
        en_p=enrichr(GeneList$gene[GeneList$Sig>0], dbs)
        for(i in dbs_size){
          print( plotEnrich(enriched_p[[i]], showTerms = 30, numChar = 40, y = "Count", orderBy = "P.value",title=paste0("Positive gene functionnal enrichment for ",IC)))
        }
      },
      error = function(e){
        p<- ggplot() +  theme_void()
      }
    ) 
    
    tryCatch(
      expr = {
        en_n=enrichr(GeneList$gene[GeneList$Sig<0], dbs)
        
        for(i in dbs_size){
          print(plotEnrich(enriched_n[[i]], showTerms = 30, numChar = 40, y = "Count", orderBy = "P.value",title=paste0("Negative gene functionnal enrichment for ",IC)))
        }
      },
      error = function(e){
        p<-  ggplot() +  theme_void()
      }
    ) 
  }
  dev.off()
  
  # Plot le poids des IC en spatial
  message("Plotting Spatial")     
  pdf(paste0(DirIC,"/Spatial_",IC,".pdf"),width=10,height=10)    
  p0<-SpatialFeaturePlot(data,crop = T,pt.size.factor = 1.2,features = IC,stroke=0)
  p<-SpatialFeaturePlot(data,crop = T,pt.size.factor = 1.2,features = IC,stroke=0,alpha=c(0,1))
  print(p0 )# + p0)
  
  dev.off()
  
  # plot les gènes pour chaques IC dans l'espace
  message("Plotting Spatial genes")
  pdf(paste0(DirIC,"/First",nb_genes,"_Gene_contr_",IC,".pdf"),width=20,height=20)  
  sort(abs(data@reductions$ica@feature.loadings[,IC]),decreasing = T) %>% names -> nICq
  p5<-SpatialFeaturePlot(data,crop = T,pt.size.factor = 1.2,features = nICq[1:nb_genes],ncol = round(nb_genes/4),stroke=0)    
  print(p5)
  dev.off()
  
  L_IC = length(data@reductions$ica@feature.loadings[GeneList$gene])
  # prepare le heatmap
  paletteLength <- 50
  myColor <- colorRampPalette(c("violet","black","yellow"))(paletteLength)
  
  if (nb_genes<L_IC){
    data_heat=data@reductions$ica@feature.loadings[GeneList$gene[1:nb_genes],]
  }else{
    data_heat=data@reductions$ica@feature.loadings[GeneList$gene[1:L_IC],]
  }
  
  
  myBreaks <- c(seq(min(data_heat), 0, length.out=ceiling(paletteLength/2) + 1), 
                seq(max(data_heat)/paletteLength, max(data_heat), length.out=floor(paletteLength/2)))
  
  # Plot the heatmap des 10 gènes les plus importants par IC
  
  pheatmap(data_heat,clustering_method = "ward.D",color=myColor, breaks=myBreaks,,clustering_distance_cols = "correlation",filename=paste0(DirIC,"/Heatmap_loadings_gene_",IC,".pdf"))
  
  #prepare le heatmap
  if (nb_genes<L_IC){
    data_heat=data@assays$SCT@scale.data[GeneList$gene[1:nb_genes],]
  }else{
    data_heat=data@assays$SCT@scale.data[GeneList$gene[1:L_IC],]
  }
  
  
  myBreaks <- c(seq(min(data_heat), 0, length.out=ceiling(paletteLength/2) + 1), 
                seq(max(data_heat)/paletteLength, max(data_heat), length.out=floor(paletteLength/2)))
  # Plot the heatmap des gènes par IC pour chaques cellules du single cell
  pheatmap(data_heat,clustering_method = "ward.D",color=myColor, breaks=myBreaks,clustering_distance_cols = "correlation",filename=paste0(DirIC,"/Heatmap_Exp_",IC,".pdf"))
  
  
}
