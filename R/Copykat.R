#' Ploidie search for cancer cells. (Non functionnal)
#'
#' Search for ploidie alteration within the sample.
#' @param data Seurat object to analyse
#' @param species ???
#' @param threads Number of threads to use
#' @param kcut ???
#' @param annotate chose whether or not to annotate the analysis, of return the Copykat object.
#' @param genome Genome to use for the analysis, either "hg20" or "mm"
#' @return A list of analysis object
#' @examples 
#' data <- Ploidie_search(data=data, species="S", threads=4, kcut=2, annotate=TRUE, genome="hg20")
#' @export
Ploidie_search=function(data=NULL, copykat.test = NULL, species="S", threads=4,kcut=2,annotate=TRUE,genome="hg20"){
  #copykat.test <- copykat(rawmat=as.matrix(data@assays$Spatial@counts), id.type=species, ngene.chr=5, win.size=25, KS.cut=0.1, sam.name="test", distance="euclidean", norm.cell.names="",output.seg="TRUE", plot.genes="TRUE", genome=genome,n.cores=threads)
  #saveRDS(copykat.test, file = "./copykat.test.RDS")
  gc()
  #predict aneuploid cells
  pred.test <- data.frame(copykat.test$prediction)
  pred.test <- pred.test[-which(pred.test$copykat.pred=="not.defined"),]  ##remove undefined cells
  CNA.test <- data.frame(copykat.test$CNAmat)
  
  
  my_palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 3, name = "RdBu")))(n = 999)
  
  chr <- as.numeric(CNA.test$chrom) %% 2+1
  rbPal1 <- colorRampPalette(c('black','grey'))
  CHR <- rbPal1(2)[as.numeric(chr)]
  chr1 <- cbind(CHR,CHR)
  
  rbPal5 <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = "Dark2")[2:1])
  com.preN <- pred.test$copykat.pred
  pred <- rbPal5(2)[as.numeric(factor(com.preN))]
  
  cells <- rbind(pred,pred)
  col_breaks = c(seq(-1,-0.4,length=50),seq(-0.4,-0.2,length=150),seq(-0.2,0.2,length=600),seq(0.2,0.4,length=150),seq(0.4, 1,length=50))
  Dist_CNA= parallelDist::parDist(t(CNA.test[!grepl("[a-z]" ,names(CNA.test))]),threads =threads, method = "euclidean")
  
  # Copykat copy heatmap for plotly
  paneuploid <-heatmap.3(t(CNA.test[!grepl("[a-z]" ,names(CNA.test))]),dendrogram="r", distfun = function(x){return(Dist_CNA)}, hclustfun = function(x) hclust(x, method="ward.D2"),
                         ColSideColors=chr1,RowSideColors=cells,Colv=NA, Rowv=TRUE,
                         notecol="black",col=my_palette,breaks=col_breaks, key=TRUE,
                         keysize=1, density.info="none", trace="none",
                         cexRow=0.1,cexCol=0.1,cex.main=1,cex.lab=0.1,
                         symm=F,symkey=F,symbreaks=T,cex=1, cex.main=4, margins=c(10,10))
  
  #
  
  laneuploid<-legend("topright", paste("pred.",names(table(com.preN)),sep=""), pch=15,col=RColorBrewer::brewer.pal(n = 8, name = "Dark2")[2:1], cex=0.6, bty="n")
  tmp<-merge(data.frame(Cells=colnames(data@assays$Spatial@counts)),data.frame(Cells=pred.test$cell.names,Aneuploid=pred.test$copykat.pred),all = T)
  Aneuploid<-tmp$Aneuploid
  names(Aneuploid)<-tmp$Cells
  data@meta.data$aneuploid<-as.factor(Aneuploid)
  
  
  #SubClone classification
  tumor.cells <- pred.test$cell.names[which(pred.test$copykat.pred=="aneuploid")]
  tumor.mat <- CNA.test %>% dplyr::select(one_of(gsub("-",".",tumor.cells)))
  
  hcc <- hclust(parallelDist::parDist(t(tumor.mat),threads =threads, method = "euclidean"), method = "ward.D2")
  hc.umap <- cutree(hcc,kcut)
  
  ######
  rbPal6 <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = "Dark2")[3:4])
  subpop <- rbPal6(2)[as.numeric(factor(hc.umap))]
  cells <- rbind(subpop,subpop)
  Dist_tumor.mat<-parallelDist::parDist(t(tumor.mat),threads =threads, method = "euclidean")
  # organise heatmap for subplot : heatmap plotly
  pSubClone<-heatmap.3(t(tumor.mat),dendrogram="r", distfun = function(x){return(Dist_tumor.mat)}, hclustfun = function(x) hclust(x, method="ward.D2"),
                       ColSideColors=chr1,RowSideColors=cells,Colv=NA, Rowv=TRUE,
                       notecol="black",col=my_palette,breaks=col_breaks, key=TRUE,
                       keysize=1, density.info="none", trace="none",
                       cexRow=0.1,cexCol=0.1,cex.main=1,cex.lab=0.1,
                       symm=F,symkey=F,symbreaks=T,cex=1, cex.main=4, margins=c(10,10))
  
  lSubClone<-legend("topright", c("c1","c2"), pch=15,col=RColorBrewer::brewer.pal(n = 8, name = "Dark2")[3:4], cex=0.9, bty='n')
  tmp<-merge(data.frame(Cells=colnames(data@assays$Spatial@counts)),data.frame(Cells=gsub("\\.","-",names(hc.umap)),SubClone=hc.umap),all=T)
  SubClone<-tmp$SubClone
  names(SubClone)<-tmp$Cells
  data@meta.data$SubClone<-as.factor(SubClone)
  pSubClone_spatial<-SpatialDimPlot(data,group.by="SubClone")
  pCount_spatial<-SpatialFeaturePlot(data,features="nCount_Spatial")
  tmp<-merge(data.frame(Cells=colnames(data@assays$Spatial@counts)),data.frame(Cells=gsub("\\.","-",names(hc.umap)),SubClone=hc.umap),all.y=T)
  pBoxSubCount<-data.frame(nCount=data$nCount_SCT[tmp$Cells],SubClone=hc.umap) %>% ggplot(aes(as.factor(SubClone),nCount)) +geom_boxplot(outlier.shape=NA) +geom_jitter(width=0.2,alpha=0.4)
  
  if (ncol(CNA.test[grepl("[a-z]" ,names(CNA.test))]) == 3) {
    cbind(CNA.test[grepl("[a-z]" ,names(CNA.test))],tumor.mat) %>% gather(key=Cells,value=LR, -chrom,-chrompos,-abspos) -> tumor.mat.s
  } else if (ncol(CNA.test[grepl("[a-z]" ,names(CNA.test))]) == 7) {
    cbind(CNA.test[grepl("[a-z]" ,names(CNA.test))],tumor.mat) %>% gather(key=Cells,value=LR, -abspos, -chromosome_name, -start_position, -end_position, -ensembl_gene_id, -mgi_symbol, -band) -> tumor.mat.s
  }
  
  #view of CNAs along crhomosomes
  pCNA_scater=data.frame(CNA.test[grepl("[a-z]" ,names(CNA.test))], Med= apply(tumor.mat[],1,function(x){median(abs(diff(x)))})) %>% ggplot(aes(chrompos,Med)) + geom_line() +facet_wrap(~chrom,scale="free_x")
  
  LR<-data.frame(CNA.test[grepl("[a-z]" ,names(CNA.test))], Med= apply(tumor.mat[],1,function(x){median(abs(diff(x)))}))
  Region_Amp_CNA<-LR %>% arrange(desc(Med)) %>% head(n=40)
  COPYKAT_FINAL = list(copykat.test = copykat.test,paneuploid = paneuploid,laneuploid = laneuploid,hcc = hcc,pSubClone = pSubClone,lSubClone = lSubClone,pSubClone_spatial = pSubClone_spatial,pCount_spatial = pCount_spatial,pBoxSubCount = pBoxSubCount,pCNA_scater = pCNA_scater,LR = LR,Region_Amp_CNA = Region_Amp_CNA)
  
  return(COPYKAT_FINAL)
}

