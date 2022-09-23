Show_IC_and_Enrich=function(data=NULL,Stat_data=NULL,dbs=c("GO_Biological_Process_2015"),nb_genes=10,ProjectName="Default"){
  print("Start printing")
  i = 1
  for (IC in names(Stat_data$Contrib_gene[names(which(Stat_data$Kurtosis_ICs>3))])){
    GeneList <- Stat_data$Contrib_gene[names(which(Stat_data$Kurtosis_ICs>3))][[IC]]
    dbs_size=c(1:length(dbs))
    GeneList <- GeneList %>% as.tibble %>%arrange(desc(abs(Sig)))
    if (websiteLive) {
      tryCatch(
        expr = {
          en=enrichr(GeneList$gene, dbs)
          en_p=enrichr(GeneList$gene[GeneList$Sig>0], dbs)
          en_n=enrichr(GeneList$gene[GeneList$Sig<0], dbs)
          data@misc[[IC]][["en"]] <- en
          data@misc[[IC]][["en_p"]] <- en_p
          data@misc[[IC]][["en_n"]] <- en_n
        },
        error = function(e){
          data@misc[[IC]][["en"]] <- NULL
          data@misc[[IC]][["en_p"]] <- NULL
          data@misc[[IC]][["en_n"]] <- NULL
        }
      ) 
    }
    # Get Ic weight per spot
    data@misc[[IC]][["IC_weight"]] <- data@reductions$ica@cell.embeddings[ , i]
    i = i+1
    
    # Get top genes
    data@misc[[IC]][["IC_top_genes"]] <- sort(abs(data@reductions$ica@feature.loadings[,IC]),decreasing = T) %>% names
    
    # get data IC/weight top genes
    L_IC = length(data@reductions$ica@feature.loadings[GeneList$gene])
    
    if (nb_genes<L_IC){
      data@misc[[IC]][["IC_top_genes_weight"]]=data@reductions$ica@feature.loadings[GeneList$gene[1:nb_genes],]
    }else{
      data@misc[[IC]][["IC_top_genes_weight"]]=data@reductions$ica@feature.loadings[GeneList$gene[1:L_IC],]
    }
    
    # heatmap spot/gene
    if (nb_genes<L_IC){
      data@misc[[IC]][["spot_top_genes_weight"]]=data@assays$SCT@scale.data[GeneList$gene[1:nb_genes],]
    }else{
      data@misc[[IC]][["spot_top_genes_weight"]]=data@assays$SCT@scale.data[GeneList$gene[1:L_IC],]
    }
  }
  
  return(data)
}