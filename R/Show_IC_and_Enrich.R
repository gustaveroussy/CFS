#' enrich IC
#'
#' Search and enrich IC genes using EnrichR. Result saved in misc$IC_X$en
#' 
#' @return datatables of enrichment intro the seurat object
#' 
#' @examples 
#' data = Show_IC_and_Enrich(data=data,dbs=c("GO_Biological_Process_2015"))
#' 
#' @export
Show_IC_and_Enrich=function(data=NULL,dbs=c("GO_Biological_Process_2015")){
  print("Start printing")
  for (IC in names(data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>3))])){
    GeneList <- data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>3))][[IC]]
    dbs_size=c(1:length(dbs))
    GeneList <- GeneList %>% as_tibble %>%arrange(desc(abs(Sig)))
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
  }
  return(data)
}
