#' enrich IC
#'
#' Search and enrich IC genes using EnrichR. Result saved in misc$IC_X$en
#' 
#' @return datatables of enrichment intro the seurat object
#' 
#' @examples 
#' data = Enrich_ICA(data=data,dbs=c("GO_Biological_Process_2015"), kurtosis = 3, overwrite = TRUE)
#' 
#' @export
Enrich_ICA=function(data=NULL, dbs=c("GO_Biological_Process_2015"), kurtosis = 3, overwrite = TRUE){
  
  #check if online
  websiteLive <- getOption("enrichR.live")
  if (websiteLive) {
    listEnrichrSites()
    setEnrichrSite("Enrichr") # Human genes   
  }
  
  if (overwrite == TRUE){
    data@misc = data@misc[!(grepl('IC_', names(data@misc)))]
  }
  
  print("Start printing")
  if(!is.null(dbs)){
    for (IC in names(data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>kurtosis))])){
      if(!(IC %in% names(data@misc))){
        GeneList <- data@misc$GeneAndStat$Contrib_gene[[IC]]
        GeneList <- GeneList %>% as_tibble %>%arrange(desc(abs(Sig)))
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
  }
  return(data)
}
