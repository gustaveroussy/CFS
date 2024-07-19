#' CFS light save
#'
#' Create a Seurat object in a lighter format for visualization with the Shiny tool
#' @param data Seurat object
#' @param assay assay to keep for visualization
#' 
#' @return Light Seurat object for visualization with the shiny tool
#' 
#' @examples 
#' light_data <- CFS_light_save(data=NULL, assay = 'SCT')
#' 
#' @export
CFS_light_save=function(data=NULL, assay = 'SCT'){
  
  if ( is.null(data@misc$GeneAndStat) ) {
    stop(
      "Light save requires ica reduction.",
      call. = FALSE
    )
  }
  
  if ( is.null(data@assays[[assay]]) ) {
    stop(
      "Assay doesn't exist.",
      call. = FALSE
    )
  }
  
  export <- subset(data, features = data@misc$GeneAndStat$All_Contrib_Gene)
  
  for(i in names(data@assays)[!(names(data@assays) %in% assay)]){
    export@assays[[i]] = NULL
  }
  
  export@assays[[assay]]@scale.data = matrix()
  export@assays[[assay]]@counts = matrix()
  
  return(export)
}
