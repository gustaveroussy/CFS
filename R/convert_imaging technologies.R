#' Create_vizgen_seurat
#'
#' Convert vizgen data using grid based method
#' @param table table of transcripts
#' @param pixel_format size of the binning (in px)
#' @param min.features minimum number of transcripts to keep per spot.
#' @param technology One of "Merfish" or "CosMX".
#' 
#' @return Seurat object corresponding to the binned vizgen data
#' 
#' @examples 
#' data <- Create_vizgen_seurat(spot_gene_expression=spot_table, pixel_format = 40, min.features = 5)
#' 
#' @export
Convert_imaging_seurat=function(table=NULL, pixel_format = 40, min.features = 5, technology = NULL){
  try(
    if(technology == "CosMX"){
      seurat.object = Create_CosMX_seurat(tx=table, pixel_format = pixel_format, min.features = min.features)
    } else if (technology == "Merfish"){
      seurat.object = Create_vizgen_seurat(spot_gene_expression=table, pixel_format = pixel_format, min.features = min.features)
    } else {
      stop('technology is not one of "Merfish" or "CosMX"')
    }
  )
  
  return(seurat.object)
}
