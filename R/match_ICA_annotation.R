#' Match_Annotation
#'
#' Annotate a sample by matching the annotation with another sample IC annotations
#' @param data_to_annotate Seurat object containing a non annotated ICA reduction
#' @param annotated_data Seurat object containing an annotated ICA reduction
#' 
#' @return Seurat object data_to_annotate with annotation table
#' 
#' @examples 
#' data_1 <- Match_Annotation=function(data_to_annotate=data_1, annotated_data = data_2)
#' 
#' @export
Match_Annotation=function(data_to_annotate=NULL, annotated_data = NULL){

  loadings_non_annotated <- data_to_annotate@reductions$ica@feature.loadings
  
  loadings_annotated <- annotated_data@reductions$ica@feature.loadings
  
  loadings_annotated = loadings_annotated[rownames(loadings_annotated) %in% rownames(loadings_non_annotated),]
  loadings_non_annotated = loadings_non_annotated[rownames(loadings_non_annotated) %in% rownames(loadings_annotated),]
  
  cor <- cor(x = loadings_annotated, y = loadings_non_annotated)
  
  annotation_data = annotated_data@misc$annotation
  
  equiv <- c()
  for (IC in colnames(loadings_non_annotated)) {
    equiv[[IC]] <- sort(abs(cor[rownames(annotation_data),IC]), decreasing = T)[1] %>% as.data.frame() %>%  rownames_to_column(var = "IC")
  }
  equiv.df <- do.call(rbind,equiv) %>% as.data.frame()
  colnames(equiv.df) <- c("annotated_equivalent", "correlation")
  
  new_annotation_data <- cbind(annotation_data[equiv.df$annotated_equivalent,], equiv.df)
  rownames(new_annotation_data) <- rownames(equiv.df)
  
  data_to_annotate@misc$annotation = new_annotation_data
  
  return(data_to_annotate)
}

