associate_signal_with_IC <- function(){
  
    
  values$annotation_for_output = list()
  
  for(i in colnames(values$Annotation)[!(colnames(values$Annotation) %in% c("Use","Annotation"))]){
    values$annotation_for_output[[i]] = list()
    
    
    # Get All annotations and their associated ICs
    list_names_IC = unique(unlist(str_split(values$Annotation[,i], pattern = ',', n = Inf, simplify = FALSE)))
    list_names_IC = list_names_IC[list_names_IC != ""]
    
    for (list_annotation in list_names_IC) {
      
      result = as.logical(values$Annotation[,'Use']) & unlist(lapply(str_split(values$Annotation[,i], pattern = ',', n = Inf, simplify = FALSE),function(i){return(list_annotation %in% i)}))
      
      values$annotation_for_output[[i]][[list_annotation]] = rownames(values$Annotation)[result]
    }
  }

}