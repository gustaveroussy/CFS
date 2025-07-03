##----------------------------------------------------------------------------##
## search tables to use for distance analysis
##----------------------------------------------------------------------------##

search_table_for_distance = function(){
  if (input$choose_distances_to_determine == "Genes") {
    if(class(values$data@assays$Spatial) == "Assay5"){
      
      assay_1 <- JoinLayers(values$data@assays$Spatial)
      gc()
      table_sample = raster::t(GetAssayData(assay_1))
      rm(assay_1)
      gc()
      
    } else {
      table_sample = raster::t(GetAssayData(values$data))
    }
    
  } else if (input$choose_distances_to_determine != "Genes") {
    
    if (input$choose_distances_to_determine == "Metadata") {
      
      table_sample = values$data@meta.data[,sapply(values$data@meta.data, is.numeric)]
      table_sample = table_sample[,input$metadata_filter_for_distances]
      
    } else {
      table_sample = values$data@reductions[[input$choose_distances_to_determine]]@cell.embeddings
      
      if(input$choose_distances_to_determine != "ica"){
        colnames(table_sample) = values$data@misc$reduction_names[[input$choose_distances_to_determine]]
      }
      
    }
    
    if(input$use_positive_values_for_distances){
      table_sample[table_sample < 0] = 0
    }
    
  }
  
  if (input$choose_distances_to_determine_2 == "Genes") {
    
    if(class(values$data@assays$Spatial) == "Assay5"){
      
      assay_2 <- JoinLayers(values$data@assays$Spatial)
      gc()
      table_sample_2 = raster::t(GetAssayData(assay_2))
      rm(assay_2)
      gc()
      
    } else {
      table_sample_2 = raster::t(GetAssayData(values$data))
    }
    
  } else if (input$choose_distances_to_determine_2 != "Genes") {
    if (input$choose_distances_to_determine_2 == "Metadata") {
      
      table_sample_2 = values$data@meta.data[,sapply(values$data@meta.data, is.numeric)]
      table_sample_2 = table_sample_2[,input$metadata_filter_for_distances]
      
      
    } else {
      
      table_sample_2 = values$data@reductions[[input$choose_distances_to_determine_2]]@cell.embeddings
      
      if(input$choose_distances_to_determine_2 != "ica"){
        colnames(table_sample_2) = values$data@misc$reduction_names[[input$choose_distances_to_determine_2]]
      }
    }
    
    if(input$use_positive_values_for_distances){
      table_sample_2[table_sample_2 < 0] = 0
    }
    
  }
  
  return(list(table_sample,table_sample_2))
}