##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

Launch_analysis <- reactive({
  data <- Load10X_Spatial(input_path())
  #remove zero
  i <- (colSums(data@assays$Spatial, na.rm=T) != 0)
  row_names_df_to_remove<-colnames(data@assays$Spatial[, !i])
  data@meta.data <- data@meta.data[!(row.names(data@meta.data) %in% row_names_df_to_remove),]
  data@images$slice1@coordinates <- data@images$slice1@coordinates[!(row.names(data@images$slice1@coordinates) %in% row_names_df_to_remove),]
  data@assays$Spatial@counts <- data@assays$Spatial[, i]  # all the zero columns
  data@assays$Spatial@data <- data@assays$Spatial[, i]  # all the zero columns
  
  data=PrepNormData(data=data,organism="Mm",variable_features=20000)
  data=ICASpatial(x=data,ncis=30,maxit=600,method="icafast")
  
  return(data)
})