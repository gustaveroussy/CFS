##----------------------------------------------------------------------------##
## Handler for the download for RDS
##----------------------------------------------------------------------------##

observeEvent(input$start_creating_reduction,{
  
  # Convert ICA to ratio after removing negative weights
  ICA = values$data@reductions$ica@cell.embeddings
  ICA[ICA < 0] = 0
  ICA_ratio <- sweep(ICA, 1, rowSums(ICA), "/") %>% as.data.frame()
  
  # Filter out rejected ICs
  ICA_2.5pc = ICA_ratio
  ICA_2.5pc[ICA_2.5pc < 0] = 0
  ICA_2.5pc_clean <- ICA_2.5pc
  ICA_2.5pc_clean[,values$data@misc$annotation[,"Use"]==FALSE] <- 0
  ICA_2.5pc_clean_ratio <- sweep(ICA_2.5pc_clean, 1, rowSums(ICA_2.5pc_clean), "/") %>% as.data.frame()
  ICA_2.5pc_clean_ratio[ICA_2.5pc_clean_ratio< input$choose_filter_value_column_for_reduction] <- 0
  
  
  
  table = NULL
  table_genes = NULL
  
  for(i in names(values$annotation_for_output[[input$choose_annotation_column_for_reduction]])){
    
    ICs = values$annotation_for_output[[input$choose_annotation_column_for_reduction]][[i]]
    type_signal = rowSums(ICA_2.5pc_clean_ratio[,ICs])
    expression_signal = rowMeans(values$data@reductions$ica@feature.loadings[,ICs])
    
    table = cbind(table,type_signal)
    table_genes = cbind(table_genes,expression_signal)
    
  }
  
  colnames(table) = paste0(paste0(toupper(input$choose_annotation_column_for_reduction),"_"),1:ncol(table))
  colnames(table_genes) = paste0(paste0(toupper(input$choose_annotation_column_for_reduction),"_"),1:ncol(table_genes))
  
  
  
  values$data@reductions[[input$choose_annotation_column_for_reduction]] = new(
    Class = "DimReduc",
    cell.embeddings = table,
    feature.loadings = table_genes,
    feature.loadings.projected = matrix(nrow = 0, ncol = 0),
    assay.used = "SCT",
    global = FALSE,
    stdev = numeric(0),
    jackstraw = new(Class = "JackStrawData"),
    misc = list(),
    key = paste0(toupper(input$choose_annotation_column_for_reduction),"_")
  )
})