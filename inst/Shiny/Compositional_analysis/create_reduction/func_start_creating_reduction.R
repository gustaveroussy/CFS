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
  ICA_2.5pc_clean[,!(as.logical(values$Annotation[,"Use"]))] <- 0
  ICA_2.5pc_clean_ratio <- sweep(ICA_2.5pc_clean, 1, rowSums(ICA_2.5pc_clean), "/") %>% as.data.frame()
  ICA_2.5pc_clean_ratio[ICA_2.5pc_clean_ratio < input$choose_filter_value_column_for_reduction] <- 0
  
  
  
  table = NULL
  table_genes = NULL
  
  for(i in names(values$annotation_for_output[[input$choose_annotation_column_for_reduction]])){
    
    ICs = values$annotation_for_output[[input$choose_annotation_column_for_reduction]][[i]]
    
    if(length(ICs) > 1){
      type_signal = rowSums(ICA_2.5pc_clean_ratio[,ICs])
      expression_signal = rowMeans(values$data@reductions$ica@feature.loadings[,ICs])
    } else {
      type_signal = ICA_2.5pc_clean_ratio[,ICs]
      expression_signal = values$data@reductions$ica@feature.loadings[,ICs]
    }
    
    table = cbind(table,type_signal)
    table_genes = cbind(table_genes,expression_signal)
    
    
    colnames(table)[ncol(table)] = gsub(" ","_",i)
    colnames(table_genes)[ncol(table_genes)] = gsub(" ","_",i)
    
  }
  
  colnames(table) = paste0(paste0(toupper(input$choose_annotation_column_for_reduction),"_"),colnames(table),"_",1:ncol(table))
  rownames(table) = rownames(values$data@meta.data)
  colnames(table_genes) = paste0(paste0(toupper(input$choose_annotation_column_for_reduction),"_"),colnames(table_genes),"_",1:ncol(table_genes))
  
  values$data@reductions[[input$choose_annotation_column_for_reduction]] = CreateDimReducObject(
    embeddings = table,
    loadings = table_genes,
    projected = new(Class = 'matrix'),
    assay = "SCT",
    stdev = numeric(),
    key = paste0(toupper(input$choose_annotation_column_for_reduction),"_"),
    global = FALSE,
    jackstraw = new(Class = "JackStrawData"),
    misc = list()
  )
  
})