##----------------------------------------------------------------------------##
## Handler for the download for RDS
##----------------------------------------------------------------------------##

observeEvent(input$start_creating_reduction,{
  
  withProgress(message = 'Creating Reduction', value = 0, {
    
    incProgress(0.2, detail = "Calculating ratios")
    # Convert ICA to ratio after removing negative weights
    ICA = values$data@reductions$ica@cell.embeddings
    ICA <- ICA[,rownames(values$Annotation[as.logical(values$Annotation[,"Use"]),])]
    ICA[ICA < 0] = 0
    ICA_ratio <- sweep(ICA, 1, rowSums(ICA), "/") %>% as.data.frame()
    
    # Filter out rejected ICs
    ICA_ratio[ICA_ratio < input$choose_filter_value_column_for_reduction] <- 0
    
    incProgress(0.3, detail = "Creating tables")
    
    table = matrix(nrow = nrow(ICA_ratio), ncol = 0)
    rownames(table) = rownames(ICA_ratio)
    table_genes = matrix(nrow = nrow(GetAssayData(values$data)), ncol = 0)
    rownames(table_genes) = rownames(nrow(GetAssayData(values$data)))
    
    
    #Combinaison ICs -> broad
    combi = data.frame(matrix(nrow = nrow(ICA_ratio), ncol = length(unique(values$Annotation[,input$choose_annotation_column_for_reduction]))))
    rownames(combi) <- rownames(ICA_ratio)
    colnames(combi) <- unique(values$Annotation[,input$choose_annotation_column_for_reduction])
    
    for(i in names(values$annotation_for_output[[input$choose_annotation_column_for_reduction]])){
      
      ICs = values$annotation_for_output[[input$choose_annotation_column_for_reduction]][[i]]
      
      if(length(ICs) > 1){
        IC_combi <- as.data.frame(ICA_ratio[,ICs])
        sum_IC <- rowSums(IC_combi)
        combi[,i] <- sum_IC
        expression_signal = rowMeans(values$data@reductions$ica@feature.loadings[,ICs])
      } else {
        type_signal = as.data.frame(ICA_ratio[,ICs])
        sum_IC <- rowSums(IC_combi)
        combi[,i] <- sum_IC
        expression_signal = values$data@reductions$ica@feature.loadings[,ICs]
      }
      
      table = cbind(table,combi[,i])
      table_genes = cbind(table_genes,expression_signal)
      
      
      colnames(table)[ncol(table)] = gsub(" ","_",i)
      colnames(table_genes)[ncol(table_genes)] = gsub(" ","_",i)
      
    }
    
    if(is.null(values$data@misc$reduction_names)){
      values$data@misc$reduction_names = list()
    }
    
    incProgress(0.5, detail = "Saving reduction tables")
    
    values$data@misc$reduction_names[[input$choose_annotation_column_for_reduction]] = colnames(table)
    
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
  
})
