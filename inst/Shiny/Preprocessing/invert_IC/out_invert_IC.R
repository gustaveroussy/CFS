##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

observeEvent(input$preprocessing_invert_IC_action_button,{
  cell = values$data@reductions$ica@cell.embeddings
  cell[,input$preprocessing_select_inver_IC] = -(cell[,input$preprocessing_select_inver_IC])
  values$data@reductions$ica@cell.embeddings = cell

  genes = values$data@reductions$ica@feature.loadings
  genes[,input$preprocessing_select_inver_IC] = -(genes[,input$preprocessing_select_inver_IC])
  values$data@reductions$ica@feature.loadings = genes
  
  list_to_change = values$data@misc[[input$preprocessing_select_inver_IC]]
  
  if("en_p" %in% names(list_to_change) & "en_n" %in% names(list_to_change)){
    b = list_to_change[["en_p"]]
    list_to_change[["en_p"]] = list_to_change[["en_n"]]
    list_to_change[["en_n"]] = b
  } else if ("en_p" %in% names(list_to_change)){
    list_to_change[["en_n"]] = list_to_change[["en_p"]]
    list_to_change[["en_p"]] = NULL
  }
  
  if("IC_weight" %in% names(list_to_change)){
    list_to_change$IC_weight = -(list_to_change$IC_weight)
  }
  
  if("IC_top_genes_weight" %in% names(list_to_change)){
    list_to_change$IC_top_genes_weight[,input$preprocessing_select_inver_IC] = -(list_to_change$IC_top_genes_weight[,input$preprocessing_select_inver_IC])
  }
  
  values$data@misc[[input$preprocessing_select_inver_IC]] = list_to_change
  
  if("GeneAndStat" %in% names(values$data@misc)){
    values$data@misc[["GeneAndStat"]][["Contrib_gene"]] = lapply(values$data@misc[["GeneAndStat"]][["Contrib_gene"]],function(x){x$Sig = -(x$Sig);return(x)})
    values$data@misc[["GeneAndStat"]][["Kurtosis_ICs"]]["IC_1"] = kurtosis(values$data@reductions$ica@feature.loadings[,input$preprocessing_select_inver_IC])
  }
  
  shinyalert("Success!", paste0(input$preprocessing_select_inver_IC," has been inverted"), type = "success")
  
})
