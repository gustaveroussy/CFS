##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

observeEvent(input$preprocessing_invert_IC_action_button, {
  if(length(input$preprocessing_invert_IC) > 0){
    for(i in input$preprocessing_invert_IC){
      
      withProgress(message = 'Pre-processing', value = 0, {
        incProgress(0.5, detail = "Inverting embeddings")
        
        values$data@reductions$ica@cell.embeddings[,i] = -(values$data@reductions$ica@cell.embeddings[,i])
        values$data@reductions$ica@feature.loadings[,i] = -(values$data@reductions$ica@feature.loadings[,i])
        values$data@misc$GeneAndStat[["Contrib_gene"]][[i]][,"Sig"] = -(values$data@misc$GeneAndStat[["Contrib_gene"]][[i]][,"Sig"])
        
        incProgress(0.5, detail = "Inverting enrichment")
        
        k = values$data@misc[[i]][["en_p"]]
        values$data@misc[[i]][["en_p"]] = values$data@misc[[i]][["en_n"]]
        values$data@misc[[i]][["en_n"]] = k
        
      })
    }
  }
})
