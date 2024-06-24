##----------------------------------------------------------------------------##
## Sample based dotplot
##----------------------------------------------------------------------------##

output[["out_genes_ic_relation_text"]] <- renderDT({ out_genes_ic_relation_text_reactive() })

out_genes_ic_relation_text_reactive <- reactive({
  req(values$data)
  req(input$ica_gene_table_contributive_to_ic)
  
  query = input$ica_gene_table_contributive_to_ic
  
  contrib_byIC <- values$data@misc$GeneAndStat$Contrib_gene
  
  table = contrib_byIC[grepl(query, contrib_byIC)] %>% do.call(rbind,.)
  
  if(!is.null(table)){
    table = table %>%  dplyr::filter(gene==query) %>%   dplyr::arrange(desc(Sig))
    
    output = as.data.frame(table[,2])
    colnames(output) = "value"
    rownames(output) = rownames(table)
    
    return(datatable(output, class = 'cell-border stripe', options = list(scrollX = T), filter = "top"))
    
  } else {
    return(NULL)
  }
  
})