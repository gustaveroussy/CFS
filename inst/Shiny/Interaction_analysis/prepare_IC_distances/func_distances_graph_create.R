##----------------------------------------------------------------------------##
## functions to create plotly object for graph
##----------------------------------------------------------------------------##

create_distance_graph = function(sample = "All"){

  if (sample == "All"){
    
  } else {
    if(!is.null(values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]])){
      tree_table = values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[input$choose_method_for_distances]]
    } else if (!is.null(values$distances[[paste0(input$choose_distances_to_determine_2,"_",input$choose_distances_to_determine)]])){
      tree_table = values$distances[[paste0(input$choose_distances_to_determine_2,"_",input$choose_distances_to_determine)]][[sample]][[input$choose_method_for_distances]]
    } else {
      tree_table = NULL
    }
  }
  
  req(tree_table)
  req(input$choose_n_dim_for_distances)
  
  if(input$choose_distances_to_determine == input$choose_distances_to_determine_2){
    colnames(tree_table) = c(input$choose_distances_to_determine,paste0(input$choose_distances_to_determine_2,"_2"),"weight")
  } else {
    colnames(tree_table) = c(input$choose_distances_to_determine,paste0(input$choose_distances_to_determine_2),"weight")
  }
  
  tree_table = tree_table[(as.double(tree_table[,"weight"]) > 0),]
  
  tree_table = tree_table[as.double(scale(tree_table[,"weight"])) > input$Z_score_for_distances,]
  
  G = graph_from_data_frame(tree_table, directed = FALSE)
  
  if(input$choose_distances_to_determine == "Genes" & input$choose_distances_to_determine_2 == "Genes" & !is.null(input$choose_ic_for_genes_filter_for_distances_ligand) | !is.null(input$choose_ic_for_genes_filter_for_distances_receptor)){
    l = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$ligand_gene_symbol
    r = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$receptor_gene_symbol
    
    if(!is.null(input$choose_ic_for_genes_filter_for_distances_ligand)){
      genes_ICs = lapply(input$choose_ic_for_genes_filter_for_distances_ligand,function(n){return(values$data@misc$GeneAndStat$Contrib_gene[[n]])})
      genes_ICs = bind_rows(genes_ICs)
      genes_ICs = genes_ICs %>%
        arrange(desc(Sig)) %>%
        filter(Sig>=0)
      l = l[l %in% unique(genes_ICs$gene)]
    }
    
    if(!is.null(input$choose_ic_for_genes_filter_for_distances_receptor)){
      genes_ICs = lapply(input$choose_ic_for_genes_filter_for_distances_receptor,function(n){return(values$data@misc$GeneAndStat$Contrib_gene[[n]])})
      genes_ICs = bind_rows(genes_ICs)
      genes_ICs = genes_ICs %>%
        arrange(desc(Sig)) %>%
        filter(Sig>=0)
      r = r[r %in% unique(genes_ICs$gene)]
    }
    
    genes = c(l,r)
    
    G = subgraph(G, genes[genes %in% names(V(G))])
  }
  
  if(input$IC_distances_remove_IC_from_graph){
    genes_to_keep = names(V(G))[!grepl("IC_",names(V(G)))]
    ICs_to_keep = rownames(values$Annotation)[unlist(values$Annotation[,"Use"] == "TRUE")]
    ICs_to_keep = ICs_to_keep[ICs_to_keep %in% names(V(G))]
    
    G = subgraph(G, c(ICs_to_keep,genes_to_keep))
    
  }
  
  return(list(G = G, tree_table = tree_table))
  
}