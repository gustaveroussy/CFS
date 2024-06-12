##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["IC_distances_plot"]] <- plotly::renderPlotly({
  req(values$distances[[input$choose_distances_to_determine]][[input$choose_sample_for_distances]][[input$choose_method_for_distances]])
  
  return(fig_distance_graph_IC())
})

output[["IC_distances_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("IC_distances_plot",
                         width = "auto",
                         height = "85vh")
  )
})

observeEvent(input$start_distance_IC,{
  req(values$data)
  req(values$data@reductions$ica)
  
  method = input$choose_method_for_distances
  sample = input$choose_sample_for_distances
  
  withProgress(message = 'Calculating distances', value = 0, {
  
    if(method == "Lee"){
      
      if(input$choose_distances_to_determine == "IC"){
        
        table_sample = values$data@reductions$ica@cell.embeddings
        
        if(input$use_positive_values_for_distances){
          table_sample[table_sample < 0] = 0
        }
        
      } else if (input$choose_distances_to_determine == "Genes") {
        table_sample = raster::t(GetAssayData(values$data))
      }
      
      if(length(values$data@images) > 1){
        table_sample = table_sample[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample)),]
      }
      
      incProgress(0.1, detail = "Finding neighbors")
      
      knn = knearneigh(GetTissueCoordinates(values$data, gsub("-", ".", sample)), k=6, longlat = NULL, use_kd_tree=TRUE)
      neighbours = knn2nb(knn, row.names = NULL, sym = FALSE)
      listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)
      
      incProgress(0.2, detail = "Calculating Lee")
      
      if (input$choose_distances_to_determine == "Genes") {
        lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
        
        df <- data.frame(lr=lr)
        df <- df %>% separate(lr, into = c('l', 'r'), sep = "_")
        
        table_sample = as.data.frame(table_sample[,colnames(table_sample) %in% unique(c(df$l,df$r))])
        
        df = df[df[,1] %in% colnames(table_sample)[colSums(table_sample) > 0],]
        df = df[df[,2] %in% colnames(table_sample)[colSums(table_sample) > 0],]
        
      } else if (input$choose_distances_to_determine == "IC"){
        df = t(combn(colnames(table_sample),2))
        df = as.data.frame(df)
      }
      
      if(nrow(df) > 0){
        x = apply(df,1,function(x){n = lee(table_sample[,x[1]], table_sample[,x[2]], listw, nrow(table_sample), zero.policy=attr(listw, "zero.policy"));return(n)})
        
        incProgress(0.7, detail = "Finished")
        
        df[,"weight"] = unlist(lapply(x,function(n){return(n$L)}))
        
        local = lapply(x,function(n){return(n$localL)})
        names(local) = paste0(df[,1]," ",df[,2])
        
        values$distances[[input$choose_distances_to_determine]][[sample]][[method]] = list(df = df,local = local)
      } else {
        shinyalert("Oops!", "No LR interaction found within selected sample", type = "error")
      }
    }
    
  })
  
})


fig_distance_graph_IC <- reactive({
  tree_table = values$distances[[input$choose_distances_to_determine]][[input$choose_sample_for_distances]][[input$choose_method_for_distances]][["df"]]
  
  req(tree_table)
  req(input$choose_n_dim_for_distances)
  
  tree_table = tree_table[(as.double(tree_table[,"weight"]) > 0),]
  
  tree_table = tree_table[scale(tree_table[,"weight"]) > input$Z_score_for_distances,]
  
  G = graph_from_data_frame(tree_table, directed = FALSE)
  
  if(!is.null(input$choose_ic_for_genes_filter_for_distances_ligand) | !is.null(input$choose_ic_for_genes_filter_for_distances_receptor)){
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
  
  if(gsize(G) > 0){

    edges_list = as_edgelist(G)
    layout <- layout_with_fr(G, dim = input$choose_n_dim_for_distances)
    
    if(input$choose_n_dim_for_distances == 2){
      Xn <- layout[,1]
      Yn <- layout[,2]
      
      #create graph
      axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
      
      fig = plot_ly(source = "S")
      
      # create edges
      for(i in 1:length(edges_list[,1])) {
        v0 <- edges_list[i,1]
        v1 <- edges_list[i,2]
        
        rownames(layout) = names(V(G))
        median_point = c((as.double(layout[v0,1]) + as.double(layout[v1,1]))/2,(as.double(layout[v0,2]) + as.double(layout[v1,2]))/2)
        
        fig = fig %>% add_segments(x = as.double(layout[v0,1]),
                     xend = median_point[1],
                     y = as.double(layout[v0,2]),
                     yend = median_point[2],
                     line=list(color="black",
                               width = input$choose_edges_size_for_distances
                               )
                     )
        
        fig = fig %>% add_segments(x = median_point[1],
                                   xend = as.double(layout[v1,1]),
                                   y = median_point[2],
                                   yend = as.double(layout[v1,2]),
                                   line=list(color="black",
                                             width = input$choose_edges_size_for_distances
                                   ),
                                   customdata = paste0(v0," <-> ",v1,"<br>Value : ",tree_table[i,3]),
                                   hovertemplate = paste0("%{customdata}",
                                                          "<extra></extra>")
                                   )
          
      }
      
      # create vertices colors
      if(input$choose_distances_to_determine == "IC"){
        
        annotation = unlist(lapply(names(V(G)), function(x){if(x %in% rownames(values$Annotation)){return(values$Annotation[x,input$choose_vertices_color_for_distances])}else{return("")}}))
        
      } else if (input$choose_distances_to_determine == "Genes") {
        annotation = c()
        
        for(i in names(V(G))){
          ICs = lapply(values$data@misc$GeneAndStat$Contrib_gene,function(x){x = x[x$Sig > 0,];return(i %in% x$gene)})
          ICs = names(ICs[ICs == TRUE])
          annotation = c(annotation,paste(ICs,collapse = ", "))
        }
        
      }
      
      colors = rep(base_palette(),ceiling(length(unique(annotation))/length(base_palette())))[as.numeric(as.factor(annotation))]
      
      #create vertices
      fig = fig %>%
        add_markers(x = ~Xn, y = ~Yn,
                    marker = list(
                      color = colors,
                      size = input$choose_vertices_size_for_distances
                    ),
                    opacity = 1,
                    text = names(V(G)),
                    customdata = annotation,
                    hovertemplate = paste0("IC : %{text}<br>",
                                           "Annotation : %{customdata}",
                                           "<extra></extra>")
        ) %>% layout(
        title = 'Distance graph',
        xaxis = axis,
        yaxis = axis
      ) %>%
      add_text(x = ~Xn, y = ~Yn, text = names(V(G)), textposition = "center") %>%
      hide_legend()
    } else if (input$choose_n_dim_for_distances == 3){
      Xn <- layout[,1]
      Yn <- layout[,2]
      Zn <- layout[,3]
      
      #create graph
      axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
      
      fig = plot_ly(type = 'scatter3d', mode = 'lines+markers')
      
      # create edges
      for(i in 1:length(edges_list[,1])) {
        v0 <- edges_list[i,1]
        v1 <- edges_list[i,2]
        
        rownames(layout) = names(V(G))
        median_point = c((as.double(layout[v0,1]) + as.double(layout[v1,1]))/2,(as.double(layout[v0,2]) + as.double(layout[v1,2]))/2,(as.double(layout[v0,3]) + as.double(layout[v1,3]))/2)
        
        fig = fig %>% add_trace(x=c(as.double(layout[v0,1]), median_point[1]), y=c(as.double(layout[v0,2]), median_point[2]), z=c(as.double(layout[v0,3]), median_point[3]),
                                type="scatter3d", mode="lines",
                                line=list(color="black",
                                          width = input$choose_edges_size_for_distances
                                ),
                                text = paste0(v0," <-> ",v1),
                                customdata = tree_table[i,3],
                                hovertemplate = paste0("%{text}<br>",
                                                       "Value : %{customdata}",
                                                       "<extra></extra>")
                                )
        
        fig = fig %>% add_trace(x=c(median_point[1],as.double(layout[v1,1])), y=c(median_point[2],as.double(layout[v1,2])), z=c(median_point[3],as.double(layout[v1,3])),
                                type="scatter3d", mode="lines",
                                line=list(color="black",
                                          width = input$choose_edges_size_for_distances
                                ),
                                text = paste0(v0," <-> ",v1),
                                customdata = tree_table[i,3],
                                hovertemplate = paste0("%{text}<br>",
                                                       "Value : %{customdata}",
                                                       "<extra></extra>")
                                )
        
      }
      
      # create vertices colors
      annotation = unlist(lapply(names(V(G)), function(x){if(x %in% rownames(values$Annotation)){return(values$Annotation[x,input$choose_vertices_color_for_distances])}else{return("")}}))
      colors = rep(base_palette(),ceiling(length(unique(annotation))/length(base_palette())))[as.numeric(as.factor(annotation))]
      
      #create edges
      fig = fig %>%
        add_markers(x = ~Xn, y = ~Yn, z = ~Zn,
                    marker = list(
                      color = colors,
                      size = input$choose_vertices_size_for_distances
                    ),
                    opacity = 1,
                    text = names(V(G)),
                    customdata = annotation,
                    hovertemplate = paste0("IC : %{text}<br>",
                                           "Annotation : %{customdata}",
                                           "<extra></extra>")
        ) %>% layout(
          title = 'Distance graph',
          xaxis = axis,
          yaxis = axis
        ) %>%
        add_text(x = ~Xn, y = ~Yn, z = ~Zn, text = names(V(G)), textposition = "center") %>%
        hide_legend()
    }
    
    return(fig)
  } else {
    return(NULL)
  }
})
