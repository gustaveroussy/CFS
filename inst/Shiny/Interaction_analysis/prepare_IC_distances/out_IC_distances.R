##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["IC_distances_plot"]] <- plotly::renderPlotly({
  req(fig_distance_graph_IC())
  
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
  
  if(is.null(values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[method]]) & is.null(values$distances[[paste0(input$choose_distances_to_determine_2,"_",input$choose_distances_to_determine)]][[method]])){
    withProgress(message = 'Calculating distances', value = 0, {
    
      if (input$choose_distances_to_determine == "Genes") {
        
        table_sample = raster::t(GetAssayData(values$data))
        
      } else if (input$choose_distances_to_determine != "Genes") {
        
        table_sample = values$data@reductions[[input$choose_distances_to_determine]]@cell.embeddings
        
        if(input$choose_distances_to_determine != "ica"){
          colnames(table_sample) = values$data@misc$reduction_names[[input$choose_distances_to_determine]]
        }
        
        if(input$use_positive_values_for_distances){
          table_sample[table_sample < 0] = 0
        }
        
      }
      
      if (input$choose_distances_to_determine_2 == "Genes") {
        
        table_sample_2 = raster::t(GetAssayData(values$data))
        
      } else if (input$choose_distances_to_determine_2 != "Genes") {
        
        table_sample_2 = values$data@reductions[[input$choose_distances_to_determine_2]]@cell.embeddings
        
        if(input$choose_distances_to_determine_2 != "ica"){
          colnames(table_sample_2) = values$data@misc$reduction_names[[input$choose_distances_to_determine_2]]
        }
        
        if(input$use_positive_values_for_distances){
          table_sample_2[table_sample_2 < 0] = 0
        }
        
      }
      
      if(length(values$data@images) > 1){
        table_sample = table_sample[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample)),]
        table_sample_2 = table_sample_2[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample_2)),]
      }
      
      incProgress(0.1, detail = "Finding neighbors")
      
      if(method == "Lee"){
        
        knn = knearneigh(GetTissueCoordinates(values$data, sample), k=6, longlat = NULL, use_kd_tree=TRUE)
        neighbours = knn2nb(knn, row.names = NULL, sym = FALSE)
        listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)
        
        incProgress(0.2, detail = "Calculating Lee")
        
        if (input$choose_distances_to_determine == "Genes" & input$choose_distances_to_determine_2 == "Genes"){
          lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
          
          df <- data.frame(lr=lr)
          df <- df %>% separate(lr, into = c(input$choose_distances_to_determine, paste0(input$choose_distances_to_determine_2,"_2")), sep = "_")
          
          table_sample = as.data.frame(table_sample[,colnames(table_sample) %in% unique(c(df[,1],df[,2]))])
          table_sample_2 = as.data.frame(table_sample_2[,colnames(table_sample_2) %in% unique(c(df[,1],df[,2]))])
          
          df = df[df[,1] %in% colnames(table_sample)[colSums(table_sample) > 0],]
          df = df[df[,2] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
          
        } else if (input$choose_distances_to_determine == "Genes") {
          
          lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
          
          df <- data.frame(lr=lr)
          df <- df %>% separate(lr, into = c('l', 'r'), sep = "_")
          
          table_sample = as.data.frame(table_sample[,colnames(table_sample) %in% unique(c(df$l,df$r))])
          
          df = df[df[,1] %in% colnames(table_sample)[colSums(table_sample) > 0],]
          df = df[df[,2] %in% colnames(table_sample)[colSums(table_sample) > 0],]
          
          df = expand_grid(unique(c(df$l,df$r)),unique(colnames(table_sample_2)))
          df = as.data.frame(df)
          colnames(df) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
          
        } else if(input$choose_distances_to_determine_2 == "Genes"){
          
          lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
          
          df <- data.frame(lr=lr)
          df <- df %>% separate(lr, into = c('l', 'r'), sep = "_")
          
          table_sample_2 = as.data.frame(table_sample_2[,colnames(table_sample_2) %in% unique(c(df$l,df$r))])
          
          df = df[df[,1] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
          df = df[df[,2] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
          
          df = expand_grid(unique(colnames(table_sample)),unique(c(df$l,df$r)))
          df = as.data.frame(df)
          colnames(df) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
          
        } else if(input$choose_distances_to_determine != "Genes" & input$choose_distances_to_determine_2 != "Genes") {
          if(input$choose_distances_to_determine == input$choose_distances_to_determine_2){
            df = t(combn(unique(colnames(table_sample)),2))
            df = as.data.frame(df)
            colnames(df) = c(input$choose_distances_to_determine,paste0(input$choose_distances_to_determine_2,"_2"))
            
          } else {
            df = expand_grid(unique(colnames(table_sample)),unique(colnames(table_sample_2)))
            df = as.data.frame(df)
            colnames(df) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
          }
        }
        
        if(nrow(df) > 0){
          
          x = apply(df,1,function(x){n = lee(table_sample[,x[1]], table_sample_2[,x[2]], listw, nrow(table_sample), zero.policy=attr(listw, "zero.policy"));return(n)})
          
          incProgress(0.7, detail = "Finished")
          
          df[,"weight"] = unlist(lapply(x,function(n){return(n$L)}))
          
          values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[method]] = df
        } else {
          shinyalert("Oops!", "No LR interaction found within selected sample", type = "error")
        }
        
      } else if (method == "Dotproduct"){
        
        if (input$choose_distances_to_determine == input$choose_distances_to_determine_2){
          
          if (input$choose_distances_to_determine == "Genes"){
            lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
            
            df <- data.frame(lr=lr)
            df <- df %>% separate(lr, into = c(input$choose_distances_to_determine, paste0(input$choose_distances_to_determine_2,"_2")), sep = "_")
            
            table_sample = table_sample[,colnames(table_sample) %in% unique(c(df[,1],df[,2]))]
            table_sample_2 = table_sample_2[,colnames(table_sample_2) %in% unique(c(df[,1],df[,2]))]
            
          }
          
          dot_product = t(table_sample) %*% table_sample_2
          dot_product = as.data.frame(as.table(as.matrix(dot_product)))
          
          if (input$choose_distances_to_determine == "Genes"){
            df = merge(df, dot_product, by=c(1,2))
          } else {
            combinations = t(combn(colnames(table_sample), 2))
            df = merge(combinations, dot_product, by=c(1,2))
          }
          
          values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[method]] = df
        
        }
        
      }
  
    })
  } else {
    shinyalert("Info", "Already calculated", type = "info")
  }
  
})

observeEvent(input$start_distance_IC_batch,{
  req(values$data)
  req(values$data@reductions$ica)
  
  method = input$choose_method_for_distances
  sample_all = names(values$data@images)
  
  for(sample in sample_all){
    if(is.null(values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[method]]) & is.null(values$distances[[paste0(input$choose_distances_to_determine_2,"_",input$choose_distances_to_determine)]][[sample]][[method]])){
      withProgress(message = paste0('Calculating ',sample), value = 0, {
        
        if (input$choose_distances_to_determine == "Genes") {
          
          table_sample = raster::t(GetAssayData(values$data))
          
        } else if (input$choose_distances_to_determine != "Genes") {
          
          table_sample = values$data@reductions[[input$choose_distances_to_determine]]@cell.embeddings
          
          if(input$choose_distances_to_determine != "ica"){
            colnames(table_sample) = values$data@misc$reduction_names[[input$choose_distances_to_determine]]
          }
          
          if(input$use_positive_values_for_distances){
            table_sample[table_sample < 0] = 0
          }
          
        }
        
        if (input$choose_distances_to_determine_2 == "Genes") {
          
          table_sample_2 = raster::t(GetAssayData(values$data))
          
        } else if (input$choose_distances_to_determine_2 != "Genes") {
          
          table_sample_2 = values$data@reductions[[input$choose_distances_to_determine_2]]@cell.embeddings
          
          if(input$choose_distances_to_determine_2 != "ica"){
            colnames(table_sample_2) = values$data@misc$reduction_names[[input$choose_distances_to_determine_2]]
          }
          
          if(input$use_positive_values_for_distances){
            table_sample_2[table_sample_2 < 0] = 0
          }
          
        }
        
        if(length(values$data@images) > 1){
          table_sample = table_sample[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample)),]
          table_sample_2 = table_sample_2[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample_2)),]
        }
        
        if(method == "Lee"){
          
          incProgress(0.1, detail = "Finding neighbors")
          
          knn = knearneigh(GetTissueCoordinates(values$data, sample), k=6, longlat = NULL, use_kd_tree=TRUE)
          neighbours = knn2nb(knn, row.names = NULL, sym = FALSE)
          listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)
          
          incProgress(0.2, detail = "Calculating Lee")
          
          if (input$choose_distances_to_determine == "Genes" & input$choose_distances_to_determine_2 == "Genes"){
            lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
            
            df <- data.frame(lr=lr)
            df <- df %>% separate(lr, into = c(input$choose_distances_to_determine, paste0(input$choose_distances_to_determine_2,"_2")), sep = "_")
            
            table_sample = as.data.frame(table_sample[,colnames(table_sample) %in% unique(c(df[,1],df[,2]))])
            table_sample_2 = as.data.frame(table_sample_2[,colnames(table_sample_2) %in% unique(c(df[,1],df[,2]))])
            
            df = df[df[,1] %in% colnames(table_sample)[colSums(table_sample) > 0],]
            df = df[df[,2] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
            
          } else if (input$choose_distances_to_determine == "Genes") {
            
            lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
            
            df <- data.frame(lr=lr)
            df <- df %>% separate(lr, into = c('l', 'r'), sep = "_")
            
            table_sample = as.data.frame(table_sample[,colnames(table_sample) %in% unique(c(df$l,df$r))])
            
            df = df[df[,1] %in% colnames(table_sample)[colSums(table_sample) > 0],]
            df = df[df[,2] %in% colnames(table_sample)[colSums(table_sample) > 0],]
            
            df = expand_grid(unique(c(df$l,df$r)),unique(colnames(table_sample_2)))
            df = as.data.frame(df)
            colnames(df) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
            
          } else if(input$choose_distances_to_determine_2 == "Genes"){
            
            lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
            
            df <- data.frame(lr=lr)
            df <- df %>% separate(lr, into = c('l', 'r'), sep = "_")
            
            table_sample_2 = as.data.frame(table_sample_2[,colnames(table_sample_2) %in% unique(c(df$l,df$r))])
            
            df = df[df[,1] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
            df = df[df[,2] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
            
            df = expand_grid(unique(colnames(table_sample)),unique(c(df$l,df$r)))
            df = as.data.frame(df)
            colnames(df) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
            
          } else if(input$choose_distances_to_determine != "Genes" & input$choose_distances_to_determine_2 != "Genes") {
            if(input$choose_distances_to_determine == input$choose_distances_to_determine_2){
              df = t(combn(unique(colnames(table_sample)),2))
              df = as.data.frame(df)
              colnames(df) = c(input$choose_distances_to_determine,paste0(input$choose_distances_to_determine_2,"_2"))
              
            } else {
              df = expand_grid(unique(colnames(table_sample)),unique(colnames(table_sample_2)))
              df = as.data.frame(df)
              colnames(df) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
            }
          }
          
          if(nrow(df) > 0){
            
            x = apply(df,1,function(x){n = lee(table_sample[,x[1]], table_sample_2[,x[2]], listw, nrow(table_sample), zero.policy=attr(listw, "zero.policy"));return(n)})
            
            incProgress(0.7, detail = "Finished")
            
            df[,"weight"] = unlist(lapply(x,function(n){return(n$L)}))
            
            values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[method]] = df
          } else {
            shinyalert("Oops!", "No LR interaction found within selected sample", type = "error")
          }
          
        }  else if (method == "Dotproduct"){
          
          if (input$choose_distances_to_determine == input$choose_distances_to_determine_2){
            
            if (input$choose_distances_to_determine == "Genes"){
              lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
              
              df <- data.frame(lr=lr)
              df <- df %>% separate(lr, into = c(input$choose_distances_to_determine, paste0(input$choose_distances_to_determine_2,"_2")), sep = "_")
              
              table_sample = table_sample[,colnames(table_sample) %in% unique(c(df[,1],df[,2]))]
              table_sample_2 = table_sample_2[,colnames(table_sample_2) %in% unique(c(df[,1],df[,2]))]
              
            }
            
            dot_product = t(table_sample) %*% table_sample_2
            dot_product = as.data.frame(as.table(as.matrix(dot_product)))
            
            if (input$choose_distances_to_determine == "Genes"){
              df = merge(df, dot_product, by=c(1,2))
            } else {
              combinations = t(combn(colnames(table_sample), 2))
              df = merge(combinations, dot_product, by=c(1,2))
            }
            
            values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[method]] = df
            
          }
          
        }
        
      })
    }
  }
})

fig_distance_graph_IC <- reactive({
  if(!is.null(values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]])){
    tree_table = values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[input$choose_sample_for_distances]][[input$choose_method_for_distances]]
  } else if (!is.null(values$distances[[paste0(input$choose_distances_to_determine_2,"_",input$choose_distances_to_determine)]])){
    tree_table = values$distances[[paste0(input$choose_distances_to_determine_2,"_",input$choose_distances_to_determine)]][[input$choose_sample_for_distances]][[input$choose_method_for_distances]]
  } else {
    tree_table = NULL
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
  
  if(gsize(G) > 0){
    
    edges_list = as.data.frame(as_edgelist(G))
    
    if(input$choose_distances_to_determine == input$choose_distances_to_determine_2){
      colnames(edges_list) = c(input$choose_distances_to_determine,paste0(input$choose_distances_to_determine_2,"_2"))
    } else {
      colnames(edges_list) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
    }
    
    if(input$choose_layout_for_distances %in% c("fr","kk","mds")){
      layout <- do.call(paste0("layout_with_",input$choose_layout_for_distances),list(graph = G, dim = input$choose_n_dim_for_distances))
    } else if(input$choose_layout_for_distances %in% c("sugiyama")){
      layout <- do.call(paste0("layout_with_",input$choose_layout_for_distances),list(graph = G))$layout
    } else {
      layout <- do.call(paste0("layout_with_",input$choose_layout_for_distances),list(graph = G))
    }
    
    if(input$choose_n_dim_for_distances == 2){
      
      Xn <- layout[,1]
      Yn <- layout[,2]
      
      #create graph
      axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
      
      fig = plot_ly(source = "S")

      rownames(layout) = names(V(G))
      
      # create edges
      edges_list[,"x_start"] = as.double(layout[edges_list[,1],1])
      edges_list[,"x_end"] = as.double(layout[edges_list[,2],1])
      edges_list[,"y_start"] = as.double(layout[edges_list[,1],2])
      edges_list[,"y_end"] = as.double(layout[edges_list[,2],2])
      edges_list[,"x_median"] = (edges_list[,"x_start"] + edges_list[,"x_end"])/2
      edges_list[,"y_median"] = (edges_list[,"y_start"] + edges_list[,"y_end"])/2
      edges_list[,"weight"] = tree_table[(paste0(tree_table[,1],"_",tree_table[,2]) %in% paste0(edges_list[,1],"_",edges_list[,2]) | paste0(tree_table[,2],"_",tree_table[,1]) %in% paste0(edges_list[,1],"_",edges_list[,2])) ,"weight"]

      if(input$distances_use_log_for_graph){
        w = log(edges_list[,"weight"] + 1)
        limits=range(w)
        pal = viridis::viridis(100)
        edges_list[,"colors"] = pal[findInterval(w,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
      } else {
        limits=range(edges_list[,"weight"])
        pal = viridis::viridis(100)
        edges_list[,"colors"] = pal[findInterval(edges_list[,"weight"],seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
      }
      
      edges_list[,"text"] = paste0(edges_list[,1]," <-> ",edges_list[,2],"<br>Value : ",edges_list$weight)
      
      for(i in 1:nrow(edges_list)){
        fig = fig %>% add_segments(data = edges_list[i,],
                                   x = ~x_start,
                                   xend = ~x_median,
                                   y = ~y_start,
                                   yend = ~y_median,
                                   line=list(color = ~colors,
                                             width = input$choose_edges_size_for_distances
                                   )
        )
        
        fig = fig %>% add_segments(data = edges_list[i,],
                                   x = ~x_median,
                                   xend = ~x_end,
                                   y = ~y_median,
                                   yend = ~y_end,
                                   line=list(color=~colors,
                                             width = input$choose_edges_size_for_distances
                                   ),
                                   customdata = ~text,
                                   hovertemplate = paste0("%{customdata}",
                                                          "<extra></extra>")
        )
      }
      
      # create vertices colors
      if(input$choose_distances_to_determine != "Genes" & input$choose_distances_to_determine_2 != "Genes"){
        annotation = c()
        annotation_2 = c()
        
        for(i in names(V(G))){

            if(i %in% rownames(values$Annotation)){
              annotation = c(annotation,values$Annotation[i,input$choose_vertices_color_for_distances])
              annotation_2 = c(annotation_2,values$Annotation[i,input$choose_vertices_annotation_for_distances])
              
            } else {
              annotation = c(annotation,"")
              annotation_2 = c(annotation_2,"")
              
            }
          
        }
        
        annotation_3 = paste0("Color: ", annotation,"<br>Annotation: ",annotation_2)
        
      } else if (input$choose_distances_to_determine == "Genes" | input$choose_distances_to_determine_2 == "Genes") {
        
        annotation = c()
        annotation_2 = c()
        
        for(i in names(V(G))){
          if(i %in% rownames(values$data@assays$SCT@data)){
            ICs = lapply(values$data@misc$GeneAndStat$Contrib_gene,function(x){x = x[x$Sig > 0,];return(i %in% x$gene)})
            ICs = names(ICs[ICs == TRUE])
            ICs = paste0(ICs, " : ", values$Annotation[rownames(values$Annotation) %in% ICs,"Type"] ," : ",values$Annotation[rownames(values$Annotation) %in% ICs,"Annotation"])
            annotation = c(annotation,paste(ICs,collapse = "<br>"))
            annotation_2 = c(annotation_2,"")
          } else {
            if(i %in% rownames(values$Annotation)){
              annotation = c(annotation,values$Annotation[i,input$choose_vertices_color_for_distances])
              annotation_2 = c(annotation_2,values$Annotation[i,input$choose_vertices_annotation_for_distances])
              
            } else {
              annotation = c(annotation,"")
              annotation_2 = c(annotation_2,"")
              
              
            }
          }
          
        }
        
        annotation_3 = paste0("Color: ", annotation,"<br>Annotation: ",annotation_2)
        
      }
      
      colors = rep(values$palette,ceiling(length(unique(annotation))/length(palette())))[as.numeric(as.factor(annotation))]

      #create vertices
      fig = fig %>%
        add_markers(x = ~Xn, y = ~Yn,
                    marker = list(
                      color = colors,
                      size = input$choose_vertices_size_for_distances
                    ),
                    opacity = 1,
                    text = names(V(G)),
                    customdata = annotation_3,
                    hovertemplate = paste0(input$choose_distances_to_determine, " : %{text}<br>",
                                           "%{customdata}",
                                           "<extra></extra>")
        ) %>% layout(
        title = 'Distance graph',
        xaxis = axis,
        yaxis = axis,
        hoverlabel = list(align = "left")
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
      
      rownames(layout) = names(V(G))
      
      # create edges
      edges_list[,"x_start"] = as.double(layout[edges_list[,1],1])
      edges_list[,"x_end"] = as.double(layout[edges_list[,2],1])
      edges_list[,"y_start"] = as.double(layout[edges_list[,1],2])
      edges_list[,"y_end"] = as.double(layout[edges_list[,2],2])
      edges_list[,"z_start"] = as.double(layout[edges_list[,1],3])
      edges_list[,"z_end"] = as.double(layout[edges_list[,2],3])
      edges_list[,"x_median"] = (edges_list[,"x_start"] + edges_list[,"x_end"])/2
      edges_list[,"y_median"] = (edges_list[,"y_start"] + edges_list[,"y_end"])/2
      edges_list[,"z_median"] = (edges_list[,"z_start"] + edges_list[,"z_end"])/2
      edges_list[,"weight"] = tree_table[(paste0(tree_table[,1],"_",tree_table[,2]) %in% paste0(edges_list[,1],"_",edges_list[,2]) | paste0(tree_table[,2],"_",tree_table[,1]) %in% paste0(edges_list[,1],"_",edges_list[,2])) ,"weight"]
      
      if(input$distances_use_log_for_graph){
        w = log(edges_list[,"weight"] + 1)
        limits=range(w)
        pal = viridis::viridis(100)
        edges_list[,"colors"] = pal[findInterval(w,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
      } else {
        limits=range(edges_list[,"weight"])
        pal = viridis::viridis(100)
        edges_list[,"colors"] = pal[findInterval(edges_list[,"weight"],seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
      }
      
      edges_list[,"text"] = paste0(edges_list[,1]," <-> ",edges_list[,2],"<br>Value : ",edges_list$weight)
      
      for(i in 1:nrow(edges_list)){
        fig = fig %>% add_trace(x=c(as.double(edges_list[i,"x_start"]), as.double(edges_list[i,"x_median"])), y=c(as.double(edges_list[i,"y_start"]), as.double(edges_list[i,"y_median"])), z=c(as.double(edges_list[i,"z_start"]), as.double(edges_list[i,"z_median"])),
                                type="scatter3d", mode="lines",
                                line=list(color=edges_list[i,"colors"],
                                          width = input$choose_edges_size_for_distances
                                ),
                                customdata = edges_list[i,"text"],
                                hovertemplate = paste0("%{customdata}",
                                                         "<extra></extra>")
                                )
        
        fig = fig %>% add_trace(x=c(as.double(edges_list[i,"x_median"]), as.double(edges_list[i,"x_end"])), y=c(as.double(edges_list[i,"y_median"]), as.double(edges_list[i,"y_end"])), z=c(as.double(edges_list[i,"z_median"]), as.double(edges_list[i,"z_end"])),
                                type="scatter3d", mode="lines",
                                line=list(color=edges_list[i,"colors"],
                                          width = input$choose_edges_size_for_distances
                                ),
                                customdata = edges_list[i,"text"],
                                hovertemplate = paste0("%{customdata}",
                                                       "<extra></extra>")
        )

      }
      
      # create vertices colors
      if(input$choose_distances_to_determine != "Genes" & input$choose_distances_to_determine_2 != "Genes"){
        annotation = c()
        annotation_2 = c()
        
        for(i in names(V(G))){
          
          if(i %in% rownames(values$Annotation)){
            annotation = c(annotation,values$Annotation[i,input$choose_vertices_color_for_distances])
            annotation_2 = c(annotation_2,values$Annotation[i,input$choose_vertices_annotation_for_distances])
            
          } else {
            annotation = c(annotation,"")
            annotation_2 = c(annotation_2,"")
            
          }
          
        }
        
        annotation_3 = paste0("Color: ", annotation,"<br>Annotation: ",annotation_2)
        
      } else if (input$choose_distances_to_determine == "Genes" | input$choose_distances_to_determine_2 == "Genes") {
        
        annotation = c()
        annotation_2 = c()
        
        for(i in names(V(G))){
          if(i %in% rownames(values$data@assays$SCT@data)){
            ICs = lapply(values$data@misc$GeneAndStat$Contrib_gene,function(x){x = x[x$Sig > 0,];return(i %in% x$gene)})
            ICs = names(ICs[ICs == TRUE])
            ICs = paste0(ICs, " : ", values$Annotation[rownames(values$Annotation) %in% ICs,"Type"] ," : ",values$Annotation[rownames(values$Annotation) %in% ICs,"Annotation"])
            annotation = c(annotation,paste(ICs,collapse = "<br>"))
            annotation_2 = c(annotation_2,"")
          } else {
            if(i %in% rownames(values$Annotation)){
              annotation = c(annotation,values$Annotation[i,input$choose_vertices_color_for_distances])
              annotation_2 = c(annotation_2,values$Annotation[i,input$choose_vertices_annotation_for_distances])
              
            } else {
              annotation = c(annotation,"")
              annotation_2 = c(annotation_2,"")
              
              
            }
          }
          
        }
        
        annotation_3 = paste0("Color: ", annotation,"<br>Annotation: ",annotation_2)
        
      }
      
      colors = rep(values$palette,ceiling(length(unique(annotation))/length(palette())))[as.numeric(as.factor(annotation))]
      
      #create vertices
      fig = fig %>%
        add_markers(x = ~Xn, y = ~Yn, z = ~Zn,
                    marker = list(
                      color = colors,
                      size = input$choose_vertices_size_for_distances
                    ),
                    opacity = 1,
                    text = names(V(G)),
                    customdata = annotation_3,
                    hovertemplate = paste0(input$choose_distances_to_determine, " : %{text}<br>",
                                           "%{customdata}",
                                           "<extra></extra>")
        ) %>% layout(
          title = 'Distance graph',
          xaxis = axis,
          yaxis = axis,
          hoverlabel = list(align = "left")
        ) %>%
        add_text(x = ~Xn, y = ~Yn, z = ~Zn, text = names(V(G)), textposition = "center") %>%
        hide_legend()
    }
    
    return(fig)
  } else {
    return(NULL)
  }
})
