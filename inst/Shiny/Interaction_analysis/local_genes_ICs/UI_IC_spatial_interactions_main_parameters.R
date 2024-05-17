##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["plot_interactions_IC_genes_from_graph_main_parameters_UI_2"]] <- renderUI({
  req(values$data)
  req(interactions_IC_genes_top_by_ICs())
  tagList(
    selectInput("select_Genes_interactions_IC_genes_choice", label = "Select Genes interactions", 
                choices = names(interactions_IC_genes_top_by_ICs()[["local"]]))
  )
})

output[["plot_interactions_IC_genes_from_graph_main_parameters_UI"]] <- renderUI({
  req(values$data)
  req(graph_click_interactions())
  tagList(
    sliderInput("IC_genes_Z_score_interactions_IC_genes_choice", "Z-score",
                min = 0, max = 100,
                value = 3, step = 0.01),
    radioButtons("transparency_interactions_IC_genes_choice", label = "Alpha type",
                 choices = list("Constant" = 1, "Scaling" = 2), 
                 selected = 1),
    sliderInput("transparency_interactions_IC_genes_range", "Alpha",
                min = 0, max = 1,
                value = 1, step = 0.01),
    numericInput("plot_interactions_IC_genes_size", "Spot size", 4, min = 0, max = NA),
    selectInput("select_color_interactions_IC_genes", label = "Select color", 
                choices = list("Magma" = "A", "Inferno" = "B", "Plasma" = "C", "Viridis" = "D", "Cividis" = "E", "Rocket" = "F", "Mako" = "G", "Turbo" = "H", "Blues", "Reds","YlGnBu","YlOrRd"), 
                selected = "D")
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
interactions_IC_genes_from_graph_main_parameters_info <- list(
  title = "Main parameters for interactions",
  text = p("Options for the main parameters of interactions")
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["interactions_IC_genes_from_graph_main_parameters_info"]], {
  showModal(
    modalDialog(
      interactions_IC_genes_from_graph_main_parameters_info[["text"]],
      title = interactions_IC_genes_from_graph_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## genes genes related to ICs
##----------------------------------------------------------------------------##
interactions_IC_genes_top_by_ICs <- reactive({
  req(values$data)
  req(values$data@reductions$ica)
  
  table = graph_click_interactions()
  
  ICs = str_split(table$customdata,"<br>") |> unlist() |> str_split(" <-> ")
  ICs = ICs[[1]]
  
  Contrib_logic = apply(values$data@reductions$ica@feature.loadings,2,function(x){abs((x-mean(x))/sd(x))>input$IC_genes_Z_score_interactions_IC_genes_choice})
  Contrib_gene <- purrr::map2(as.data.frame(Contrib_logic[,ICs]),colnames(as.data.frame(Contrib_logic[,ICs])),function(x,y){data.frame("gene"=rownames(data@reductions$ica@feature.loadings)[x],"Sig"=as.data.frame(data@reductions$ica@feature.loadings)[x,y])})
  
  Contrib_gene[[ICs[1]]] = Contrib_gene[[ICs[1]]][order(Contrib_gene[[ICs[1]]]$Sig,decreasing = TRUE),]
  Contrib_gene[[ICs[2]]] = Contrib_gene[[ICs[2]]][order(Contrib_gene[[ICs[2]]]$Sig,decreasing = TRUE),]
  
  data = values$data
  method = input$choose_method_for_distances
  sample = input$choose_sample_for_distances
  
  withProgress(message = 'Calculating distances', value = 0, {
    
    if(method == "Lee"){
      
      table = t(data@assays$SCT@data)
      
      if(length(data@images) > 1){
        table_sample = table[grepl(paste0(sample,"_[ACGT]"), rownames(table)),]
      } else {
        table_sample = table
      }
      
      incProgress(0.1, detail = "Finding neighbors")
      
      knn = knearneigh(GetTissueCoordinates(data, sample), k=6, longlat = NULL, use_kd_tree=TRUE)
      neighbours = knn2nb(knn, row.names = NULL, sym = FALSE)
      listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)
      
      incProgress(0.2, detail = "Calculating Lee")
      
      df = expand.grid(Contrib_gene[[ICs[1]]]$gene,Contrib_gene[[ICs[2]]]$gene)
      
      x = apply(df,1,function(x){n = lee(table_sample[,x[1]], table_sample[,x[2]], listw, nrow(table_sample), zero.policy=attr(listw, "zero.policy"));return(n)})
      
      incProgress(0.7, detail = "Finished")
      
      df[,"weight"] = unlist(lapply(x,function(n){return(n$L)}))
      
      local = lapply(x,function(n){return(n$localL)})
      names(local) = paste0(df[,1]," ",df[,2])
      
      tree_table[,"weight"] = scale(tree_table[,"weight"])
      
      tree_table = tree_table[!(as.double(tree_table[,"weight"]) <= 0),]
      
      tree_table = tree_table[as.double(tree_table[,"weight"]) >= (sd(as.double(tree_table[,"weight"])) * input$IC_genes_Z_score_interactions_IC_genes_choice),]
      
    }
    
  })
  
  return(list(ICs = ICs, df = df,local = local))
})





