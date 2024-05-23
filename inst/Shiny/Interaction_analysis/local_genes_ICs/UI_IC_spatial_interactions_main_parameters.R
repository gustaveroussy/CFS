##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["plot_interactions_IC_genes_from_graph_main_parameters_UI_2"]] <- renderUI({
  req(values$data)
  req(genes_selection_IC_genes_top_by_ICs())
  if(input$choose_distances_to_determine == "IC"){
    tagList(
      selectInput("select_Genes_interactions_IC_genes_choice_1", label = "Select IC 1", 
                  choices = genes_selection_IC_genes_top_by_ICs()[[1]]["gene"]),
      selectInput("select_Genes_interactions_IC_genes_choice_2", label = "Select IC 2", 
                  choices = genes_selection_IC_genes_top_by_ICs()[[2]]["gene"])
    )
  }
})

output[["plot_interactions_IC_genes_from_graph_main_parameters_UI"]] <- renderUI({
  req(values$data)
  req(graph_click_interactions())
  if(input$choose_distances_to_determine == "IC"){
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
                  selected = "D"),
      checkboxInput("positive_genes_interactions_IC_genes", label = "Only search positively associated genes", value = TRUE, width = NULL),
      actionButton("start_genes_IC_interactions", "Display interactions")
    )
  }
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
## get contrib genes related to ICs
##----------------------------------------------------------------------------##
genes_selection_IC_genes_top_by_ICs <- reactive({
  if(input$choose_distances_to_determine == "IC"){
    table = graph_click_interactions()
    
    ICs = str_split(table$customdata,"<br>") |> unlist() |> str_split(" <-> ")
    ICs = ICs[[1]]
    
    Contrib_logic = apply(values$data@reductions$ica@feature.loadings,2,function(x){abs((x-mean(x))/sd(x))>input$IC_genes_Z_score_interactions_IC_genes_choice})
    
    Contrib_gene <- purrr::map2(as.data.frame(Contrib_logic[,ICs]),colnames(Contrib_logic[,ICs]),function(x,y){data.frame("gene"=rownames(values$data@reductions$ica@feature.loadings)[x],"Sig"=values$data@reductions$ica@feature.loadings[x,y])})
    
    Contrib_gene[[ICs[1]]] = Contrib_gene[[ICs[1]]][order(Contrib_gene[[ICs[1]]]$Sig,decreasing = TRUE),]
    Contrib_gene[[ICs[2]]] = Contrib_gene[[ICs[2]]][order(Contrib_gene[[ICs[2]]]$Sig,decreasing = TRUE),]
    
    if(input$positive_genes_interactions_IC_genes){
      Contrib_gene = lapply(Contrib_gene,function(x){return(x[x$Sig > 0,])})
    }
    
    return(Contrib_gene)
  }
  
})

##----------------------------------------------------------------------------##
## genes genes related to ICs
##----------------------------------------------------------------------------##
interactions_IC_genes_top_by_ICs <- reactive({
  req(values$data)
  req(values$data@reductions$ica)
  req(genes_selection_IC_genes_top_by_ICs())
  if(input$choose_distances_to_determine == "IC"){
    Contrib_gene = genes_selection_IC_genes_top_by_ICs()
    
    data = values$data
    
    method = input$choose_method_for_distances
    sample = input$choose_sample_for_distances
    
    withProgress(message = 'Calculating distances', value = 0, {
      
      if(method == "Lee"){
        
        table = raster::t(data@assays$SCT@data)
        
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
        
        x = lee(table_sample[,input$select_Genes_interactions_IC_genes_choice_1], table_sample[,input$select_Genes_interactions_IC_genes_choice_2], listw, nrow(table_sample), zero.policy=attr(listw, "zero.policy"))
        
        incProgress(0.7, detail = "Finished")
        
      }
      
    })
    
    return(list(L = x$L,local = x$localL))
  }
})





