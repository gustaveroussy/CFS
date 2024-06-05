##----------------------------------------------------------------------------##
## Enrichment barplot
##----------------------------------------------------------------------------##

output[["IC_enrichment_interactive"]] <- plotly::renderPlotly({
  return(enrichment_barplot_react())
})

output[["IC_enrichment"]] <- shiny::renderPlot({
  plot(enrichment_barplot_react())
})

enrichment_barplot_react <- reactive({
  req(values$data)
  req(input$IC_choice)
  req(input$IC_enrichment_database_choice)
  req(input$p_n_enrichment)
  
  req(!is.null(values$data@misc[[input[["IC_choice"]]]][[input$p_n_enrichment]]))
  
  data <- values$data
  IC_C = input[["IC_choice"]]
  database_C = input[["IC_enrichment_database_choice"]]
  p_n = input[["p_n_enrichment"]]
  
  table <- data@misc[[IC_C]][[p_n]][[database_C]]
  
  number_of_genes <- table["Overlap"]
  
  table["Overlap"] <- lapply(table["Overlap"], sub, pattern="/.*", replacement="")
  
  number_enrichr <- input$enrichment_disp_number
  
  
  if(input$IC_enrichment_interactive_display){
    # create list of genes
    for (k in 1:length(rownames(table))) {
      gene_text = c()
      u <- unlist(strsplit(table["Genes"][k,][1], split = ";"))
      for (v in 1:length(u)) {
        gene_text <- append(gene_text,u[v])
        if (v %% 5 == 0) {
          gene_text <- append(gene_text,"<br>")
        }
      }
      u <- paste(gene_text,collapse = ",")
      
      # Replace all characters occurrence in a string
      u <- gsub('<br>,','<br>',u)
      
      table["Genes"][k,] <- u
    }
    
    genes <- table["Genes"]
    
    x <- table["Overlap"][1:number_enrichr,]
    y <- table["Term"][1:number_enrichr,]
    
    fig <- plot_ly(type = "bar", orientation = 'h')
    
    fig <- fig %>% add_trace(
      x = x, 
      y = y,
      hovertext = paste0(y,"\nnumber of genes: ", number_of_genes[1:number_enrichr,],"\nP-value: ",table["Adjusted.P.value"][1:number_enrichr,]
                         , "\ngenes : ", genes[1:number_enrichr,]),
      hoverinfo = 'text', 
      showlegend=FALSE,
      marker = list(color = table["Adjusted.P.value"][1:number_enrichr,], colorscale = colorscale_enrichment(),
                    colorbar = list(title = "P-value", exponentformat = "power"), showscale = TRUE, reversescale=input$invert_color_enrichment_projection)
    )
    
    fig <- fig %>% layout(yaxis = list(autorange = "reversed", title = 'Enrichment', tickfont = list(size = 7)),
                          xaxis = list(title = 'Nb genes', tickfont = list(size = 10))
    )
    return(fig)
  } else {
    
    if(nrow(table) < number_enrichr){
      number_enrichr = nrow(table)
    }
    
    overlap <- as.numeric(table["Overlap"][1:number_enrichr,])
    enrichment <- table["Term"][1:number_enrichr,]
    adjusted_p_value = table["Adjusted.P.value"][1:number_enrichr,]
    
    p <- ggplot(head(table,number_enrichr), aes(x = reorder(enrichment, desc(adjusted_p_value)), y = overlap)) + geom_col(aes(fill = adjusted_p_value), width = 0.7) + 
      ggplot2::scale_fill_gradientn(colours = viridis_pal(option = if(input$select_color_IC_enrichment %in% c("A","B","C","D","E","F","G","H")){input$select_color_IC_enrichment}else{"D"})(nrow(table) * ncol(table)), oob=squish) +
      coord_flip() +
      xlab("Enrichment")
    
    return(p)
  }
})

##----------------------------------------------------------------------------##
## Create the colorscale for enrichment
##----------------------------------------------------------------------------##
colorscale_enrichment <- reactive({
  if(input$select_color_IC_enrichment %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
    return(input$select_color_IC_enrichment)
  } else {
    #prepare colorscales
    l = list()
    se = seq(0, 1, (1/(input$enrichment_disp_number-1)))
    col = viridis_pal(option = input$select_color_IC_enrichment)(input$enrichment_disp_number)
    for(i in 1:length(se)){
      l[[i]] = list(se[i],col[i])
    }
    return(l)
  }
})
