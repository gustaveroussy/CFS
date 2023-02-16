##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

output[["top_gene_IC_plot"]] <- plotly::renderPlotly({
  
  data_heat = top_IC_heatmap_table()
  
  if (input$top_IC_column_organization == TRUE){
    fig = heatmaply(data_heat,
                    xlab = "ICs",
                    ylab = "Genes",
                    colors = eval(parse(text=paste0(input$select_color_IC_top,"(n=256)"))),
                    scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(colours = eval(parse(text=paste0(input$select_color_IC_top,"(n=256)"))), limits=c(input$slider_IC_top_range[1], input$slider_IC_top_range[2]), oob=squish),
                    hclust_method = "ward.D"
    )
  } else {
    fig <- plot_ly(
      x = colnames(data_heat), y = rownames(data_heat),
      z = data_heat, type = "heatmap", zmin = input$slider_IC_top_range[1], zmax = input$slider_IC_top_range[2],
      colorscale = if(input$select_color_IC_top == "viridis"){"Viridis"}else{input$select_color_IC_top},
      hovertemplate = paste(
        "Gene: %{y:.2f%}<br>",
        "IC: %{x:.2f%}<br>",
        "Value: %{z:.2f%}",
        "<extra></extra>"
      )
    )
  }
  
  return(fig)
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["top_IC_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("top_gene_IC_plot",
                         width = "auto",
                         height = "85vh")
  )
})

##----------------------------------------------------------------------------##
## Reactive Top IC genes heatmap table
##----------------------------------------------------------------------------##

top_IC_heatmap_table <- reactive({
  
  req(values$data)
  req(input$select_number_IC_top_heatmap)
  
  data <- values$data
  
  list_gene <-  purrr::map(data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>3))],function(.x){x<-.x %>% arrange(desc(abs(Sig))) %>% head(n=input$select_number_IC_top_heatmap) ;return(x$gene)}) %>% unlist %>% unique
  
  data_heat=data@reductions$ica@feature.loadings[as.matrix(list_gene),]
  
  if (input$top_IC_kurtosis_filter){
    data_heat = data_heat[,names(which(data@misc$GeneAndStat$Kurtosis_ICs > 3))]
  }
  
  if(input$log_top_IC_heatmap == TRUE){
    data_heat = log10(data_heat)
    data_heat[is.nan(data_heat)] <- 0
  }
  
  return(data_heat)
})
