##----------------------------------------------------------------------------##
## top IC heatmap
##----------------------------------------------------------------------------##

# wrapper

output[["top_gene_IC_plot"]] <- plotly::renderPlotly({
  
  data_heat = top_IC_heatmap_table()
  
  if (input$top_IC_column_organization == TRUE){
    
    fig = plotly_figure_ICA_all_heatmaply()
    
    # View(fig)
    
    if(!is.null(highlightcolumn_heatmaply())){

      x = highlightcolumn_heatmaply()
      
      fig[["x"]][["layout"]][["shapes"]][[1]] = list(type = "rect",
                                                     fillcolor = "white", line = list(color = "black"), opacity = 0.1,
                                                     x0 = (x-0.5),
                                                     x1 = (x+0.5), xref = "x",
                                                     y0 = 0, y1 = 0.8, yref = "paper") #(y+0.5)
      
    }
    
  } else {
 
    fig = plotly_figure_ICA_all()
    
    # print(colnames(data_heat))
    
    if(!is.null(highlightcolumn())){
      
      fig <- layout(fig,
                   shapes = list(
                     list(type = "rect",
                          fillcolor = "white", line = list(color = "black"), opacity = 0.1,
                          x0 = which(colnames(data_heat) == highlightcolumn())/ncol(data_heat)-(1/ncol(data_heat)), x1 = which(colnames(data_heat) == highlightcolumn())/ncol(data_heat), xref = "paper",
                          y0 = 0, y1 = 1, yref = "paper")))
                    
    }
    
  }
  
  return(fig)
})

plotly_figure_ICA_all = reactive({
  data_heat = top_IC_heatmap_table()
  
  fig <- plot_ly(source = "F",
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
  
  fig <- fig %>% event_register('plotly_click')
  
  return(fig)
  
})

plotly_figure_ICA_all_heatmaply = reactive({
  data_heat = top_IC_heatmap_table()
  
  fig = heatmaply(data_heat,
                  xlab = "ICs",
                  ylab = "Genes",
                  colors = eval(parse(text=paste0(input$select_color_IC_top,"(n=256)"))),
                  scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(colours = eval(parse(text=paste0(input$select_color_IC_top,"(n=256)"))), limits=c(input$slider_IC_top_range[1], input$slider_IC_top_range[2]), oob=squish),
                  hclust_method = "ward.D"
  )
  
  fig <- fig %>% event_register('plotly_click')
  
  return(fig)
})

highlightcolumn = reactive({
  table = toString(plotly::event_data(c("plotly_click"), source = "F")[3])
  return(table)
})

highlightcolumn_heatmaply = reactive({
  x = plotly::event_data(c("plotly_click"), source = "A")[3]
  x = c(x)$x
  return(x)
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
