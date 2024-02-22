##----------------------------------------------------------------------------##
## Sample based dotplot
##----------------------------------------------------------------------------##

output[["sample_based_dotplot"]] <- plotly::renderPlotly({
  return(sample_based_dotplot_react())
})

sample_based_dotplot_react <- reactive({
  
  IC_anno <- values$data@misc$annotation
  IC_S <- as.data.frame(values$data@reductions$ica@cell.embeddings)[,rownames(IC_anno)]
  IC_S_long <- IC_S %>% rownames_to_column(var = "spot") %>% tidyr::separate(spot, into = c("sample", "spot"), sep = "\\_(?!.*_)", remove = FALSE) %>% tidyr::pivot_longer(cols = !c(spot | sample), names_to = "IC", values_to = "weight")

  percentil = IC_S_long %>% summarise(.by = c(sample,IC), quantile = scales::percent(c(0.90)), percent = quantile(weight, c(0.90)), weight = sum(weight > percent)/sum(IC_S_long$weight > percent))
  
  x = percentil$sample
  y = percentil$IC
  
  fig <- plot_ly()
  
  fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                           x = x,
                           y = y,
                           marker = list(color = percentil$percent,
                                         colorscale = colorscale_sample_based_dotplot(),
                                         showscale = TRUE,
                                         size = percentil$weight*1000,
                                         reversescale=F),
                           text = percentil$weight*100,
                           customdata = percentil$percent,
                           hovertemplate = paste0("90th percentile weight : %{customdata}<br>",
                                                  "% of spot > 90% percentile: %{text}",
                                                  "<extra></extra>")
  )
  
  fig <- fig %>% layout(title = "IC dotplot by samples", xaxis=list(title = 'Sample', showgrid = FALSE, showticklabels=TRUE),
                        yaxis = list(title = 'IC', showgrid = TRUE, showticklabels=TRUE),
                        showlegend = FALSE)

  return(fig)
})

##----------------------------------------------------------------------------##
## Create the colorscale for dotplot
##----------------------------------------------------------------------------##
colorscale_sample_based_dotplot <- reactive({
  if(input$select_color_sample_based_dotplot %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
    return(input$select_color_sample_based_dotplot)
  } else {
    #prepare colorscales
    l = list()
    se = seq(0, 1, (1/(5000-1)))
    col = viridis_pal(option = input$select_color_sample_based_dotplot)(5000)
    for(i in 1:length(se)){
      l[[i]] = list(se[i],col[i])
    }
    return(l)
  }
})