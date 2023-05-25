##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

output[["Volcano_plot"]] <- plotly::renderPlotly({
  req(values$marker_gene)
  req(values$UMAP)
  
  table <- values$marker_gene[[(as.integer(input$marker_cluster_choice)+1)]]
  
  gene = rownames(table)
  fold_change = table$avg_log2FC
  p_value = -log10(table$p_val_adj)
  
  input_log_fold_change = input$Volcano_plot_log_fold_change
  input_p_value = -log10(input$Volcano_plot_p_value)
  
  table_2 = head(table[which(table$p_val_adj < input$Volcano_plot_p_value & ( table$avg_log2FC < -input_log_fold_change | table$avg_log2FC > input_log_fold_change)),],input$Volcano_plot_top_gene)
  
  a <- list(
    x = table_2$avg_log2FC,
    y = -log10(table_2$p_val_adj),
    text = rownames(table_2),
    xref = "x",
    yref = "y",
    xanchor = 'left',
    yanchor = 'bottom',
    showarrow = FALSE,
    arrowhead = 7,
    ax = 20,
    ay = -20
  )

  fig <- plot_ly()
  
  fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                           x = fold_change[(fold_change > input_log_fold_change)&(p_value >input_p_value)],
                           y = p_value[(fold_change > input_log_fold_change)&(p_value >input_p_value)],
                           marker = list(color = "green",
                                         showscale = TRUE),
                           opacity = input$Volcano_plot_alpha,
                           text = gene[(fold_change > input_log_fold_change)&(p_value >input_p_value)],
                           customdata = formatC(table$p_val_adj[(fold_change > input_log_fold_change)&(p_value >input_p_value)], format = "e", digits = 2),
                           hovertemplate = paste0("gene: %{text}<br>",
                                                  "Log2 Fold change : %{x}<br>",
                                                  "p-value: %{customdata}",
                                                  "<extra></extra>")
  )
  
  fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                           x = fold_change[(fold_change < (-input_log_fold_change))&(p_value >input_p_value)],
                           y = p_value[(fold_change < (-input_log_fold_change))&(p_value >input_p_value)],
                           marker = list(color = "red",
                                         showscale = TRUE),
                           opacity = input$Volcano_plot_alpha,
                           text = gene[(fold_change < (-input_log_fold_change))&(p_value >input_p_value)],
                           customdata = formatC(table$p_val_adj[(fold_change < (-input_log_fold_change))&(p_value >input_p_value)], format = "e", digits = 2),
                           hovertemplate = paste0("gene: %{text}<br>",
                                                  "Log2 Fold change : %{x}<br>",
                                                  "p-value: %{customdata}",
                                                  "<extra></extra>")
  )
  
  fig <- fig %>% add_trace(type = 'scatter', mode = "markers",
                           x = fold_change[(fold_change > (-input_log_fold_change))&(fold_change < input_log_fold_change)|(p_value < input_p_value)],
                           y = p_value[(fold_change > (-input_log_fold_change))&(fold_change < input_log_fold_change)|(p_value < input_p_value)],
                           marker = list(color = "grey",
                                         showscale = TRUE),
                           opacity = input$Volcano_plot_alpha,
                           text = gene[(fold_change > (-input_log_fold_change))&(fold_change < input_log_fold_change)|(p_value < input_p_value)],
                           customdata = formatC(table$p_val_adj[(fold_change > (-input_log_fold_change))&(fold_change < input_log_fold_change)|(p_value < input_p_value)], format = "e", digits = 2),
                           hovertemplate = paste0("gene: %{text}<br>",
                                                  "Log2 Fold change : %{x}<br>",
                                                  "p-value: %{customdata}",
                                                  "<extra></extra>")
  )

  fig <- fig %>% layout(annotations = a, xaxis=list(showgrid = TRUE, showticklabels=TRUE, range = list(-max(abs(fold_change))-(max(abs(fold_change))/10), max(abs(fold_change))+(max(abs(fold_change))/10))),
                        yaxis = list(showgrid = TRUE, showticklabels=TRUE),
                        showlegend = FALSE, shapes = list(vline(input_log_fold_change, dash="dash"),
                                                          vline((-input_log_fold_change), dash="dash"),
                                                          hline(input_p_value, dash="dash")
                                                          )
  )
    
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Volcano_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("Volcano_plot",
                         width = "auto",
                         height = "85vh")
  )
})