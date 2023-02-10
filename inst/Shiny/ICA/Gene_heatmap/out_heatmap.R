##----------------------------------------------------------------------------##
## gene heatmap
##----------------------------------------------------------------------------##

output[["IC_gene_heatmap"]] <- plotly::renderPlotly({
  req(values$data)
  req(input$IC_choice)
  
  IC_C = input[["IC_choice"]]
  
  z <- table_ic_gene_to_return()
  
  plot_ly(
    x = colnames(z), y = rownames(z),
    z = z, type = "heatmap", zmin = input$slider_IC_gene_heatmap_range[1], zmax = input$slider_IC_gene_heatmap_range[2],
    colorscale = input$select_color_IC_gene_heatmap,
    hovertemplate = paste(
      "Gene: %{y:.2f%}<br>",
      "IC: %{x:.2f%}<br>",
      "Value: %{z:.2f%}",
      "<extra></extra>"
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["IC_gene_heatmap_plot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("IC_gene_heatmap",
                         width = "auto",
                         height = "85vh")
  )
})

##----------------------------------------------------------------------------##
## gene heatmap text
##----------------------------------------------------------------------------##

output[["IC_text_output"]] <- renderUI({
  IC_C = input[["IC_choice"]]
  Gene <- GeneList_heatmap_IC()
  text <- paste0("Number of genes playing in ", IC_C, " : ", length(Gene),
                 "<br><br>", "Number of genes playing positively in ", IC_C, " : ", length(Gene[Gene > 0])
  )
  return(HTML(text))
})