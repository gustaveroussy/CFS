##----------------------------------------------------------------------------##
## gene heatmap
##----------------------------------------------------------------------------##

output[["IC_gene_heatmap_interactive"]] <- plotly::renderPlotly({
  return(heatmap_IC_gene_react())
})

output[["IC_gene_heatmap"]] <- shiny::renderPlot({
  plot(heatmap_IC_gene_react())
})

heatmap_IC_gene_react <- reactive({
  req(values$data)
  req(input$IC_choice)
  
  IC_C = input[["IC_choice"]]
  
  z <- table_ic_gene_to_return()
  
  mm = min_max_gene_heatmap()
  if(input$heatmap_IC_gene_interactive_display){
    if(mm$max == mm$min){
      if((input$select_color_IC_top %in% c("Blues", "Reds","YlGnBu","YlOrRd"))){
        plot_ly(
          x = colnames(z), y = rownames(z),
          z = z, type = "heatmap",
          colorscale = input$select_color_IC_gene_heatmap,
          hovertemplate = paste(
            "Gene: %{y:.2f%}<br>",
            "IC: %{x:.2f%}<br>",
            "Value: %{z:.2f%}",
            "<extra></extra>"
          ),
          reversescale=input$invert_color_gene_heatmap
        )
      } else {
        plot_ly(
          x = colnames(z), y = rownames(z),
          z = z, type = "heatmap",
          colors = viridis_pal(option = input$select_color_IC_gene_heatmap)(nrow(z) * ncol(z)),
          hovertemplate = paste(
            "Gene: %{y:.2f%}<br>",
            "IC: %{x:.2f%}<br>",
            "Value: %{z:.2f%}",
            "<extra></extra>"
          ),
          reversescale=input$invert_color_gene_heatmap
        )
      }
    } else {
      if((input$select_color_IC_top %in% c("Blues", "Reds","YlGnBu","YlOrRd"))){
        plot_ly(
          x = colnames(z), y = rownames(z),
          z = z, type = "heatmap", zmin = input$slider_IC_gene_heatmap_range[1], zmax = input$slider_IC_gene_heatmap_range[2],
          colorscale = input$select_color_IC_gene_heatmap,
          hovertemplate = paste(
            "Gene: %{y:.2f%}<br>",
            "IC: %{x:.2f%}<br>",
            "Value: %{z:.2f%}",
            "<extra></extra>"
          ),
          reversescale=input$invert_color_gene_heatmap
        )
      } else {
        plot_ly(
          x = colnames(z), y = rownames(z),
          z = z, type = "heatmap", zmin = input$slider_IC_gene_heatmap_range[1], zmax = input$slider_IC_gene_heatmap_range[2],
          colors = viridis_pal(option = input$select_color_IC_gene_heatmap)(nrow(z) * ncol(z)),
          hovertemplate = paste(
            "Gene: %{y:.2f%}<br>",
            "IC: %{x:.2f%}<br>",
            "Value: %{z:.2f%}",
            "<extra></extra>"
          ),
          reversescale=input$invert_color_gene_heatmap
        )
      }
    }
  } else {
    
    
    melt = reshape2::melt(z)
    colnames(melt) = c("Genes", "IC", "value")
    
    ord = rownames(z)
    ord_2 = colnames(z)
    
    melt$Genes <- factor( melt$Genes, levels = ord, labels = ord )
    melt$IC <- factor( melt$IC, levels = ord_2,  labels = ord_2 )
    
    if(input$select_color_IC_gene_heatmap %in% c("A","B","C","D","E","F","G","H")){
      fig = ggplot(melt, aes(IC, Genes)) +
        geom_tile(aes(fill = value)) +
        ggplot2::scale_fill_gradientn(colours = viridis_pal(option = input$select_color_IC_gene_heatmap)(nrow(z) * ncol(z)), oob=squish)
      
    } else {
      fig = ggplot(melt, aes(IC, Genes)) +
        geom_tile(aes(fill = value)) +
        ggplot2::scale_fill_gradientn(colours = viridis_pal(option = "D")(nrow(z) * ncol(z)), oob=squish)
    }
    
    return(fig)
  }
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["IC_gene_heatmap_plot_or_message"]] <- renderUI({
  if(input$heatmap_IC_gene_interactive_display){
    tagList(
      plotly::plotlyOutput("IC_gene_heatmap_interactive",
                           width = "auto",
                           height = "85vh")
    )
  } else {
    tagList(
      shiny::plotOutput("IC_gene_heatmap",
                        width = "auto",
                        height = "85vh")
    )
  }
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

