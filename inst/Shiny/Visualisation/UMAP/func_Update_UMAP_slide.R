##----------------------------------------------------------------------------##
## Plot the plotly scatter plot
##----------------------------------------------------------------------------##

# search for the cells that were selected while in density
square_cell_UMAP <- reactive({
  if(!is.null(plots$spatial)){
    return(plotly::event_data(c("plotly_brushed"), source = "C"))
  }
})

observeEvent(square_cell_UMAP(), {
  table = square_cell_UMAP()
  
  shinyalert(html = TRUE, text = HTML(paste0("x: ", round(table$x[1],2), ", ", round(table$x[2],2), "<br>y: ", round(table$y[2],2), ", ", round(table$y[1],2))))
})

