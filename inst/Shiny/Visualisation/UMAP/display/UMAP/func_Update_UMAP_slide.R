##----------------------------------------------------------------------------##
## Get information when sliding UMAP
##----------------------------------------------------------------------------##

# search for the cells that were selected while in density
square_cell_UMAP_selected <- reactive({
  return(plotly::event_data(c("plotly_selected"), source = "G"))
})


##----------------------------------------------------------------------------##
## Get information when sliding spatial display
##----------------------------------------------------------------------------##

# search for the cells that were selected while in density
square_cell_UMAP_spatial <- reactive({
  if(!is.null(plots$spatial)){
    return(plotly::event_data(c("plotly_brushed"), source = "C"))
  }
})

observeEvent(square_cell_UMAP_spatial(), {
  table = square_cell_UMAP_spatial()
  
  shinyalert(html = TRUE, text = HTML(paste0("x: ", round(table$x[1],2), ", ", round(table$x[2],2), "<br>y: ", round(table$y[2],2), ", ", round(table$y[1],2))))
})

