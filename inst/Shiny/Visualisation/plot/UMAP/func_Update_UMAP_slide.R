##----------------------------------------------------------------------------##
## Get information when sliding UMAP
##----------------------------------------------------------------------------##

# search for the cells that were selected while in density
selected_cell_UMAP_selected <- reactive({
  return(plotly::event_data(c("plotly_selected"), source = "G"))
})

observeEvent(selected_cell_UMAP_selected(), {
  table = selected_cell_UMAP_selected()
  
  shinyalert(html = TRUE, text = HTML("Region saved"))
  
  if(!("Regions" %in% names(values$data@misc))){
    values$data@misc$Regions = list()
  }
  
  values$data@misc$Regions[[input$name_of_region_UMAP]] = table$customdata

})

##----------------------------------------------------------------------------##
## Get information when sliding spatial display
##----------------------------------------------------------------------------##

# search for the cells that were selected while in density
square_cell_UMAP_spatial <- reactive({
  return(plotly::event_data(c("plotly_brushed"), source = "C"))
})

selected_cell_UMAP_spatial <- reactive({
  return(plotly::event_data(c("plotly_selected"), source = "C"))
})

observeEvent(square_cell_UMAP_spatial(), {
  table = square_cell_UMAP_spatial()
  
  shinyalert(html = TRUE, text = HTML(paste0("x: ", round(table$x[1],2), ", ", round(table$x[2],2), "<br>y: ", round(table$y[2],2), ", ", round(table$y[1],2))))
  
})

observeEvent(selected_cell_UMAP_spatial(), {
  table = selected_cell_UMAP_spatial()
  
  shinyalert(html = TRUE, text = HTML("Region saved"))
  
  if(!("Regions" %in% names(values$data@misc))){
    values$data@misc$Regions = list()
  }
  
  values$data@misc$Regions[[input$name_of_region_spatial]] = table$customdata
  
})

