##----------------------------------------------------------------------------##
## Get information when sliding UMAP
##----------------------------------------------------------------------------##

# search for the cells that were selected while in density
selected_cell_UMAP_selected <- reactive({
  return(plotly::event_data(c("plotly_selected"), source = "G"))
})

observeEvent(selected_cell_UMAP_selected(), {
  table = selected_cell_UMAP_selected()
  
  if(!("Regions" %in% names(values$data@misc))){
    values$data@misc$Regions = list()
  }
  
  values$data@misc$Regions[[input$name_of_region_UMAP]] = table$customdata
  
  shinyalert(html = TRUE, text = HTML("Region saved"))

})

##----------------------------------------------------------------------------##
## Get information when sliding spatial display
##----------------------------------------------------------------------------##

# search for the cells that were selected while in density
selected_cell_UMAP_spatial <- reactive({
  return(plotly::event_data(c("plotly_selected"), source = "C"))
})

observeEvent(selected_cell_UMAP_spatial(), {
  table = selected_cell_UMAP_spatial()
  
  if(!("Regions" %in% names(values$data@misc))){
    values$data@misc$Regions = list()
  }
  
  values$data@misc$Regions[[input$name_of_region_spatial]] = table$customdata
  
  shinyalert(html = TRUE, text = HTML("Region saved"))
  
})

