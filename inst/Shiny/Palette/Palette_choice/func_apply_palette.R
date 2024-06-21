########################################
# Color palette
########################################

palette <- reactive({
  req(values$data)
  req(input$palette_colors_1)
  
  n = 1
  palette = c()
  colors = input[["palette_colors_1"]]
  
  while(length(colors) != 0){
    n = n + 1
    
    palette = c(palette,colors)
    
    colors = input[[paste0("palette_colors_",n)]]
  }
  
  saveRDS(palette,paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))
  
  return(palette)
})
