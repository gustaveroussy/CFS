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
  
  return(palette)
})
