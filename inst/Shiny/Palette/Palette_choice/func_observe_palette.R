##----------------------------------------------------------------------------##
## reset palette to default
##----------------------------------------------------------------------------##

update_palette <- reactive({
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

observeEvent(update_palette(),{

  values$palette = update_palette()

  saveRDS(update_palette(),paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))

})
