##--------------------------------------------------------------------------##
## colors palette load
##--------------------------------------------------------------------------##
update_palette = reactive({
  req(input$palette_colors_1)
  
  n = 1
  values$palette = c()
  colors = input[["palette_colors_1"]]
  
  while(length(colors) != 0){
    n = n + 1
    
    values$palette = c(values$palette,colors)
    
    colors = input[[paste0("palette_colors_",n)]]
    
  }
  
  saveRDS(values$palette,paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))
  
  return(values$palette)
})
