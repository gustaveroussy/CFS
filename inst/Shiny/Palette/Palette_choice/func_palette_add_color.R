##----------------------------------------------------------------------------##
## reset palette to default
##----------------------------------------------------------------------------##

observeEvent(input[["add_palette_values"]],{

  insertUI(
    selector = paste0("#palette_colors_div_",length(values$palette)),
    ui = div(id = paste0("palette_colors_div_",(length(values$palette) + 1)),colourInput(inputId = paste0("palette_colors_",(length(values$palette) + 1)), paste0("Color ",(length(values$palette) + 1)), value = "#FFFFFF"))
  )
  values$palette = c(values$palette,"#FFFFFF")
  saveRDS(values$palette,paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))

})
