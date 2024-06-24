##----------------------------------------------------------------------------##
## reset palette to default
##----------------------------------------------------------------------------##

observeEvent(input[["remove_palette_values"]],{
  colors = palette()
  removeUI(selector = paste0("#palette_colors_div_",length(colors)))
  colors = palette()
})
