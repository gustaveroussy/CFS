##----------------------------------------------------------------------------##
## reset palette to default
##----------------------------------------------------------------------------##

observeEvent(input[["add_palette_values"]],{
  colors = palette()
  insertUI(
    selector = "#dynamic_palette_ui",
    ui = colourInput(inputId = paste0("palette_colors_",length(colors) + 1), paste0("Color ",length(colors) + 1), value = colors[length(colors) + 1])
  )
  colors = palette()
})
