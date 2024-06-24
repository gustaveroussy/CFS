##----------------------------------------------------------------------------##
## reset palette to default
##----------------------------------------------------------------------------##

observeEvent(input[["reset_palette_default_values"]],{
  colors = readRDS(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/def_palette.RDS"))
  lapply(1:length(colors), function(i) {
    updateColourInput(session, inputId = paste0("palette_colors_",i), paste0("Color ",i), value = colors[i])
  })
  i = length(colors) + 1
  while(!is.null(input[[paste0("palette_colors_",i)]])){
    removeUI(selector = paste0("#palette_colors_div_",i))
    i = i + 1
  }
  colors = palette()
})
