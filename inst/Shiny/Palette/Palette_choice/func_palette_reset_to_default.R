##----------------------------------------------------------------------------##
## reset palette to default
##----------------------------------------------------------------------------##

observeEvent(input[["reset_palette_default_values"]],{
  colors = readRDS(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/def_palette.RDS"))
  lapply(1:length(colors), function(i) {
    updateColourInput(session, inputId = paste0("palette_colors_",i), paste0("Color ",i), value = colors[i])
  })
})
