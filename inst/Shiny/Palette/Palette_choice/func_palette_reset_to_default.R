##----------------------------------------------------------------------------##
## reset palette to default
##----------------------------------------------------------------------------##

observeEvent(input[["reset_palette_default_values"]],{
  values$palette = readRDS(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/def_palette.RDS"))
  lapply(1:length(values$palette), function(i) {
    updateColourInput(session, inputId = paste0("palette_colors_",i), paste0("Color ",i), value = values$palette[i])
  })
  i = length(values$palette) + 1
  while(!is.null(input[[paste0("palette_colors_",i)]])){
    removeUI(selector = paste0("#palette_colors_div_",i))
    i = i + 1
  }

  saveRDS(values$palette,paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))
  
})
