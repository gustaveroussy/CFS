##----------------------------------------------------------------------------##
## reset palette to default
##----------------------------------------------------------------------------##

observeEvent(input[["remove_palette_values"]],{
  removeUI(selector = paste0("#palette_colors_div_",length(values$palette)))
  values$palette = values$palette[-length(values$palette)]
  saveRDS(values$palette,paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))
})
