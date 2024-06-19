output[["Palette_UI"]] <- renderUI({
  def_colors = readRDS(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/def_palette.RDS"))
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
           tagList(
             lapply(1:length(def_colors), function(n) {
               colourInput(inputId = paste0("palette_colors_",n), paste0("Color ",n), value = def_colors[n])
             })
           )
    )
  )
})

outputOptions(
  output,
  "Palette_UI",
  suspendWhenHidden = FALSE
)