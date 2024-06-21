output[["Palette_UI"]] <- renderUI({
  if(file.exists(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))){
    colors = readRDS(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))
  } else {
    colors = readRDS(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/def_palette.RDS"))
  }
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
           box(id = "Boxplot_distances_container",
               title = tagList(
                 p("Color palette", style = "padding-right: 5px; display: inline")
               ),
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               height = NULL,
               collapsible = TRUE,
               collapsed = FALSE,
               tagList(
                 lapply(1:length(colors), function(n) {
                   colourInput(inputId = paste0("palette_colors_",n), paste0("Color ",n), value = colors[n])
                 }),
                 actionButton("reset_palette_default_values", "Reset")
               )
           )
    )
  )
})

outputOptions(
  output,
  "Palette_UI",
  suspendWhenHidden = FALSE
)