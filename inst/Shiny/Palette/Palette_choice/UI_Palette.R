output[["Palette_UI"]] <- renderUI({
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
               div(id = "dynamic_palette_ui",
                   lapply(1:length(values$palette), function(n) {
                     div(id = paste0("palette_colors_div_",n),
                          colourInput(inputId = paste0("palette_colors_",n), paste0("Color ",n), value = values$palette[n])
                         )
                   })
                  ),
                 actionButton("reset_palette_default_values", "Reset"),
                 actionButton("remove_palette_values", "Remove Color"),
                 actionButton("add_palette_values", "Add Color"),
               )
           )
    )
})

outputOptions(
  output,
  "Palette_UI",
  suspendWhenHidden = FALSE
)