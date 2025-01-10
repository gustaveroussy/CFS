##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##
files_to_load <- list.files(
  paste0(Shiny.options[["shiny_root"]], "/About"),
  recursive = TRUE,
  pattern = "func_|obj_|UI_|out_|event_",
  full.names = TRUE
)

for ( i in files_to_load ) {
  source(i, local = TRUE)
}

observe({
  output$CFSimage <- renderImage({
    list(src = paste0(Shiny.options[["shiny_root"]], "/../../docs/README_images/CFS_Logo.png"), contentType = 'image/png',width = 400, height = 200,
         alt = "CFS")
  }, deleteFile = FALSE)
})