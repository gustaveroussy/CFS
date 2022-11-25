##----------------------------------------------------------------------------##
## Tab: Display
##----------------------------------------------------------------------------##
files_to_load <- list.files(
  paste0(Shiny.options[["shiny_root"]], "/Visualisation"),
  pattern = "func_|obj_|UI_|out_|event_",
  full.names = TRUE
)

for ( i in files_to_load ) {
  source(i, local = TRUE)
}