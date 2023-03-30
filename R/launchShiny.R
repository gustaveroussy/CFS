#' Launch Shiny
#'
#' Start the shiny for visualization and annotation purposes
#' 
#' @return None
#' 
#' @examples 
#' launchShiny()
#' 
#' @export
launchShiny <- function(
    mode = "open",
    maxFileSize = 15000,
    file_to_load = NULL,
    welcome_message = NULL,
    projections_show_hover_info = TRUE,
    ...
){
  ##--------------------------------------------------------------------------##
  ## Create global variable with options that need to be available inside the
  ## Shiny app.
  ##--------------------------------------------------------------------------##
  Shiny.options <<- list(
    "mode" = mode,
    "file_to_load" = file_to_load,
    "welcome_message" = welcome_message,
    "shiny_root" = paste0(system.file(package = "CFS"),"/Shiny"),
    "projections_show_hover_info" = projections_show_hover_info
  )
  
  ##--------------------------------------------------------------------------##
  ## Allow upload of files up to 800 MB.
  ##--------------------------------------------------------------------------##
  options(shiny.maxRequestSize = maxFileSize * 1024^2)
  
  ##--------------------------------------------------------------------------##
  ## Load server and UI functions.
  ##--------------------------------------------------------------------------##
  source(
    paste0(paste0(system.file(package = "CFS"),"/Shiny"),"/shiny_UI.R"),
    local = TRUE
  )
  source(
    paste0(paste0(system.file(package = "CFS"),"/Shiny"),"/shiny_server.R"),
    local = TRUE
  )
  ##--------------------------------------------------------------------------##
  ## Launch Shiny.
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '##---------------------------------------------------------------------------##\n',
      '## Launching Shiny\n',
      '##---------------------------------------------------------------------------##'
    )
  )
  shiny::shinyApp(
    ui = ui,
    server = server,
    ...
  )
}
