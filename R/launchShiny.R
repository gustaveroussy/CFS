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
    "shiny_root" = "/home/c_thuilliez/Desktop/scRNAS_CT/inst/Shiny/",
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
    "/home/c_thuilliez/Desktop/scRNAS_CT/inst/Shiny/shiny_UI.R",
    local = TRUE
  )
  source(
    "/home/c_thuilliez/Desktop/scRNAS_CT/inst/Shiny/shiny_server.R",
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
