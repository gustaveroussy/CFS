##----------------------------------------------------------------------------##
## Server function for Cerebro.
##----------------------------------------------------------------------------##
server <- function(input, output, session) {

##--------------------------------------------------------------------------##
## Tabs.
##--------------------------------------------------------------------------##
source(paste0(Shiny.options[["shiny_root"]], "/load_file/server.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/server.R"), local = TRUE)

  }