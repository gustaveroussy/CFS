##----------------------------------------------------------------------------##
## Server function for Cerebro.
##----------------------------------------------------------------------------##
server <- function(input, output, session) {
  
##--------------------------------------------------------------------------##
## Functions
##--------------------------------------------------------------------------##
  
  source(paste0(Shiny.options[["shiny_root"]], "../../R/Cluster_ICA.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "../../R/PseudoTime.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "../../R/prepare_data.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "../../R/Cluster_ICA.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "../../R/RunICA.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "../../R/Show_IC_and_Enrich.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "../../R/Minor_functions.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "./Functions/Prepare_table_marker.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "./Functions/Plotly_line.R"), local = TRUE)

##--------------------------------------------------------------------------##
## Tabs.
##--------------------------------------------------------------------------##
  source(paste0(Shiny.options[["shiny_root"]], "/load_file/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Preprocessing/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/ICA/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Visualisation/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Output/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Table/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Marker_table/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/About/server.R"), local = TRUE)
  
########################################
# IC_list
########################################
  
  output[["IC_list_UI"]] <- renderUI({
    selectizeInput("Ic_list", label = "list of IC",
                    choices = values$IC_names,
                    selected = NULL,
                    multiple = TRUE,
                    options = NULL)
  })

  }