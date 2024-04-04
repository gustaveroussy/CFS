##----------------------------------------------------------------------------##
## Server
##----------------------------------------------------------------------------##
server <- function(input, output, session) {
  
##--------------------------------------------------------------------------##
## Functions
##--------------------------------------------------------------------------##

  source(paste0(Shiny.options[["shiny_root"]], "/Functions/Prepare_table_marker.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Functions/Plotly_line.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Functions/alpha_color_scale.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Functions/Plotly_colorscale.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Functions/associate_signal_with_IC.R"), local = TRUE)

##--------------------------------------------------------------------------##
## Tabs.
##--------------------------------------------------------------------------##
  source(paste0(Shiny.options[["shiny_root"]], "/load_file/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Preprocessing/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/ICA/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/ICA_table/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Visualisation/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Output/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Table/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Marker_table/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/About/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Signal_analysis/server.R"), local = TRUE)
  
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

########################################
# observe sample button
########################################
  
  observeEvent(input$select_all_samples_image_spatial, {
    req(values$data)
    updateSelectizeInput(
      session = getDefaultReactiveDomain(),
      "Plot_image_spatial",
      selected = images_names(),
      choices = images_names()
    )
  })

  }