##--------------------------------------------------------------------------##
## Load shiny library.
##--------------------------------------------------------------------------##

if(Shiny.options[["offline_mode"]] == FALSE){
  if(!file.exists(miniconda_path())){
    reticulate::install_miniconda()
  }
  
  if(!reticulate::py_module_available("kaleido")){
    if(Sys.info()['sysname'] == "Darwin" | Sys.info()['sysname'] == "Linux"){
      reticulate::conda_install('r-reticulate', 'python-kaleido')
    } else {
      reticulate::conda_install('r-reticulate', 'python-kaleido=0.1.0')
    }
    reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
  }
  
  # if(!reticulate::py_module_available("leidenalg")){
  #   reticulate::conda_install('r-reticulate', 'leidenalg')
  # }
  
  reticulate::use_miniconda('r-reticulate')
  
  library(enrichR)
}

##----------------------------------------------------------------------------##
## Load UI content for each tab.
##----------------------------------------------------------------------------##
source(paste0(Shiny.options[["shiny_root"]], "/load_file/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Preprocessing/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA_table/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Visualisation/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Output/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Table/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Marker_table/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/About/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Signal_analysis/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Interaction_analysis/UI.R"), local = TRUE)

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  dashboardHeader(title = "CellsFromSpace"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Load Data", tabName = "Load_file", icon = icon("spinner")),
      menuItem("Pre-Processing", tabName = "Preprocessing", icon = icon("gears")),
      menuItem("ICA", tabName = "ICA", icon = icon("wave-square")),
      menuItem("IC gene weights", tabName = "ICA_table", icon = icon("table-list")),
      menuItem("ICA Table", tabName = "Table", icon = icon("table-list")),
      menuItem("Distances analysis", tabName = "interaction_analysis", icon = icon("wave-square")),
      menuItem("Signal analysis", tabName = "signal_analysis", icon = icon("wave-square")),
      menuItem("Visualization", tabName = "Visualization", icon = icon("display")),
      menuItem("Marker table", tabName = "Marker_table", icon = icon("table-list")),
      menuItem("Ouput", tabName = "Output", icon = icon("arrow-up-from-bracket")),
      menuItem("About", tabName = "About", icon = icon("bars"))
    ),
    uiOutput("IC_list_UI"),
    
    selectizeInput("Plot_image_spatial", "Sample to display", NULL, selected = NULL, multiple = TRUE),
    
    actionButton("select_all_samples_image_spatial", "Select all samples"),
    
    shinyWidgets::awesomeCheckbox(
      inputId = "spatial_mirror_X",
      label = "mirror X",
      value = FALSE
    ),
    shinyWidgets::awesomeCheckbox(
      inputId = "spatial_mirror_Y",
      label = "mirror Y",
      value = FALSE
    ),
    shinyWidgets::awesomeCheckbox(
      inputId = "spatial_flip",
      label = "90° turn",
      value = FALSE
    )
  ),
  dashboardBody(
    tabItems(
      tab_load,
      tab_preprocessing,
      tab_ICA,
      tab_ICA_table,
      tab_table,
      tab_interaction_analysis,
      tab_signal_analysis,
      tab_visualisation,
      tab_marker_table,
      tab_output,
      tab_about
    )
  )
)




