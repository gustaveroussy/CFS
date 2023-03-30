##--------------------------------------------------------------------------##
## Load shiny library.
##--------------------------------------------------------------------------##

# library(shiny)
# library(shinydashboard)
# library(shinyFiles)
# library(shinyWidgets)
# library(shinyalert)
# library(pheatmap)
# library(Seurat)
# library(plotly)
# library(raster)
# library(RColorBrewer)
# library(scatterpie)
# library(destiny)
# library(rclipboard)
# library(tibble)
# library(DT)
# library(interp)
# library(stringr)
# library(png)
# library(jpeg)
# library(imagefx)
# library(heatmaply)
# library(scales)
library(enrichR)
# library(e1071)

##--------------------------------------------------------------------------##
## Set class to read shiny object from saveForShiny
##--------------------------------------------------------------------------##

#setClass("shiny_visium", slots=list(ica="list", images="list"))

##--------------------------------------------------------------------------##
## Functions.
##--------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Load UI content for each tab.
##----------------------------------------------------------------------------##
source(paste0(Shiny.options[["shiny_root"]], "/load_file/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Preprocessing/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Visualisation/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Output/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Table/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Marker_table/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/About/UI.R"), local = TRUE)

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  dashboardHeader(title = "SpICA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Load Data", tabName = "Load_file", icon = icon("spinner")),
      menuItem("Pre-Processing", tabName = "Preprocessing", icon = icon("gears")),
      menuItem("ICA", tabName = "ICA", icon = icon("wave-square")),
      menuItem("ICA Table", tabName = "Table", icon = icon("table-list")),
      menuItem("Visualisation", tabName = "Visualisation", icon = icon("display")),
      menuItem("Marker table", tabName = "Marker_table", icon = icon("table-list")),
      menuItem("Ouput", tabName = "Output", icon = icon("arrow-up-from-bracket")),
      menuItem("About", tabName = "About", icon = icon("bars"))
    ),
    uiOutput("IC_list_UI")
  ),
  dashboardBody(
    tabItems(
      tab_load,
      tab_preprocessing,
      tab_ICA,
      tab_table,
      tab_visualisation,
      tab_marker_table,
      tab_output,
      tab_about
    )
  )
)