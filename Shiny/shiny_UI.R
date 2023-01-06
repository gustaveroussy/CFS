##--------------------------------------------------------------------------##
## Load shiny library.
##--------------------------------------------------------------------------##

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyWidgets)
library(shinyalert)
library(pheatmap)
library(Seurat)
library(plotly)
library(raster)
library(RColorBrewer)
library(magick)
library(scatterpie)
library(destiny)
library(rclipboard)
library(tibble)
library(DT)
library(interp)
library(stringr)
library(png)

##--------------------------------------------------------------------------##
## Set class to read shiny object from saveForShiny
##--------------------------------------------------------------------------##

setClass("shiny_visium", slots=list(ica="list", images="list"))

##--------------------------------------------------------------------------##
## Functions.
##--------------------------------------------------------------------------##



##----------------------------------------------------------------------------##
## Load UI content for each tab.
##----------------------------------------------------------------------------##
source(paste0(Shiny.options[["shiny_root"]], "/load_file/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Visualisation/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Output/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Table/UI.R"), local = TRUE)

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  dashboardHeader(title = "Shiny"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Load Data", tabName = "Load_file", icon = icon("spinner")),
      menuItem("ICA", tabName = "ICA", icon = icon("wave-square")),
      menuItem("Visualisation", tabName = "Visualisation", icon = icon("display")),
      menuItem("Ouput", tabName = "Output", icon = icon("arrow-up-from-bracket")),
      menuItem("Table", tabName = "Table", icon = icon("table-list"))
    ),
    uiOutput("IC_list_UI")
  ),
  dashboardBody(
    tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 30;

        $("#heatmap_container").height(boxHeight);
        $("#top_gene_IC_plot").height(boxHeight - 20);
        
        $("#IC_plot_container").height(boxHeight);
        $("#Spatial_IC_plot").height(boxHeight - 20);
        
        $("#gene_plot_container").height(boxHeight);
        $("#Spatial_gene_plot").height(boxHeight - 20);
        
        $("#spot_gene_heatmap_container").height(boxHeight);
        $("#spot_gene_heatmap").height(boxHeight - 20);
        
        $("#IC_gene_heatmap_container").height(boxHeight + 50);
        $("#IC_gene_heatmap").height(boxHeight - 20);
        
        $("#IC_enrichment_container").height(boxHeight);
        $("#IC_enrichment").height(boxHeight - 20);
        
        $("#Plot_container").height(boxHeight);
        $("#Plot").height(boxHeight - 20);
        
        $("#Plot_spatial_container").height(boxHeight);
        $("#Plot_Spatial").height(boxHeight - 20);
        
        $("#trajectory_spatial_container").height(boxHeight);
        $("#trajectory").height(boxHeight - 20);
        
        $("#trajectory_container").height(boxHeight);
        $("#trajectory_Spatial").height(boxHeight - 20);
        
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
    tabItems(
      tab_load,
      tab_ICA,
      tab_visualisation,
      tab_output,
      tab_table
    )
  )
)