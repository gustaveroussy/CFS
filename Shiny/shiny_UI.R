##--------------------------------------------------------------------------##
## Load shiny library.
##--------------------------------------------------------------------------##

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyWidgets)
library(dplyr)
library(pheatmap)
library(igraph)
library(CellChat)
library(tidyverse)
library(Seurat)
library(pheatmap)
#library(spatialEco)
# library(Cairo)
library(e1071)
# library(patchwork)
library(ggalluvial)
library(NMF)
# library(copykat)
library(enrichR)
library(ape)
#library(Rfast2)
#library(harmony)
#library(furrr)
library(copykat)
library(plotly)
library(reshape2)
options(browser = "firefox")

library(corrplot)
#install.packages("Seurat")

setEnrichrSite("Enrichr")
dbs <- listEnrichrDbs()
websiteLive <- TRUE



n_cores = 8
future::plan("multicore", workers = n_cores)

##--------------------------------------------------------------------------##
## Functions.
##--------------------------------------------------------------------------##

source(paste0(Shiny.options[["shiny_root"]], "/Functions/DoHeatmapICA.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Functions/Minor_functions.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Functions/prepare_data.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Functions/RunICA.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Functions/IC_Stat.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Functions/Show_IC_and_Enrich.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Functions/Cluster_ICA.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Functions/Markers_Clusters_ICA.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/Functions/Copykat.R"), local = TRUE)

##----------------------------------------------------------------------------##
## Load UI content for each tab.
##----------------------------------------------------------------------------##
source(paste0(Shiny.options[["shiny_root"]], "/load_file/UI.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI.R"), local = TRUE)

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  dashboardHeader(title = "Shiny"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Load Data", tabName = "Load_file", icon = icon("spinner")),
      menuItem("ICA", tabName = "ICA", icon = icon("wave-square"))
    )
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
      tab_ICA
    )
  )
)