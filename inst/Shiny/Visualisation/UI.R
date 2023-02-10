##----------------------------------------------------------------------------##
## Tab: ICA
##----------------------------------------------------------------------------##
tab_visualisation <- tabItem(
  tabName = "Visualisation",
  uiOutput("Plot_UI"),
  uiOutput("Plot_Spatial_UI"),
  uiOutput("trajectory_UI"),
  uiOutput("trajectory_Spatial_UI") 
)