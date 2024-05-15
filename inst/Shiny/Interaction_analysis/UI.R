##----------------------------------------------------------------------------##
## Tab: Signal analysis
##----------------------------------------------------------------------------##
tab_interaction_analysis <- tabItem(
  tabName = "interaction_analysis",
  uiOutput("IC_distances_UI"),
  uiOutput("IC_spatial_interactions_UI")
)