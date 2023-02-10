##----------------------------------------------------------------------------##
## marker table
##----------------------------------------------------------------------------##
tab_marker_table <- tabItem(
  tabName = "Marker_table",
  uiOutput("marker_cluster_choice_UI"),
  uiOutput("Volcano_plot_UI"),
  uiOutput("Marker_table_UI")
)