##----------------------------------------------------------------------------##
## Tab: ICA
##----------------------------------------------------------------------------##
tab_load <- tabItem(
  tabName = "Load_file",
  uiOutput("load_data_UI"),
  uiOutput("output_data_UI"),
  uiOutput("project_name_UI"),
  uiOutput("start_analysis_UI")
)