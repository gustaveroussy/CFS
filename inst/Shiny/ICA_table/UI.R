##----------------------------------------------------------------------------##
## Tab: ICA
##----------------------------------------------------------------------------##
tab_ICA_table <- tabItem(
  tabName = "ICA_table",
  uiOutput("ICA_genes_table_UI"),
  uiOutput("sample_based_dotplot_UI")
)