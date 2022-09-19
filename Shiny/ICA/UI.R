##----------------------------------------------------------------------------##
## Tab: ICA
##----------------------------------------------------------------------------##
tab_ICA <- tabItem(
  tabName = "ICA",
  uiOutput("ICA_top_IC_UI"),
  uiOutput("Spatial_IC_UI"),
  uiOutput("Heatmap_Exp_IC_UI"),
  uiOutput("Heatmap_loadings_gene_IC_UI"),
  uiOutput("FEA_IC_UI"),
  uiOutput("First_Gene_contr_IC_UI")
)