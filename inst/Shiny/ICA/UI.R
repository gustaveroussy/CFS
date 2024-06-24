##----------------------------------------------------------------------------##
## Tab: ICA
##----------------------------------------------------------------------------##
tab_ICA <- tabItem(
  tabName = "ICA",
  uiOutput("IC_choice_UI"),
  DTOutput("Annotation_table_UI"),
  uiOutput("ICA_top_IC_UI"),
  uiOutput("Spatial_IC_UI"),
  uiOutput("Spatial_gene_UI"),
  uiOutput("IC_gene_heatmap_UI"),
  uiOutput("IC_enrichment_UI")
)