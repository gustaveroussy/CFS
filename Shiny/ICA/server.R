##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##
source(paste0(Shiny.options[["shiny_root"]], "/ICA/ICA_top_IC.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/Spatial_IC.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/Spatial_gene.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/IC_gene_weight.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/funct_Reactive.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI_spot_gene_weight.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI_Enrichment.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI_IC_enrichment_main_parameters.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI_spot_gene_weight_main_parameters.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI_IC_gene_weight_main_parameters.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI_gene_projection_main_parameters.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI_IC_projection_main_parameters.R"), local = TRUE)
source(paste0(Shiny.options[["shiny_root"]], "/ICA/UI_ICA_top_IC_main_parameters.R"), local = TRUE)