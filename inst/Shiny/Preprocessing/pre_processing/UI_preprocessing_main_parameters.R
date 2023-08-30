##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["preprocessing_main_parameters_UI"]] <- renderUI({
  websiteLive <- getOption("enrichR.live")
  tagList(
    HTML("<h3><b>Normalisation process</b></h3>"),
    selectInput("preprocessing_specie_select", label = "Select organism", 
                choices = list("Human" = "Hs", "Mice" = "Mm"), 
                selected = "Hs"),
    numericInput("preprocessing_variable_features", label = "Variable Features", value = 2000,
                 min = 10, step = 1),
    HTML("<h3><b>ICA process</b></h3>"),
    numericInput("preprocessing_number_of_ICs", label = "Number of ICs", value = 100,
                 min = 2, step = 1),
    numericInput("preprocessing_maxit", label = "Maximum iterations", value = 600,
                 min = 1, step = 1),
    selectInput("preprocessing_ICA_function", label = "ICA fonction to run", 
                choices = list("icafast", "icaimax", "icajade"), 
                selected = "icafast"),
    numericInput("preprocessing_kurtosis", label = "Kurtosis filter", value = 3,
                 min = 0, step = 0.01),
    numericInput("preprocessing_sd", label = "Genes standard deviation", value = 3,
                 min = 0, step = 0.01),
    HTML("<h3><b>Enrichment process</b></h3>"),
    selectizeInput("preprocessing_database", label = "Enrichment database",
                   choices = if (websiteLive){enrichR::listEnrichrDbs()$libraryName} else {NULL},
                   selected = c("PanglaoDB_Augmented_2021",
                                "CellMarker_Augmented_2021",
                                "Azimuth_Cell_Types_2021",
                                "Tabula_Sapiens",
                                "Tabula_Muris",
                                "GO_Biological_process_2023",
                                "GO_Molecular_Function_2023",
                                "GO_Cellular_Compartment_2023",
                                "MSigDB_Hallmark_2020",
                                "Reactome_2022",
                                "KEGG_2021_Human"), multiple = TRUE,
                   options = NULL),
    actionButton("preprocessing_action_button", label = "Process")
    )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["preprocessing_main_parameters_info"]], {
  showModal(
    modalDialog(
      preprocessing_main_parameters_info[["text"]],
      title = preprocessing_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
preprocessing_main_parameters_info <- list(
  title = "Main parameters for Pre-processing",
  text = HTML("
    Pre-Processing tab.<br>
    <b>Normalisation process :</b>
    <ul>
    <li><b>Select organism:</b> Organism for which the sample originate from</li>
    <li><b>Variable Features:</b> Number of features to pick for SCTransform</li>
    </ul>
    <b>ICA process :</b>
    <ul>
    <li><b>Number of ICs:</b> Number of ICs to extract from the sample</li>
    <li><b>Maximum iterations:</b> Number of iterations of the ICA</li>
    <li><b>ICA fonction to run:</b> Method of ICA</li>
    <li><b>Kurtosis filter:</b> Kurtosis filter to apply at the end of the analysis over ICs</li>
    <li><b>Genes standard deviation:</b> Keep genes expressed more or less than number of standard deviations from the IC.</li>
    </ul>
    <b>Enrichment process :</b>
    <ul>
    <li><b>Enrichment database:</b> Database to use for enrichment analysis</li>
    </ul>
    "
  )
)