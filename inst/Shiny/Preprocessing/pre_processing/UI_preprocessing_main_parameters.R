##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["preprocessing_main_parameters_UI"]] <- renderUI({
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
    numericInput("preprocessing_maxit", label = "Maximum iterations", value = 100,
                 min = 1, step = 1),
    selectInput("preprocessing_ICA_function", label = "ICA fonction to run", 
                choices = list("icafast", "icaimax", "icajade"), 
                selected = "icafast"),
    numericInput("preprocessing_kurtosis", label = "Kurtosis filter", value = 3,
                 min = 0, step = 0.01),
    HTML("<h3><b>Enrichment process</b></h3>"),
    selectizeInput("preprocessing_database", label = "Enrichment database",
                   choices = listEnrichrDbs()$libraryName,
                   selected = NULL, multiple = TRUE,
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
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
    <li><b>Plot:</b> This option allows to plot the IC over the spatial data</li>
    <li><b>Scatter pie:</b> This option allows to plot a scatter pie of multiple ICs at once</li>
    <li><b>Color range:</b> Color range of the plot</li>
    <li><b>Select color:</b> Type of color scale of the plot</li>
    </ul>
    "
  )
)