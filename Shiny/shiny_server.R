##----------------------------------------------------------------------------##
## Server function for Cerebro.
##----------------------------------------------------------------------------##
server <- function(input, output, session) {
  
##--------------------------------------------------------------------------##
## Functions
##--------------------------------------------------------------------------##
  
  source(paste0(Shiny.options[["shiny_root"]], "/Functions/Cluster_ICA.R"), local = TRUE)

##--------------------------------------------------------------------------##
## Tabs.
##--------------------------------------------------------------------------##
  source(paste0(Shiny.options[["shiny_root"]], "/load_file/server.R"), local = TRUE)

  source(paste0(Shiny.options[["shiny_root"]], "/ICA/server.R"), local = TRUE)
  source(paste0(Shiny.options[["shiny_root"]], "/Display/server.R"), local = TRUE)
  
  ICA_list <- reactive({
    req(input$input_file$datapath)
    x <- names(Launch_analysis()@misc[3:length(Launch_analysis()@misc)-1])
    return(x)
  })
  
  output[["IC_list_UI"]] <- renderUI({
    selectizeInput("Ic_list", label = "list of IC",
                    choices = ICA_list(),
                    selected = NULL,
                    multiple = TRUE,
                    options = NULL)
  })

  }