##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

Launch_analysis <- reactive({
  data <- readRDS(input$input_file$datapath)
  return(data)
})

observeEvent(input$input_file, {
  Launch_analysis()
})