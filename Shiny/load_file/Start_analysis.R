##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

output[["start_analysis_UI"]] <- renderUI({
  fluidPage(
    mainPanel(
      actionButton("start_analysis", "Start analysis")
    ))
})

observeEvent(input$start_analysis, {
  message(input$input_file)
})