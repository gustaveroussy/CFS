##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

# input directory

output[["load_data_integration_UI"]] <- renderUI({
  fluidRow(
    column(12,
           titlePanel("Load Visium output folder Integration"),
           shinyDirButton('load_data_file_select_integration', label='Select a folder', title='Please select a folder', multiple=TRUE),
           actionButton("reset_integration_button", "Reset", class = "btn btn-primary"),
           uiOutput("text_integration_f")
    )
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["text_integration_f"]] <- renderText({ values$text_integration_folders })