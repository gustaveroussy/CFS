##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

# input directory

output[["load_data_UI"]] <- renderUI({
  fluidRow(
    column(12,
           titlePanel("Load processed data"),
           fileInput(
             inputId = "input_file",
             label = "Select input data (.rds file)",
             multiple = FALSE,
             accept = c(".rds"),
             width = '350px',
             buttonLabel = "Browse...",
             placeholder = "No file selected"
           ),
           shinyFilesButton(id = "input_file_local", label = "Local load", title = "Select input data (.rds file)", multiple = FALSE)
    )
  )
})