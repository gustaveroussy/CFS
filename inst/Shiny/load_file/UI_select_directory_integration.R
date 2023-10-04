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
           shinyDirButton('load_data_file_select_integration', label='Select a folder', title='Please select a folder', multiple=TRUE)
    )
  )
})