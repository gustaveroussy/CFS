##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

# input directory

output[["load_data_2_UI"]] <- renderUI({
  fluidRow(
    column(12,
           titlePanel("Load Visium output folder"),
           shinyDirButton('load_data_file_select', label='Select a folder', title='Please select a folder', multiple=FALSE)
    )
  )
})