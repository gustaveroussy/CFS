##----------------------------------------------------------------------------##
## Tab: Load image
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

# input directory

output[["load_image_UI"]] <- renderUI({
  fluidRow(
    column(12,
           titlePanel("Load high res image"),
           fileInput(
             inputId = "input_image",
             label = "Select image (.png file)",
             multiple = FALSE,
             accept = c(".png"),
             width = '350px',
             buttonLabel = "Browse...",
             placeholder = "No file selected"
           )
    )
  )
})