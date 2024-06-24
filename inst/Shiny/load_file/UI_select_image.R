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
             label = "Select image (.png/.jpg/.tif file)",
             multiple = FALSE,
             accept = c(".png",".jpg",".tif",".tiff"),
             width = '350px',
             buttonLabel = "Browse...",
             placeholder = "No file selected"
           ),
           shinyFilesButton(id = "input_image_local", label = "Local load", title = "Select image (.png/.jpg/.tif file)", multiple = FALSE)
    )
  )
})