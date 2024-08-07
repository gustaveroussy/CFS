##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

values <- reactiveValues(data = NULL, IC_names = NULL, Stat = NULL, Annotation = NULL,
                         annotation_for_output = list(), low_image = NULL, HD_image = NULL, HD_image_2 = NULL,
                         cropped_image = NULL, marker_gene = NULL, integration_folders = NULL, distances = NULL)


observeEvent(input$input_file, {

  load_file_from_path(filepath = input$input_file$datapath)
  
})
