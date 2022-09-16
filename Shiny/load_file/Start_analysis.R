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
  name <- input$project_name
  DirIC=paste0(output_path(),"/",name)
  message(DirIC)
  dir.create(DirIC, recursive=FALSE)
  data <- Launch_analysis()
  Stat_data <- Stat_analysis()
  view(data@meta.data)
})