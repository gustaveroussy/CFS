##----------------------------------------------------------------------------##
## UI elements to set main parameters for preprocessing.
##----------------------------------------------------------------------------##

output[["preprocessing_rename_sample_UI"]] <- renderUI({
  req(values$data)
  tagList(
    selectInput("preprocessing_sample_to_rename",
                   label = "Sample to rename",
                   choices = names(values$data@images)
                ),
    textInput("preprocessing_rename_text", "Rename to", value = ""),
    actionButton("preprocessing_rename_sample_action_button", label = "Rename")
    )
})



##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["preprocessing_rename_sample_main_parameters_info"]], {
  showModal(
    modalDialog(
      preprocessing_rename_sample_main_parameters_info[["text"]],
      title = preprocessing_rename_sample_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
preprocessing_rename_sample_main_parameters_info <- list(
  title = "Rename Samples",
  text = HTML("
    Tab used to rename samples
    "
  )
)