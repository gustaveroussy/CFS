##----------------------------------------------------------------------------##
## UI elements to set main parameters for preprocessing.
##----------------------------------------------------------------------------##

output[["preprocessing_invert_IC_UI"]] <- renderUI({
  tagList(
    selectizeInput("preprocessing_select_inver_IC", label = "IC to invert",
                   choices = colnames(values$data@reductions[["ica"]]@cell.embeddings), multiple = FALSE,
                   options = NULL),
    actionButton("preprocessing_invert_IC_action_button", label = "Invert")
    )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["invert_IC_main_parameters_info"]], {
  showModal(
    modalDialog(
      invert_IC_main_parameters_info[["text"]],
      title = invert_IC_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
invert_IC_main_parameters_info <- list(
  title = "Invert IC",
  text = HTML("
    Tab used to invert the values of ICs if the automated process fails
    "
  )
)