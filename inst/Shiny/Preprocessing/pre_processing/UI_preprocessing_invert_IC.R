##----------------------------------------------------------------------------##
## UI elements to set main parameters for preprocessing.
##----------------------------------------------------------------------------##

output[["preprocessing_invert_IC_UI"]] <- renderUI({
  req(values$data)
  req(values$data@reductions$ica)
  tagList(
    selectizeInput("preprocessing_invert_IC", label = "IC to invert",
                   choices = colnames(values$data@reductions$ica@cell.embeddings),
                   multiple = TRUE,
                   options = NULL),
    actionButton("preprocessing_invert_IC_action_button", label = "Action")
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
    To be used if an IC is inverted compared to what is expected from the signal.<br>
    <b>IC to invert :</b> Select the IC to invert</li><br>
    <b>Action:</b> Starts the inversion
    "
  )
)