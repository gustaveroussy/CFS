output[["IC_choice_UI"]] <- renderUI({
  selectInput(
    "IC_choice",
    label = "Choose IC to observe",
    choices = values$IC_names,
    width = '100%'
  )
})