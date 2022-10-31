output[["IC_choice_UI"]] <- renderUI({
  selectInput(
    "IC_choice",
    label = "Choose IC to observe",
    choices = head(names(Launch_analysis()@misc)[-1],-1),
    width = '100%'
  )
})