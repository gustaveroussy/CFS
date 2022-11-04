output[["IC_choice_UI"]] <- renderUI({
  selectInput(
    "IC_choice",
    label = "Choose IC to observe",
    choices = names(Launch_analysis()@misc)[startsWith(names(Launch_analysis()@misc), "IC_")],
    width = '100%'
  )
})