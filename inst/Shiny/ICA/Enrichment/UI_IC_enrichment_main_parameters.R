##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["IC_enrichment_main_parameters_UI"]] <- renderUI({
  req(values$data)
  tagList(
    selectInput(
      "IC_enrichment_database_choice",
      label = "Choose database related to enrichment",
      choices = names(values$data@misc$IC_1$en)
    )
  )
})

output[["IC_enrichment_color_main_parameters_UI"]] <- renderUI({
  tagList(
    selectInput("select_color_IC_enrichment", label = "Select color", 
                choices = list("Viridis", "Blues", "Reds","YlGnBu","YlOrRd"), 
                selected = "Viridis")
  )
})

output[["IC_enrichment_display_number_main_parameters_UI"]] <- renderUI({
  req(input$IC_choice)
  req(input$p_n_enrichment)
  req(input$IC_enrichment_database_choice)
  tagList(
    numericInput("enrichment_disp_number", label = "Enrichment to display", value = 30, min = 1, max = length(values$data@misc[[input$IC_choice]][[input$p_n_enrichment]][[input$IC_enrichment_database_choice]]), step = 1)
  )
})

output[["IC_enrichment_p_n_main_parameters_UI"]] <- renderUI({
  tagList(
    radioButtons("p_n_enrichment", label = "Gene expression", 
                 choices = list("All" = "en", "Positive" = "en_p", "Negative" = "en_n"),
                 selected = "en")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_enrichment_main_parameters_info"]], {
  showModal(
    modalDialog(
      IC_enrichment_main_parameters_info[["text"]],
      title = IC_enrichment_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_enrichment_main_parameters_info <- list(
  title = "Main parameters for projection",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
    <li><b>Choose database related to enrichment:</b> Choose one of the database that was used in the enrichment process</li>
    <li><b>Select color:</b> Choose the color scale for the P-value</li>
    <li><b>Enrichment to display:</b> Number of enrichment result to display from the most significant to the least significant</li>
    <li><b>Gene expression:</b> choose positively associated or negatively associated genes enrichment to the IC</li>
    </ul>
    "
  )
)