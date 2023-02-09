output[["preprocessing_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      box(id = "preprocessing_main_parameters",
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "preprocessing_main_parameters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-right: 3px"
            ),
            shinyWidgets::dropdownButton(
              circle = FALSE,
              icon = icon("cog"),
              inline = TRUE,
              size = "xs"
            )
          ),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          height = NULL,
          collapsible = TRUE,
          collapsed = FALSE,
          uiOutput("preprocessing_main_parameters_UI")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "preprocessing_container",
        title = tagList(
          p("Pre-processing", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "preprocessing_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            circle = FALSE,
            icon = icon("cog"),
            inline = TRUE,
            size = "xs"
          )
        ),
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        height = NULL,
        collapsible = TRUE,
        collapsed = FALSE,
        #uiOutput("preprocessing_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
#output[["groups_tree_text"]] <- renderText({ "Data not available." })

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["preprocessing_info"]], {
  showModal(
    modalDialog(
      preprocessing_info[["text"]],
      title = preprocessing_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
preprocessing_info <- list(
  title = "Pre-processing",
  text = HTML("Window for pre-processing data from Visium output")
)
