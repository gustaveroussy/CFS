output[["trajectory_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
           box(id = "trajectory_main_parameters_UI",
               title = tagList(
                 "Main parameters",
                 actionButton(
                   inputId = "trajectory_main_parameters_info",
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
               uiOutput("trajectory_main_parameters_UI")
           )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "trajectory_container",
        title = tagList(
          p("Trajectory", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "trajectory_info",
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
        uiOutput("trajectory_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["trajectory_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("trajectory", height = "900px", width = 'auto')
    )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["trajectory_info"]], {
  showModal(
    modalDialog(
      trajectory_info[["text"]],
      title = trajectory_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
trajectory_info <- list(
  title = "Plot",
  text = p("Heatmap representation of the expression of the overall top genes overs all IC")
)