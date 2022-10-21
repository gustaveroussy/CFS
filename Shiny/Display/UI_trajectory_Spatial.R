output[["trajectory_Spatial_UI"]] <- renderUI({
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
      box(id = "trajectory_spatial_container",
        title = tagList(
          p("trajectory_Spatial", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "trajectory_Spatial_info",
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
        uiOutput("trajectory_Spatial_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["trajectory_Spatial_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("trajectory_Spatial", height = '900px', width = 'auto')
    )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
#output[["groups_tree_text"]] <- renderText({ "Data not available." })

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["trajectory_Spatial_info"]], {
  showModal(
    modalDialog(
      trajectory_spatial_info[["text"]],
      title = trajectory_spatial_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
trajectory_spatial_info <- list(
  title = "trajectory Spatial",
  text = p("Heatmap representation of the expression of the overall top genes overs all IC")
)