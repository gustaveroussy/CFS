output[["Spatial_IC_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      box(id = "IC_plot_main_parameters",
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "IC_projection_main_parameters_info",
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
          uiOutput("IC_projection_main_parameters_UI"),
          uiOutput("pie_chart_check")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "IC_plot_container",
        title = tagList(
          p("Plot IC weight", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "Spatial_IC_info",
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
        uiOutput("Spatial_IC_plot_or_message")
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
observeEvent(input[["Spatial_IC_info"]], {
  showModal(
    modalDialog(
      Spatial_IC_info[["text"]],
      title = Spatial_IC_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
Spatial_IC_info <- list(
  title = "Plot IC weight",
  text = HTML("Plot of IC weight over spatial imagery")
)
