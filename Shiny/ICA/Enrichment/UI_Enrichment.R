output[["IC_enrichment_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      box(id = "IC_enrichment_main_parameters",
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "IC_enrichment_main_parameters_info",
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
          uiOutput("IC_enrichment_main_parameters_UI"),
          uiOutput("IC_enrichment_color_main_parameters_UI"),
          uiOutput("IC_enrichment_display_number_main_parameters_UI"),
          uiOutput("IC_enrichment_p_n_main_parameters_UI")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "IC_enrichment_container",
        title = tagList(
          p("Build bar chart of enrichment", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "IC_enrichment_info",
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
        uiOutput("IC_enrichment_plot_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["IC_enrichment_plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("IC_enrichment")
    )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
#output[["groups_tree_text"]] <- renderText({ "Data not available." })

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_enrichment_info"]], {
  showModal(
    modalDialog(
      IC_enrichment_info[["text"]],
      title = IC_enrichment_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_enrichment_info <- list(
  title = "Plot gene weight",
  text = p("Plot of gene weight over spatial imagery")
)