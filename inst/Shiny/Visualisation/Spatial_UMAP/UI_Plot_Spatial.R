output[["Plot_Spatial_UI"]] <- renderUI({
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
      box(id = "Plot_spatial_container",
        title = tagList(
          p("Plot_Spatial", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "Plot_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("Spatial_display_image_UI")
            ),
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
        uiOutput("Plot_Spatial_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Plot_Spatial_or_message"]] <- renderUI({
  if(input$Spatial_use_ggplot){
    tagList(
      shiny::plotOutput("scatter_pie_ggplot_plot")
    )
  } else {
    tagList(
      plotly::plotlyOutput("Plot_Spatial",
                           width = "auto",
                           height = "120vh")
    )
  }
})

##----------------------------------------------------------------------------##
## Drop down column organization
##----------------------------------------------------------------------------##

output[["Spatial_display_image_UI"]] <- renderUI({
  tagList(
    shinyWidgets::awesomeCheckbox(
      inputId = "Spatial_use_ggplot",
      label = "Use ggplot",
      value = TRUE
    ),
    shinyWidgets::awesomeCheckbox(
      inputId = "Spatial_display_image",
      label = "Display image",
      value = TRUE
    ),
    numericInput("Plot_scatter_size_spatial", "Spot size", 10, min = 0, max = NA)
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "Spatial_display_image_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["Plot_Spatial_info"]], {
  showModal(
    modalDialog(
      IC_top_gene_info[["text"]],
      title = IC_top_gene_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_top_gene_info <- list(
  title = "Plot Spatial",
  text = p("Heatmap representation of the expression of the overall top genes overs all IC")
)