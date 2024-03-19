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
          p("IC weight per spot", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "Spatial_IC_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              numericInput('IC_spatial_heatmap_export_width','width',640,min = 1,max = NA,step = 1,width = NULL),
              numericInput('IC_spatial_heatmap_export_height','height',480,min = 1,max = NA,step = 1,width = NULL),
              numericInput('IC_spatial_heatmap_export_scale','scale',1,min = 0.001,max = NA,step = 0.01,width = NULL),
              uiOutput("IC_spatial_heatmap_export_UI")
            ),
            circle = FALSE,
            icon = icon("download"),
            inline = TRUE,
            size = "xs"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("invert_color_ICA_projection_UI")
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
        uiOutput("Spatial_IC_plot_or_message")
      )
    )
  )
})


##----------------------------------------------------------------------------##
## invert color scale
##----------------------------------------------------------------------------##
output[["invert_color_ICA_projection_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "invert_color_ICA_projection",
    label = "Invert color scale",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "invert_color_ICA_projection_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## export button
##----------------------------------------------------------------------------##
output[["IC_spatial_heatmap_export_UI"]] <- renderUI({
  shinyFiles::shinySaveButton(
    "IC_spatial_heatmap_export",
    label = HTML("<p style='color:black;'>export</p>"),
    title = "png, jpg, jpeg, webp, svg, or pdf",
    filetype = c("pdf", "png", "jpg", "jpeg", "webp", "svg"),
    viewtype = "icon",
    class = "btn-xs",
    style = "margin-right: 3px"
  )
})

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
