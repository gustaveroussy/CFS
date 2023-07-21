output[["Plot_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
           box(id = "Plot_main_parameters_UI",
               title = tagList(
                 "Main parameters",
                 actionButton(
                   inputId = "Plot_main_parameters_info",
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
               uiOutput("Plot_type_UI"),
               uiOutput("Plot_main_parameters_UI"),
               uiOutput("Plot_main_parameters_3_UI"),
               uiOutput("Plot_main_parameters_2_UI"),
               uiOutput("start_plot_UI")
           )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "Plot_container",
        title = tagList(
          p("Plot", style = "padding-right: 5px; display: inline"),
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
              numericInput('plot_export_width','width',640,min = 1,max = NA,step = 1,width = NULL),
              numericInput('plot_export_height','height',480,min = 1,max = NA,step = 1,width = NULL),
              numericInput('plot_export_scale','scale',1,min = 0.001,max = NA,step = 0.01,width = NULL),
              uiOutput("plot_export_UI")
            ),
            circle = FALSE,
            icon = icon("download"),
            inline = TRUE,
            size = "xs"
          ),
          shinyWidgets::dropdownButton(
            circle = FALSE,
            icon = icon("cog"),
            inline = TRUE,
            size = "xs",
            tags$div(
              style = "color: black !important;",
              uiOutput("invert_color_visualisation_UMAP_UI"),
              uiOutput("ggplot_scatter_pie_UI")
            )
          )
        ),
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        height = NULL,
        collapsible = TRUE,
        collapsed = FALSE,
        uiOutput("Plot_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## invert color scale
##----------------------------------------------------------------------------##
output[["invert_color_visualisation_UMAP_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "invert_color_visualisation_UMAP",
    label = "Invert color scale",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "invert_color_visualisation_UMAP_UI",
  suspendWhenHidden = FALSE
)

output[["ggplot_scatter_pie_UI"]] <- renderUI({
  tagList(
    shinyWidgets::awesomeCheckbox(
      inputId = "show_grid_scatter_pie",
      label = "Display plotly grid",
      value = TRUE
    ),
    shinyWidgets::awesomeCheckbox(
      inputId = "ggplot_scatter_pie",
      label = "Use ggplot scatter pie",
      value = FALSE
    ),
    shinyWidgets::awesomeCheckbox(
      inputId = "full_annotation_UMAP",
      label = "Full annotation for scatter",
      value = FALSE
    ),
    numericInput("Plot_scatter_size_UMAP", "Spot size", 10, min = 0, max = NA)
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "ggplot_scatter_pie_UI",
  suspendWhenHidden = FALSE
)

# export button
output[["plot_export_UI"]] <- renderUI({
  shinyFiles::shinySaveButton(
    "plot_export",
    label = HTML("<p style='color:black;'>export</p>"),
    title = "png, jpg, jpeg, webp, svg, or pdf",
    filetype = c("pdf", "png", "jpg", "jpeg", "webp", "svg"),
    viewtype = "icon",
    class = "btn-xs",
    style = "margin-right: 3px"
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("Plot",
                           width = "auto",
                           height = "85vh")
    )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["Plot_info"]], {
  showModal(
    modalDialog(
      Plot_info[["text"]],
      title = Plot_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
Plot_info <- list(
  title = "Plot",
  text = p("Window displaying diverse representation of data")
)