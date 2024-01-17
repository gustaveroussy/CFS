output[["sample_based_dotplot_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
           box(id = "sample_based_dotplot_main_parameters",
               title = tagList(
                 "Main parameters",
                 actionButton(
                   inputId = "sample_based_dotplot_main_parameters_info",
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
               uiOutput("sample_based_dotplot_main_parameters_UI")
           )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
           box(id = "sample_based_dotplot_container",
               title = tagList(
                 p("Sample based dotplot", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "sample_based_dotplot_info",
                   label = "info",
                   icon = NULL,
                   class = "btn-xs",
                   title = "Show additional information for this panel.",
                   style = "margin-right: 3px"
                 ),
                 shinyWidgets::dropdownButton(
                   tags$div(
                     style = "color: black !important;",
                     numericInput('sample_based_dotplot_export_width','width',640,min = 1,max = NA,step = 1,width = NULL),
                     numericInput('sample_based_dotplot_export_height','height',480,min = 1,max = NA,step = 1,width = NULL),
                     numericInput('sample_based_dotplot_export_scale','scale',1,min = 0.001,max = NA,step = 0.01,width = NULL),
                     uiOutput("sample_based_dotplot_export_UI")
                   ),
                   circle = FALSE,
                   icon = icon("download"),
                   inline = TRUE,
                   size = "xs"
                 ),
               ),
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               height = NULL,
               collapsible = FALSE,
               collapsed = FALSE,
               uiOutput("sample_based_dotplot_or_message")
           )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["sample_based_dotplot_or_message"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("sample_based_dotplot",
                         width = "auto",
                         height = "85vh")
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##

output[["sample_based_dotplot_export_UI"]] <- renderUI({
  shinyFiles::shinySaveButton(
    "sample_based_dotplot_export",
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
observeEvent(input[["sample_based_dotplot_info"]], {
  showModal(
    modalDialog(
      sample_based_dotplot_info[["text"]],
      title = sample_based_dotplot_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
sample_based_dotplot_info <- list(
  title = "Sample based dotplot",
  text = HTML("ICA distribution through samples.")
)