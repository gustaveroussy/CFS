output[["Plot_Spatial_UI"]] <- renderUI({
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
      box(id = "Plot_spatial_container",
        title = tagList(
          p("Plot Spatial", style = "padding-right: 5px; display: inline"),
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
              numericInput('plot_spatial_export_width','width',640,min = 1,max = NA,step = 1,width = NULL),
              numericInput('plot_spatial_export_height','height',480,min = 1,max = NA,step = 1,width = NULL),
              numericInput('plot_spatial_export_scale','scale',1,min = 0.001,max = NA,step = 0.01,width = NULL),
              uiOutput("plot_spatial_export_UI")
            ),
            circle = FALSE,
            icon = icon("download"),
            inline = TRUE,
            size = "xs"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("invert_color_visualisation_spatial_UI"),
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
      shiny::plotOutput("Plot_Spatial_ggplot",
                        width = "auto",
                        height = "120vh")
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
## invert color scale
##----------------------------------------------------------------------------##
output[["invert_color_visualisation_spatial_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "invert_color_visualisation_spatial",
    label = "Invert color scale",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "invert_color_visualisation_spatial_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Drop down column organization
##----------------------------------------------------------------------------##

output[["Spatial_display_image_UI"]] <- renderUI({
  tagList(
    shinyWidgets::awesomeCheckbox(
      inputId = "Spatial_visualisation_comput",
      label = "Display",
      value = TRUE
    ),
    shinyWidgets::awesomeCheckbox(
      inputId = "Spatial_use_ggplot",
      label = "Use ggplot",
      value = FALSE
    ),
    selectInput(
      inputId = "full_annotation_spatial",
      label = "Select annotation",
      choices = c("None","IC", "Mean IC","Full annotation","Mean full annotation"),
      selected = "Full annotation",
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    ),
    shinyWidgets::awesomeCheckbox(
      inputId = "Spatial_display_image",
      label = "Display image",
      value = TRUE
    ),
    shinyWidgets::awesomeCheckbox(
      inputId = "black_b_scatter_pie",
      label = "Black background for scatter pie",
      value = FALSE
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

# export button
output[["plot_spatial_export_UI"]] <- renderUI({
  shinyFiles::shinySaveButton(
    "plot_spatial_export",
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