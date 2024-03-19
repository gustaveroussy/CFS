output[["Spatial_gene_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      box(id = "gene_plot_main_parameters",
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "gene_projection_main_parameters_info",
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
          uiOutput("gene_choice_main_parameters_UI"),
          uiOutput("gene_color_choice_main_parameters_UI")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "gene_plot_container",
        title = tagList(
          p("IC related Genes weights per spot", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "Spatial_gene_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              numericInput('spatial_gene_export_width','width',640,min = 1,max = NA,step = 1,width = NULL),
              numericInput('spatial_gene_export_height','height',480,min = 1,max = NA,step = 1,width = NULL),
              numericInput('spatial_gene_export_scale','scale',1,min = 0.001,max = NA,step = 0.01,width = NULL),
              uiOutput("spatial_gene_export_UI")
            ),
            circle = FALSE,
            icon = icon("download"),
            inline = TRUE,
            size = "xs"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("invert_color_gene_projection_UI")
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
        uiOutput("Spatial_gene_plot_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## invert color scale
##----------------------------------------------------------------------------##
output[["invert_color_gene_projection_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "invert_color_gene_projection",
    label = "Invert color scale",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "invert_color_gene_projection_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["spatial_gene_export_UI"]] <- renderUI({
  shinyFiles::shinySaveButton(
    "spatial_gene_export",
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
observeEvent(input[["Spatial_gene_info"]], {
  showModal(
    modalDialog(
      Spatial_gene_info[["text"]],
      title = Spatial_gene_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
Spatial_gene_info <- list(
  title = "Plot gene weight",
  text = p("Plot of gene association with IC over spatial imagery")
)