output[["ICA_top_IC_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
           box(id = "ICA_top_IC_main_parameters",
               title = tagList(
                 "Main parameters",
                 actionButton(
                   inputId = "ICA_top_IC_main_parameters_info",
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
               uiOutput("ICA_top_IC_main_parameters_gene_number_UI"),
               uiOutput("ICA_top_IC_main_parameters_colorscale_UI"),
               uiOutput("ICA_top_IC_main_parameters_slider_UI")
           )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "heatmap_container",
        title = tagList(
          p("IC top genes heatmap", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "IC_top_gene_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              numericInput('top_IC_heatmap_export_width','width',640,min = 1,max = NA,step = 1,width = NULL),
              numericInput('top_IC_heatmap_export_height','height',480,min = 1,max = NA,step = 1,width = NULL),
              numericInput('top_IC_heatmap_export_scale','scale',1,min = 0.001,max = NA,step = 0.01,width = NULL),
              uiOutput("top_IC_heatmap_export_UI")
            ),
            circle = FALSE,
            icon = icon("download"),
            inline = TRUE,
            size = "xs"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("log_top_IC_heatmap_UI"),
              uiOutput("heatmap_top_IC_column_organization_UI"),
              uiOutput("top_IC_kurtosis_filter_UI")
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
        tags$div(
          uiOutput("top_IC_plot_or_message")
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Drop down column organization
##----------------------------------------------------------------------------##

output[["heatmap_top_IC_column_organization_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "top_IC_column_organization",
    label = "Cluster ICs",
    value = FALSE
    )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "heatmap_top_IC_column_organization_UI",
  suspendWhenHidden = FALSE
)

output[["log_top_IC_heatmap_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "log_top_IC_heatmap",
    label = "Log Scale",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "log_top_IC_heatmap_UI",
  suspendWhenHidden = FALSE
)

# kurtosis filter
output[["top_IC_kurtosis_filter_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "top_IC_kurtosis_filter",
    label = "Kurtosis filter",
    value = TRUE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "top_IC_kurtosis_filter_UI",
  suspendWhenHidden = FALSE
)

output[["top_IC_heatmap_export_UI"]] <- renderUI({
  shinyFiles::shinySaveButton(
    "top_IC_heatmap_export",
    label = HTML("<p style='color:black;'>export</p>"),
    title = "png, jpg, jpeg, webp, svg, or pdf",
    filetype = c("png", "jpg", "jpeg", "webp", "svg", "pdf"),
    viewtype = "icon",
    class = "btn-xs",
    style = "margin-right: 3px"
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
#output[["groups_tree_text"]] <- renderText({ "Data not available." })

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_top_gene_info"]], {
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
  title = "IC top genes heatmap",
  text = p("Heatmap representation of the expression of the overall top genes over all IC")
)