output[["IC_gene_heatmap_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      box(id = "IC_gene_heatmap_main_parameters",
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "IC_gene_heatmap_main_parameters_info",
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
          uiOutput("IC_gene_heatmap_slider_main_parameters_UI"),
          uiOutput("IC_gene_heatmap_number_main_parameters_UI"),
          uiOutput("IC_gene_heatmap_color_main_parameters_UI")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "IC_gene_heatmap_container",
        title = tagList(
          p("IC-specific heatmap", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "IC_gene_heatmap_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              numericInput('heatmap_IC_gene_export_width','width',640,min = 1,max = NA,step = 1,width = NULL),
              numericInput('heatmap_IC_gene_export_height','height',480,min = 1,max = NA,step = 1,width = NULL),
              numericInput('heatmap_IC_gene_export_scale','scale',1,min = 0.001,max = NA,step = 0.01,width = NULL),
              uiOutput("heatmap_IC_gene_export_UI")
            ),
            circle = FALSE,
            icon = icon("download"),
            inline = TRUE,
            size = "xs"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("heatmap_IC_gene_column_organization_UI"),
              uiOutput("invert_color_gene_heatmap_UI")
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
        uiOutput("IC_gene_heatmap_plot_or_message"),
        fluidRow(
          column(width = 6, offset = 1, style = "padding: 0px;",
                 uiOutput("IC_text_output")
          ),
          column(width = 5, offset = 0, style = "padding: 0px;",
                 uiOutput("clip_all"),
                 uiOutput("clip_p")
          )
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## invert color scale
##----------------------------------------------------------------------------##
output[["invert_color_gene_heatmap_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "invert_color_gene_heatmap",
    label = "Invert color scale",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "invert_color_gene_heatmap_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Drop down column organization
##----------------------------------------------------------------------------##

output[["heatmap_IC_gene_column_organization_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "IC_gene_column_organization",
    label = "Cluster ICs",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "heatmap_IC_gene_column_organization_UI",
  suspendWhenHidden = FALSE
)

# output button
output[["heatmap_IC_gene_export_UI"]] <- renderUI({
  shinyFiles::shinySaveButton(
    "heatmap_IC_gene_export",
    label = HTML("<p style='color:black;'>export</p>"),
    title = "png, jpg, jpeg, webp, svg, or pdf",
    filetype = c("pdf", "png", "jpg", "jpeg", "webp", "svg"),
    viewtype = "icon",
    class = "btn-xs",
    style = "margin-right: 3px"
  )
})

##----------------------------------------------------------------------------##
## gene heatmap clipboard
##----------------------------------------------------------------------------##

# Add clipboard buttons
output$clip_all <- renderUI({
  output$clip_all <- renderUI({
    rclipButton(
      inputId = "clipa",
      label = "Gene Clipboard",
      clipText = toString(names(GeneList_heatmap_IC())), 
      icon = icon("clipboard")
    )
  })
})

# Add clipboard buttons
output$clip_p <- renderUI({
  output$clip_p <- renderUI({
    rclipButton(
      inputId = "clipp",
      label = "Positive Gene Clipboard",
      clipText = toString(names(GeneList_heatmap_IC())), 
      icon = icon("clipboard")
    )
  })
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
#output[["groups_tree_text"]] <- renderText({ "Data not available." })

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_gene_heatmap_info"]], {
  showModal(
    modalDialog(
      IC_gene_heatmap_info[["text"]],
      title = IC_gene_heatmap_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_gene_heatmap_info <- list(
  title = "Plot gene weight",
  text = p("Heatmap display of genes associated to the IC")
)