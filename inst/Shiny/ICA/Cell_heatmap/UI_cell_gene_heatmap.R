output[["spot_gene_heatmap_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      box(id = "spot_gene_heatmap_main_parameters",
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "spot_gene_heatmap_main_parameters_info",
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
          collapsed = TRUE,
          uiOutput("spot_gene_heatmap_main_parameters_UI"),
          uiOutput("spot_gene_heatmap_slider_main_parameters_UI"),
          uiOutput("spot_gene_heatmap_gene_main_parameters_UI"),
          uiOutput("spot_gene_heatmap_color_main_parameters_UI")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "spot_gene_heatmap_container",
        title = tagList(
          p("IC-contributory genes per spot", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "spot_gene_heatmap_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("heatmap_cells_column_organization_UI")
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
        collapsed = TRUE,
        uiOutput("spot_gene_heatmap_plot_or_message")
      )
    )
  )
})

## organize row based on proximity
output[["heatmap_cells_column_organization_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "cells_column_organization",
    label = "Cluster cells",
    value = TRUE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "heatmap_cells_column_organization_UI",
  suspendWhenHidden = FALSE
)


##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["spot_gene_heatmap_plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("spot_gene_heatmap",
                           width = "auto",
                           height = "85vh")
    )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
#output[["groups_tree_text"]] <- renderText({ "Data not available." })

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["spot_gene_heatmap_info"]], {
  showModal(
    modalDialog(
      spot_gene_heatmap_info[["text"]],
      title = spot_gene_heatmap_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
spot_gene_heatmap_info <- list(
  title = "Cell genes based heatmap",
  text = p("Heatmap display of main gene of the currently selected IC in cells")
)