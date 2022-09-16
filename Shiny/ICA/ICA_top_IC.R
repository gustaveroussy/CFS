output[["ICA_top_IC_UI"]] <- renderUI({
  fluidRow(
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
      uiOutput("top_IC_plot_or_message")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["top_IC_plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("top_gene_IC_plot")
    )
})

##----------------------------------------------------------------------------##
## Relationship tree.
##----------------------------------------------------------------------------##

output[["top_gene_IC_plot"]] <- plotly::renderPlotly({
  data <- Launch_analysis()
  plot_ly(
    x = colnames(data@misc[["top_gene_ICA"]]), y = rownames(data@misc[["top_gene_ICA"]]),
    z = data@misc[["top_gene_ICA"]], type = "heatmap"
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
  text = p("Heatmap representation of the expression of the overall top genes overs all IC")
)