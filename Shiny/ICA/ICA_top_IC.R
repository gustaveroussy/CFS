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
               uiOutput("ICA_top_IC_main_parameters_UI")
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
  
  p <- pheatmap(data@ica[["top_gene_ICA"]],clustering_method = "ward.D",clustering_distance_cols = "correlation")
  
  col_order <- p[["tree_col"]][["order"]]
  row_order <- p[["tree_row"]][["order"]]
  data@ica[["top_gene_ICA"]] <- data@ica[["top_gene_ICA"]][,col_order]
  data@ica[["top_gene_ICA"]] <- data@ica[["top_gene_ICA"]][row_order,]
  
  fig <- plot_ly(
    x = colnames(data@ica[["top_gene_ICA"]]), y = rownames(data@ica[["top_gene_ICA"]]),
    z = data@ica[["top_gene_ICA"]], type = "heatmap", zmin = input$slider_IC_top_range[1], zmax = input$slider_IC_top_range[2],
    colorscale = input$select_color_IC_top,
    hovertemplate = paste(
      "Gene: %{y:.2f%}<br>",
      "IC: %{x:.2f%}<br>",
      "Value: %{z:.2f%}",
      "<extra></extra>"
    )
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