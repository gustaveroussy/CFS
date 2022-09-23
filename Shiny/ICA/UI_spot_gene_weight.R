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
          collapsed = FALSE,
          uiOutput("spot_gene_heatmap_main_parameters_UI"),
          uiOutput("spot_gene_heatmap_slider_main_parameters_UI"),
          uiOutput("spot_gene_heatmap_color_main_parameters_UI")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "spot_gene_heatmap_container",
        title = tagList(
          p("Build heatmap of genes related to spot", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "spot_gene_heatmap_info",
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
        uiOutput("spot_gene_heatmap_plot_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["spot_gene_heatmap_plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("spot_gene_heatmap")
    )
})

##----------------------------------------------------------------------------##
## Relationship tree.
##----------------------------------------------------------------------------##

output[["spot_gene_heatmap"]] <- plotly::renderPlotly({
  
  data <- Launch_analysis()
  IC_C = input[["spot_gene_heatmap_IC_choice"]]
  
  p <- pheatmap(data@misc[[IC_C]]$spot_top_genes_weight,clustering_method = "ward.D",clustering_distance_cols = "correlation")
  
  col_order <- p[["tree_col"]][["order"]]
  row_order <- p[["tree_row"]][["order"]]
  data@misc[[IC_C]]$spot_top_genes_weight <- data@misc[[IC_C]]$spot_top_genes_weight[,col_order]
  data@misc[[IC_C]]$spot_top_genes_weight <- data@misc[[IC_C]]$spot_top_genes_weight[row_order,]
  
  plot_ly(
    x = colnames(data@misc[[IC_C]]$spot_top_genes_weight), y = rownames(data@misc[[IC_C]]$spot_top_genes_weight),
    z = data@misc[[IC_C]]$spot_top_genes_weight, type = "heatmap", zmin = input$slider_spot_gene_heatmap_range[1], zmax = input$slider_spot_gene_heatmap_range[2],
    colorscale = input$select_color_spot_gene_heatmap
  ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
               yaxis = list(showgrid = FALSE)
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
  title = "Plot gene weight",
  text = p("Plot of gene weight over spatial imagery")
)