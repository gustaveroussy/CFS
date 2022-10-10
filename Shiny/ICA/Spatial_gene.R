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
          uiOutput("gene_IC_main_parameters_UI"),
          uiOutput("gene_choice_main_parameters_UI"),
          uiOutput("gene_color_choice_main_parameters_UI")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "gene_plot_container",
        title = tagList(
          p("Plot IC related Genes weight", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "Spatial_gene_info",
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
        uiOutput("Spatial_gene_plot_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Spatial_gene_plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("Spatial_gene_plot")
    )
})

##----------------------------------------------------------------------------##
## Relationship tree.
##----------------------------------------------------------------------------##

output[["Spatial_gene_plot"]] <- plotly::renderPlotly({
  data <- Launch_analysis()
  
  IC_C = input[["gene_projection_IC_choice"]]
  
  if (length(input$gene_projection_gene_choice) == 1){
  plot_ly(x = data@images$slice1@coordinates$imagecol, y = -data@images$slice1@coordinates$imagerow,
          marker = list(color = data@ica[[IC_C]]$spot_top_genes_weight[input$gene_projection_gene_choice,],
                        colorscale = input$select_color_gene_projection),
          type = 'scatter', mode = "markers"
          ) %>% layout(title = input$gene_projection_gene_choice, xaxis=list(showgrid = FALSE, showticklabels=FALSE),
          yaxis = list(showgrid = FALSE, showticklabels=FALSE),
    images = list(
      plot_image()
    )
  )
  } else if (length(input$gene_projection_gene_choice) > 1) {
    plotList <- list()
    i = 1
    for ( x in input$gene_projection_gene_choice ) {
      
      plotList[[i]] <-  plot_ly(x = data@images$slice1@coordinates$imagecol, y = -data@images$slice1@coordinates$imagerow,
              marker = list(color = data@ica[[IC_C]]$spot_top_genes_weight[x,],
                            colorscale = input$select_color_gene_projection),
              type = 'scatter', mode = "markers"
      ) %>% layout(title = input$gene_projection_gene_choice, xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                   yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                   images = list(
           plot_image()
        )
      )
    
    i = i+1
    }
    subplot(plotList) %>% layout(showlegend = FALSE)
  }
})
##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
#output[["groups_tree_text"]] <- renderText({ "Data not available." })

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
  text = p("Plot of gene weight over spatial imagery")
)