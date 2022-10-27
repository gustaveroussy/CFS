output[["IC_enrichment_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      box(id = "IC_enrichment_main_parameters",
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "IC_enrichment_main_parameters_info",
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
          uiOutput("IC_enrichment_main_parameters_UI"),
          uiOutput("IC_enrichment_slider_main_parameters_UI"),
          uiOutput("IC_enrichment_color_main_parameters_UI")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "IC_enrichment_container",
        title = tagList(
          p("Build bar chart of enrichment", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "IC_enrichment_info",
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
        uiOutput("IC_enrichment_plot_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["IC_enrichment_plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("IC_enrichment")
    )
})

##----------------------------------------------------------------------------##
## Relationship tree.
##----------------------------------------------------------------------------##

output[["IC_enrichment"]] <- plotly::renderPlotly({
  
  data <- Launch_analysis()
  IC_C = input[["IC_enrichment_IC_choice"]]
  database_C = input[["IC_enrichment_database_choice"]]
  
  table <- data@misc[[IC_C]]$en[[database_C]]
  
  number_of_genes <- table["Overlap"]
  
  table["Overlap"] <- lapply(table["Overlap"], sub, pattern="/.*", replacement="")
  
  x <- table["Overlap"][1:30,]
  y <- table["Term"][1:30,]
  
  fig <- plot_ly(type = "bar", orientation = 'h')
  
  fig <- fig %>% add_trace(
    x = x, 
    y = y,
    hovertext = paste0(y,"\nnumber of genes: ", number_of_genes[1:30,],"\nP-value: ",table["Adjusted.P.value"][1:30,]),
    hoverinfo = 'text', 
    showlegend=FALSE,
    marker = list(color = table["Adjusted.P.value"][1:30,], colorscale = "Viridis",
                  colorbar = list(title = "P-value"), showscale = TRUE, reversescale=TRUE)
  )
  
  fig <- fig %>% layout(yaxis = list(autorange = "reversed", title = 'Enrichment', tickfont = list(size = 7)),
               xaxis = list(title = 'Nb genes', tickfont = list(size = 10))
  )
  
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
#output[["groups_tree_text"]] <- renderText({ "Data not available." })

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_enrichment_info"]], {
  showModal(
    modalDialog(
      IC_enrichment_info[["text"]],
      title = IC_enrichment_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_enrichment_info <- list(
  title = "Plot gene weight",
  text = p("Plot of gene weight over spatial imagery")
)