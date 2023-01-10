output[["Plot_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
           box(id = "Plot_main_parameters_UI",
               title = tagList(
                 "Main parameters",
                 actionButton(
                   inputId = "Plot_main_parameters_info",
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
               uiOutput("Plot_type_UI"),
               uiOutput("Plot_main_parameters_UI"),
               uiOutput("start_plot_UI")
           )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "Plot_container",
        title = tagList(
          p("Plot", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "Plot_info",
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
            size = "xs",
            uiOutput("ggplot_scatter_pie_UI")
          )
        ),
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        height = NULL,
        collapsible = TRUE,
        collapsed = FALSE,
        uiOutput("Plot_or_message")
      )
    )
  )
})

output[["ggplot_scatter_pie_UI"]] <- renderUI({
  tagList(
    checkboxInput("ggplot_scatter_pie", label = HTML("<font color=\"#FFFFFF\">Use ggplot scatter pie</font>"), value = FALSE)
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "ggplot_scatter_pie_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("Plot", height = "auto", width = 'auto')
    )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["Plot_info"]], {
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
  title = "Plot",
  text = p("Window displaying UMAP representation of data")
)