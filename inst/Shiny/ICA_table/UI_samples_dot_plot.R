output[["sample_based_dotplot_UI"]] <- renderUI({
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
           box(id = "sample_dotplot_container",
               title = tagList(
                 p("Sample based dotplot", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "sample_based_dotplot_info",
                   label = "info",
                   icon = NULL,
                   class = "btn-xs",
                   title = "Show additional information for this panel.",
                   style = "margin-right: 3px"
                 ),
               ),
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               height = NULL,
               collapsible = FALSE,
               collapsed = FALSE,
               uiOutput("sample_based_dotplot")
           )
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["sample_based_dotplot_info"]], {
  showModal(
    modalDialog(
      sample_based_dotplot_info[["text"]],
      title = sample_based_dotplot_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
sample_based_dotplot_info <- list(
  title = "Sample based dotplot",
  text = HTML("ICA distribution through samples.")
)