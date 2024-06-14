output[["Cell_reduction_UI"]] <- renderUI({
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
           box(id = "cell_reduction_container",
               title = tagList(
                 p("Create cell annotation", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "ouput_cell",
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
               uiOutput("Output_or_message_cell_reduction")
           )
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["ouput_cell"]], {
  showModal(
    modalDialog(
      ouput_cell[["text"]],
      title = ouput_cell[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
ouput_graph_cell <- list(
  title = "Create cell annotation",
  text = p("Create a reduction for a specific annotation")
)
