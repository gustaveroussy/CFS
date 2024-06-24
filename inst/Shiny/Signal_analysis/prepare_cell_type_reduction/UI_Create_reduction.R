output[["Auto_annotation_UI"]] <- renderUI({
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
           box(id = "cell_auto_annotation_container",
               title = tagList(
                 p("Auto Annotation", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "ouput_graph_cell",
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
               uiOutput("output_graph_auto_annotation"),
               uiOutput("output_graph_auto_annotation_2")
           )
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["ouput_graph_cell"]], {
  showModal(
    modalDialog(
      ouput_graph_cell[["text"]],
      title = ouput_graph_cell[["title"]],
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

