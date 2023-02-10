output[["Marker_table_UI"]] <- renderUI({
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
           box(id = "marker_table_container",
               title = tagList(
                 p("Marker table", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "marker_table_info",
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
               DTOutput("Marker_table_or_message"),
               column(width = 12, align="left", offset = 0, style = "padding: 0px;",
                      downloadButton("download_marker_table", "Download Table"))
           )
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["marker_table_info"]], {
  showModal(
    modalDialog(
      marker_table_info[["text"]],
      title = marker_table_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
marker_table_info <- list(
  title = "Annotation table",
  text = HTML("Annotation table of all the ICA:
              <ul>
                <li><b>Use</b>: Indicates whether or not the ICA is used when displaying or exporting data based on the associated cell type</li>
                <li><b>Type</b>: Indicate cell type or biological process associated with the ICA, if multiple, separate them with a comma (,) without spaces</li>
                <li><b>Annotation</b>: Complementary information for the user</li>
              </ul>
              ")
)