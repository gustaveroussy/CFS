output[["ICA_table_UI"]] <- renderUI({
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
           box(id = "table_container",
               title = tagList(
                 p("ICA table", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "table_info",
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
               DTOutput("Table_or_message"),
               column(width = 12, align="left", offset = 0, style = "padding: 0px;",
                      downloadButton("download_table", "Download Table"))
           )
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
observeEvent(input[["table_info"]], {
  showModal(
    modalDialog(
      table_info[["text"]],
      title = table_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
table_info <- list(
  title = "Annotation table",
  text = HTML("Annotation table of all the ICA:
              <ul>
                <li><b>Use</b>: Indicates whether or not the ICA is used when displaying or exporting data based on the associated cell type</li>
                <li><b>Type</b>: Indicate cell type or biological process associated with the ICA, if multiple, separate them with a comma (,) without spaces</li>
                <li><b>Annotation</b>: Complementary information for the user</li>
              </ul>
              ")
)