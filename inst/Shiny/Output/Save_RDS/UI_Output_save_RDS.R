output[["Output_directory_UI"]] <- renderUI({
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
           box(id = "output_container",
               title = tagList(
                 p("Output", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "ouput_info",
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
               uiOutput("Output_or_message"),
               column(width = 6, align="left", offset = 0, style = "padding: 0px;",
                      downloadButton("download_RDS", "Download RDS")),
               column(width = 6, align="right", offset = 0, style = "padding: 0px;",
                      downloadButton("download_subcluster_RDS", "download subset"))
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
observeEvent(input[["ouput_info"]], {
  showModal(
    modalDialog(
      ouput_info[["text"]],
      title = ouput_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
ouput_info <- list(
  title = "Output",
  text = p("Choose to save data or subcluster data")
)