output[["ICA_table_UI"]] <- renderUI({
  fluidRow(
    column(width = 11, offset = 1, style = "padding: 0px;",
      fileInput("import_annotation_table", "Choose CSV File",
                multiple = FALSE,
                accept = c(".csv"))
    ),
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
                 shinyWidgets::dropdownButton(
                   tags$div(
                     style = "color: black !important;",
                     textInput('add_column_annotation_table_name','Column name', value = "", width = NULL, placeholder = NULL),
                     actionButton('add_column_annotation_table','Add column', style = "color: black")
                   ),
                   circle = FALSE,
                   icon = icon("cog"),
                   inline = TRUE,
                   size = "xs"
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

observeEvent(input$import_annotation_table, {
  
  values$Annotation = NULL
  values$Annotation = as.matrix(read.csv(file = input$import_annotation_table$datapath, row.names = 1))
  
  associate_signal_with_IC()
  
})

observeEvent(input$add_column_annotation_table, {

  values$Annotation = cbind(values$Annotation,list("b" = c("")))
  
  colnames(values$Annotation)[ncol(values$Annotation)] = input$add_column_annotation_table_name
  
  associate_signal_with_IC()
  
})

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