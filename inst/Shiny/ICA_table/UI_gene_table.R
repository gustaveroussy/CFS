output[["ICA_genes_table_UI"]] <- renderUI({
  fluidRow(
    column(width = 12, offset = 0, style = "padding: 0px;",
           box(id = "table_gene_container",
               title = tagList(
                 p("IC gene weights", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "table_gene_info",
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
               DTOutput("ICA_genes_table")
           )
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["table_gene_info"]], {
  showModal(
    modalDialog(
      table_gene_info[["text"]],
      title = table_gene_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
table_gene_info <- list(
  title = "ICA/Gene table",
  text = HTML("
    Table displaying IC signals and their associated genes.
    "
  )
)