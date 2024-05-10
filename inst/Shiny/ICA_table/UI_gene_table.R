output[["ICA_genes_table_UI"]] <- renderUI({
  req(values$data)
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
           box(id = "gene_contributive_container",
               title = tagList(
                 p("Gene contribution", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "gene_contribution_info",
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
               selectInput(
                 "ica_gene_table_contributive_to_ic",
                 "Choose gene",
                 unique(unlist(lapply(names(values$data@misc$GeneAndStat$Contrib_gene), function(x){return(values$data@misc$GeneAndStat$Contrib_gene[[x]][,"gene"])})))
               ),
               DTOutput("out_genes_ic_relation_text")
           )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
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