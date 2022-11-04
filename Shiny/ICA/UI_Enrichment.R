output[["IC_enrichment_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      box(id = "IC_enrichment_main_parameters",
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "IC_enrichment_main_parameters_info",
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
          uiOutput("IC_enrichment_main_parameters_UI"),
          uiOutput("IC_enrichment_color_main_parameters_UI"),
          uiOutput("IC_enrichment_display_number_main_parameters_UI"),
          uiOutput("IC_enrichment_p_n_main_parameters_UI")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "IC_enrichment_container",
        title = tagList(
          p("Build bar chart of enrichment", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "IC_enrichment_info",
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
        uiOutput("IC_enrichment_plot_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["IC_enrichment_plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("IC_enrichment")
    )
})

##----------------------------------------------------------------------------##
## Relationship tree.
##----------------------------------------------------------------------------##

output[["IC_enrichment"]] <- plotly::renderPlotly({
  
  data <- Launch_analysis()
  IC_C = input[["IC_choice"]]
  database_C = input[["IC_enrichment_database_choice"]]
  p_n = input[["p_n_enrichment"]]
  
  table <- data@misc[[IC_C]][[p_n]][[database_C]]
  
  number_of_genes <- table["Overlap"]
  
  table["Overlap"] <- lapply(table["Overlap"], sub, pattern="/.*", replacement="")
  
  number_enrichr <- input$enrichment_disp_number
  
  # create list of genes
  for (k in 1:length(rownames(table))) {
    gene_text = c()
    u <- unlist(strsplit(table["Genes"][k,][1], split = ";"))
    for (v in 1:length(u)) {
      gene_text <- append(gene_text,u[v])
      if (v %% 5 == 0) {
        gene_text <- append(gene_text,"<br>")
      }
    }
    u <- paste(gene_text,collapse = ",")
    
    # Replace all characters occurrence in a string
    u <- gsub('<br>,','<br>',u)
    
    table["Genes"][k,] <- u
  }
  
  genes <- table["Genes"]
  
  x <- table["Overlap"][1:number_enrichr,]
  y <- table["Term"][1:number_enrichr,]
  
  fig <- plot_ly(type = "bar", orientation = 'h')
  
  fig <- fig %>% add_trace(
    x = x, 
    y = y,
    hovertext = paste0(y,"\nnumber of genes: ", number_of_genes[1:number_enrichr,],"\nP-value: ",table["Adjusted.P.value"][1:number_enrichr,]
                       , "\ngenes : ", genes[1:number_enrichr,]),
    hoverinfo = 'text', 
    showlegend=FALSE,
    marker = list(color = table["Adjusted.P.value"][1:number_enrichr,], colorscale = input$select_color_IC_enrichment,
                  colorbar = list(title = "P-value", exponentformat = "power"), showscale = TRUE, reversescale=TRUE)
  )
  
  fig <- fig %>% layout(yaxis = list(autorange = "reversed", title = 'Enrichment', tickfont = list(size = 7)),
               xaxis = list(title = 'Nb genes', tickfont = list(size = 10))
  )
  
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
#output[["groups_tree_text"]] <- renderText({ "Data not available." })

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_enrichment_info"]], {
  showModal(
    modalDialog(
      IC_enrichment_info[["text"]],
      title = IC_enrichment_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_enrichment_info <- list(
  title = "Plot gene weight",
  text = p("Plot of gene weight over spatial imagery")
)