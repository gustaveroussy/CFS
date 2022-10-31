output[["IC_gene_heatmap_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      box(id = "IC_gene_heatmap_main_parameters",
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "IC_gene_heatmap_main_parameters_info",
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
          uiOutput("IC_gene_heatmap_slider_main_parameters_UI"),
          uiOutput("IC_gene_heatmap_color_main_parameters_UI")
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "IC_gene_heatmap_container",
        title = tagList(
          p("Build heatmap of genes related to IC", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "IC_gene_heatmap_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("heatmap_IC_gene_column_organization_UI")
            ),
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
        uiOutput("IC_gene_heatmap_plot_or_message"),
        fluidRow(
          column(width = 6, offset = 1, style = "padding: 0px;",
                 uiOutput("IC_text_output")
          ),
          column(width = 5, offset = 0, style = "padding: 0px;",
                 uiOutput("clip_all"),
                 uiOutput("clip_p")
          )
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Drop down column organization
##----------------------------------------------------------------------------##

output[["heatmap_IC_gene_column_organization_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "IC_gene_column_organization",
    label = "Try to organize column based on proximity",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "heatmap_IC_gene_column_organization_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["IC_gene_heatmap_plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("IC_gene_heatmap")
    )
})

##----------------------------------------------------------------------------##
## gene heatmap text
##----------------------------------------------------------------------------##

output[["IC_text_output"]] <- renderUI({
  IC_C = input[["IC_choice"]]
  Gene <- GeneList_heatmap_IC()
  text <- paste0("Number of genes playing in ", IC_C, " : ", length(Gene),
                 "<br><br>", "Number of genes playing positively in ", IC_C, " : ", length(Gene[Gene > 0])
                 )
  return(HTML(text))
})

##----------------------------------------------------------------------------##
## gene list
##----------------------------------------------------------------------------##

GeneList_heatmap_IC <- reactive({
  IC_C = input[["IC_choice"]]
  data <- Launch_analysis()
  GeneList <- data@misc$GeneAndStat$Contrib_gene[names(which(data@misc$GeneAndStat$Kurtosis_ICs>3))][[IC_C]]
  GeneList <- GeneList %>% as.tibble %>%arrange(desc(abs(Sig)))
  Gene <- data@reductions$ica@feature.loadings[GeneList$gene,][,IC_C]
  return(Gene)
})


##----------------------------------------------------------------------------##
## gene heatmap clipboard
##----------------------------------------------------------------------------##

# Add clipboard buttons
output$clip_all <- renderUI({
  output$clip_all <- renderUI({
    rclipButton(
      inputId = "clipa",
      label = "Gene Clipboard",
      clipText = toString(names(GeneList_heatmap_IC())), 
      icon = icon("clipboard")
    )
  })
})

if (interactive()){
  observeEvent(input$clipa, clipr::write_clip(toString(names(GeneList_heatmap_IC()))))
}

# Add clipboard buttons
output$clip_p <- renderUI({
  output$clip_p <- renderUI({
    rclipButton(
      inputId = "clipp",
      label = "Positive Gene Clipboard",
      clipText = toString(names(GeneList_heatmap_IC())), 
      icon = icon("clipboard")
    )
  })
})

if (interactive()){
  observeEvent(input$clipp, clipr::write_clip(toString(names(GeneList_heatmap_IC()[GeneList_heatmap_IC() > 0]))))
}

##----------------------------------------------------------------------------##
## gene heatmap
##----------------------------------------------------------------------------##

output[["IC_gene_heatmap"]] <- plotly::renderPlotly({
  
  data <- Launch_analysis()
  IC_C = input[["IC_choice"]]
  
  p <- pheatmap(data@misc[[IC_C]]$IC_top_genes_weight,clustering_method = "ward.D",clustering_distance_cols = "correlation")
  
  row_order <- p[["tree_row"]][["order"]]
  data@misc[[IC_C]]$IC_top_genes_weight <- data@misc[[IC_C]]$IC_top_genes_weight[row_order,]
  
  if (input$IC_gene_column_organization == TRUE){
    col_order <- p[["tree_col"]][["order"]]
    data@misc[[IC_C]]$IC_top_genes_weight <- data@misc[[IC_C]]$IC_top_genes_weight[,col_order]
  }
  
  
  plot_ly(
    x = colnames(data@misc[[IC_C]]$IC_top_genes_weight), y = rownames(data@misc[[IC_C]]$IC_top_genes_weight),
    z = data@misc[[IC_C]]$IC_top_genes_weight, type = "heatmap", zmin = input$slider_IC_gene_heatmap_range[1], zmax = input$slider_IC_gene_heatmap_range[2],
    colorscale = input$select_color_IC_gene_heatmap,
    hovertemplate = paste(
      "Gene: %{y:.2f%}<br>",
      "IC: %{x:.2f%}<br>",
      "Value: %{z:.2f%}",
      "<extra></extra>"
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
observeEvent(input[["IC_gene_heatmap_info"]], {
  showModal(
    modalDialog(
      IC_gene_heatmap_info[["text"]],
      title = IC_gene_heatmap_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_gene_heatmap_info <- list(
  title = "Plot gene weight",
  text = p("Plot of gene weight over spatial imagery")
)