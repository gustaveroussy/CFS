output[["ICA_top_IC_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
           box(id = "ICA_top_IC_main_parameters",
               title = tagList(
                 "Main parameters",
                 actionButton(
                   inputId = "ICA_top_IC_main_parameters_info",
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
               uiOutput("ICA_top_IC_main_parameters_UI")
           )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      box(id = "heatmap_container",
        title = tagList(
          p("IC top genes heatmap", style = "padding-right: 5px; display: inline"),
          actionButton(
            inputId = "IC_top_gene_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 3px"
          ),
          shinyFiles::shinySaveButton(
            "top_IC_heatmap_export",
            label = "export to PDF",
            title = "Export dimensional reduction to PDF file.",
            filetype = "pdf",
            viewtype = "icon",
            class = "btn-xs",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("heatmap_top_IC_column_organization_UI")
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
        uiOutput("top_IC_plot_or_message")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Drop down column organization
##----------------------------------------------------------------------------##

output[["heatmap_top_IC_column_organization_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "top_IC_column_organization",
    label = "Try to organize column based on proximity",
    value = FALSE
    )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "heatmap_top_IC_column_organization_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##
observeEvent(input[["top_IC_heatmap_export"]], {
  req(Launch_analysis())
  
  input_data <- Launch_analysis()
  
  available_storage_volumes <- getVolumes()
  
  ## open dialog to select where plot should be saved and how the file should
  ## be named
  shinyFiles::shinyFileSave(
    input,
    id = "top_IC_heatmap_export",
    roots = available_storage_volumes,
    session = session,
    restrictions = system.file(package = "base")
  )
  
  ## retrieve info from dialog
  save_file_input <- shinyFiles::parseSavePath(
    available_storage_volumes,
    input[["top_IC_heatmap_export"]]
  )
  
  ## extract specified file path
  save_file_path <- as.character(save_file_input$datapath[1])
  
  ## create heatmap
  
  ###
  input_data <- data
  Stats <- ICGeneAndStats(data = input_data)
  ngenes = 10
  list_gene = c()
  ###

  for(nics in colnames(input_data@misc[["top_gene_ICA"]])) {
    
    list_gene <- append(list_gene, head(Stats$Contrib_gene[[nics]][rev(order(abs(Stats$Contrib_gene[[nics]]$Sig))),1],n=10))
    
  }
  list_gene <- list_gene %>% unlist %>% unique
  
  paletteLength <- 50
  myColor <- colorRampPalette(c("violet","black","yellow"))(paletteLength)
  data_heat=input_data@reductions$ica@feature.loadings[as.matrix(list_gene),]
    
  myBreaks <- c(seq(min(data_heat), 0, length.out=ceiling(paletteLength/2) + 1), 
                seq(max(data_heat)/paletteLength, max(data_heat), length.out=floor(paletteLength/2)))
    
  pheatmap(data_heat,clustering_method = "ward.D",color=myColor, breaks=myBreaks,,clustering_distance_cols = "correlation",filename=paste0(save_file_path, "/Heatmap_loadings_gene_All_IC_best,",ngenes,".pdf"),width = 15,height=30)
  
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["top_IC_plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("top_gene_IC_plot")
    )
})

##----------------------------------------------------------------------------##
## Relationship tree.
##----------------------------------------------------------------------------##

output[["top_gene_IC_plot"]] <- plotly::renderPlotly({
  data <- Launch_analysis()
  
  p <- pheatmap(data@misc[["top_gene_ICA"]],clustering_method = "ward.D",clustering_distance_cols = "correlation")
  
  row_order <- p[["tree_row"]][["order"]]
  
  if (input$top_IC_column_organization == TRUE){
    col_order <- p[["tree_col"]][["order"]]
    data@misc[["top_gene_ICA"]] <- data@misc[["top_gene_ICA"]][,col_order]
  }
  
  data@misc[["top_gene_ICA"]] <- data@misc[["top_gene_ICA"]][row_order,]
  
  fig <- plot_ly(
    x = colnames(data@misc[["top_gene_ICA"]]), y = rownames(data@misc[["top_gene_ICA"]]),
    z = data@misc[["top_gene_ICA"]], type = "heatmap", zmin = input$slider_IC_top_range[1], zmax = input$slider_IC_top_range[2],
    colorscale = input$select_color_IC_top,
    hovertemplate = paste(
      "Gene: %{y:.2f%}<br>",
      "IC: %{x:.2f%}<br>",
      "Value: %{z:.2f%}",
      "<extra></extra>"
    )
  )
  
  fig <- fig %>% layout(yaxis = list(title = 'Genes', tickfont = list(size = 7)),
                        xaxis = list(title = 'IC')
                        )
})
##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
#output[["groups_tree_text"]] <- renderText({ "Data not available." })

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_top_gene_info"]], {
  showModal(
    modalDialog(
      IC_top_gene_info[["text"]],
      title = IC_top_gene_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_top_gene_info <- list(
  title = "IC top genes heatmap",
  text = p("Heatmap representation of the expression of the overall top genes overs all IC")
)