output[["IC_spatial_interactions_IC_genes_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
           box(id = "interactions_IC_genes_from_graph_parameters_container",
               title = tagList(
                 "Main parameters",
                 actionButton(
                   inputId = "interactions_IC_genes_from_graph_main_parameters_info",
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
               uiOutput("plot_interactions_IC_genes_from_graph_main_parameters_UI_3"),
               uiOutput("plot_interactions_IC_genes_from_graph_main_parameters_UI_2"),
               uiOutput("plot_interactions_IC_genes_from_graph_main_parameters_UI")
           )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
           box(id = "IC_spatial_interactions_IC_genes_container",
               title = tagList(
                 p("IC Genes association", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "IC_spatial_interactions_IC_genes_info",
                   label = "info",
                   icon = NULL,
                   class = "btn-xs",
                   title = "Show additional information for this panel.",
                   style = "margin-right: 3px"
                 ),
                 shinyWidgets::dropdownButton(
                   tags$div(
                     style = "color: black !important;",
                     numericInput('IC_interactions_IC_genes_export_width','width',640,min = 1,max = NA,step = 1,width = NULL),
                     numericInput('IC_interactions_IC_genes_export_height','height',480,min = 1,max = NA,step = 1,width = NULL),
                     numericInput('IC_interactions_IC_genes_export_scale','scale',1,min = 0.001,max = NA,step = 0.01,width = NULL),
                     uiOutput("IC_interactions_IC_genes_export_UI")
                   ),
                   circle = FALSE,
                   icon = icon("download"),
                   inline = TRUE,
                   size = "xs"
                 ),
                 shinyWidgets::dropdownButton(
                   tags$div(
                     style = "color: black !important;"
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
               tags$div(
                 uiOutput("plot_interactions_IC_genes_from_graph_or_message")
               )
           )
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_spatial_interactions_IC_genes_info <- list(
  title = "Interactions",
  text = p("Interactions plots between different variables")
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_spatial_interactions_IC_genes_info"]], {
  showModal(
    modalDialog(
      IC_spatial_interactions_IC_genes_info[["text"]],
      title = IC_spatial_interactions_IC_genes_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## export button
##----------------------------------------------------------------------------##
output[["IC_interactions_IC_genes_export_UI"]] <- renderUI({
  shinyFiles::shinySaveButton(
    "IC_interactions_export",
    label = HTML("<p style='color:black;'>export</p>"),
    title = "png, jpg, jpeg, webp, svg, or pdf",
    filetype = c("pdf", "png", "jpg", "jpeg", "webp", "svg"),
    viewtype = "icon",
    class = "btn-xs",
    style = "margin-right: 3px"
  )
})
