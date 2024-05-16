output[["IC_spatial_interactions_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
           box(id = "interactions_from_graph_parameters_container",
               title = tagList(
                 "Main parameters",
                 actionButton(
                   inputId = "interactions_from_graph_main_parameters_info",
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
               uiOutput("plot_interactions_from_graph_main_parameters_UI")
           )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
           box(id = "IC_spatial_interactions_container",
               title = tagList(
                 p("IC interactions", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "IC_spatial_interactions_info",
                   label = "info",
                   icon = NULL,
                   class = "btn-xs",
                   title = "Show additional information for this panel.",
                   style = "margin-right: 3px"
                 ),
                 shinyWidgets::dropdownButton(
                   tags$div(
                     style = "color: black !important;"# ,
                     # numericInput('top_IC_heatmap_export_width','width',640,min = 1,max = NA,step = 1,width = NULL),
                     # numericInput('top_IC_heatmap_export_height','height',480,min = 1,max = NA,step = 1,width = NULL),
                     # numericInput('top_IC_heatmap_export_scale','scale',1,min = 0.001,max = NA,step = 0.01,width = NULL),
                     # uiOutput("top_IC_heatmap_export_UI")
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
                 uiOutput("plot_interactions_from_graph_or_message")
               )
           )
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
IC_distance_info <- list(
  title = "Distances",
  text = p("show distance graphs between different variables")
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["IC_distance_info"]], {
  showModal(
    modalDialog(
      IC_distance_info[["text"]],
      title = IC_distance_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
