output[["IC_distances_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
           box(id = "IC_distances_parameters_container",
               title = tagList(
                 "Main parameters",
                 actionButton(
                   inputId = "IC_distance_main_parameters_info",
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
               uiOutput("IC_distance_main_parameters_UI")
           ),
           box(id = "IC_distances_parameters_2_container",
               title = tagList(
                 "Display parameters"
               ),
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               height = NULL,
               collapsible = FALSE,
               collapsed = FALSE,
               uiOutput("IC_distance_main_parameters_UI_2")
           )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
           box(id = "IC_distances_container",
               title = tagList(
                 p("Ic distances", style = "padding-right: 5px; display: inline"),
                 actionButton(
                   inputId = "IC_distance_info",
                   label = "info",
                   icon = NULL,
                   class = "btn-xs",
                   title = "Show additional information for this panel.",
                   style = "margin-right: 3px"
                 ),
                 shinyWidgets::dropdownButton(
                   tags$div(
                     style = "color: black !important;",
                     numericInput('IC_distances_export_width','width',640,min = 1,max = NA,step = 1,width = NULL),
                     numericInput('IC_distances_export_height','height',480,min = 1,max = NA,step = 1,width = NULL),
                     numericInput('IC_distances_export_scale','scale',1,min = 0.001,max = NA,step = 0.01,width = NULL),
                     uiOutput("IC_distances_export_UI")
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
                 uiOutput("IC_distances_plot_or_message")
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


##----------------------------------------------------------------------------##
## export button
##----------------------------------------------------------------------------##
output[["IC_distances_export_UI"]] <- renderUI({
  shinyFiles::shinySaveButton(
    "IC_distances_export",
    label = HTML("<p style='color:black;'>export</p>"),
    title = "png, jpg, jpeg, webp, svg, or pdf",
    filetype = c("pdf", "png", "jpg", "jpeg", "webp", "svg"),
    viewtype = "icon",
    class = "btn-xs",
    style = "margin-right: 3px"
  )
})