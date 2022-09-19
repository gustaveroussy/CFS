output[["Spatial_IC_UI"]] <- renderUI({
  fluidRow(
    box(id = "IC_plot_main_parameters",
        title = tagList(
          "Main parameters",
          actionButton(
            inputId = "IC_projection_main_parameters_info",
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
        uiOutput("IC_projection_main_parameters_UI")
    ),
    box(id = "IC_plot_container",
      title = tagList(
        p("Plot IC weight", style = "padding-right: 5px; display: inline"),
        actionButton(
          inputId = "Spatial_IC_info",
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
      uiOutput("Spatial_IC_plot_or_message")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Spatial_IC_plot_or_message"]] <- renderUI({
    tagList(
      plotly::plotlyOutput("Spatial_IC_plot")
    )
})

##----------------------------------------------------------------------------##
## Relationship tree.
##----------------------------------------------------------------------------##

output[["Spatial_IC_plot"]] <- plotly::renderPlotly({
  data <- Launch_analysis()
  image = GetImage(data, mode = c("plotly"))
  image$x = 100
  image$y = 0
  image$sizex = 11100
  image$sizey = 11400
  image$sizing = "stretch"
  image$yanchor = "top"
  image$xanchor = "left"
  
  IC_C = input[["IC_projection_IC_choice"]]
  
  plot_ly(x = data@images$slice1@coordinates$imagecol, y = -data@images$slice1@coordinates$imagerow, marker = list(color = data@misc[[IC_C]]$IC_weight), type = 'scatter', mode = "markers") %>% layout(
    images = list(
      image
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
observeEvent(input[["Spatial_IC_info"]], {
  showModal(
    modalDialog(
      Spatial_IC_info[["text"]],
      title = Spatial_IC_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
Spatial_IC_info <- list(
  title = "Plot IC weight",
  text = p("Plot of IC weight over spatial imagery")
)