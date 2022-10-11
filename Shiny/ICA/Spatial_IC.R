output[["Spatial_IC_UI"]] <- renderUI({
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
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
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
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
  if (input$IC_projection_IC_choice == "All"){
    
    fig <- plot_ly()
    
    data <- Launch_analysis()
    
    IC_C = names(data@misc[-1])
    
    df = data.frame(matrix(nrow = length(data@misc[[IC_C[1]]]$IC_weight), ncol = 0))
    
    for (i in IC_C) {
      df[i] = as.data.frame(data@misc[[i]]$IC_weight)
    }
    
    df[df<0] <- 0
    
    rownames(df) <- rownames(as.data.frame(data@misc[[IC_C[1]]]$IC_weight))
    
    df <- rowPercents(df, digits=1)
    
    for(i in 1:length(data@misc[[IC_C[1]]]$IC_weight)) {
      fig <- fig %>% add_pie(data = df, labels = colnames(df[,1:(ncol(df)-2)]), values = df["AAACAGTGTTCCTGGG-1",1:(ncol(df)-2)],
                             name = "Cut", domain = list(x = c(0, 0.4), y = c(0.4, 1)))
    }

    fig <- fig %>% layout(xaxis=list(showgrid = FALSE, zeroline = FALSE, showticklabels=FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels=FALSE),
                 images = list(
                   plot_image()
                 )
    )
    print(fig)
    typeof(df["AAACAGTGTTCCTGGG-1",])
  }else{
    data <- Launch_analysis()
    
    IC_C = input[["IC_projection_IC_choice"]]
    
    plot_ly(x = data@images$slice1@coordinates$imagecol, y = -data@images$slice1@coordinates$imagerow,
            marker = list(color = data@misc[[IC_C]]$IC_weight,
                          colorscale = input$select_color_IC_projection),
            type = 'scatter', mode = "markers",
            ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                         yaxis = list(showgrid = FALSE, showticklabels=FALSE),
      images = list(
        plot_image()
      )
    )
  }
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
