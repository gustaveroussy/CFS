##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["Output_or_message"]] <- renderUI({

  fluidRow(
    column(width = 4,
           "4"
    ),
    column(width = 6,
           "3 offset 2"
    )
  )

#  tagList(
#    plotly::plotlyOutput("Plot", height = "auto", width = 'auto')
#  )
})