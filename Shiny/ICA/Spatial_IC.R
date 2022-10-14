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
    data <- Launch_analysis()
    
    #type=c(48,13,18,26)
    type=c(1,2,3,4)
    
    ic_types=data@reductions$ica@cell.embeddings[,type]
    
    ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
    
    #We normalize by the sum
    sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
    #we calculate the factor of size to reduce pie size where IC are low
    sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
    
    #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
    # We build the final object
    ic_types<-cbind(data@images$slice1@coordinates,ic_types) %>%  cbind(.,sum_IC)
    
    ic_types <- ic_types[rowSums(ic_types[,5:2+length(type)])>0,]
    
    #We build the plot
    
    picture_row = nrow(data@images$slice1@image)
    picture_col = ncol(data@images$slice1@image)
    plot_row = max(ic_types["row"])
    plot_col = max(ic_types["col"])
    
    fig <- plot_ly(type = 'pie')
    
    for (i in 1:nrow(ic_types)) {
      r=sum_IC
      
      col_coordinates = (ic_types[i,"col"])/max(ic_types["col"])
      row_coordinates = (ic_types[i,"row"])/max(ic_types["row"])
      
      x = c(col_coordinates-(ic_types[i,"sum_IC"]/20),col_coordinates+ic_types[i,"sum_IC"]/20)
      y = c(row_coordinates-(ic_types[i,"sum_IC"]/20),row_coordinates+ic_types[i,"sum_IC"]/20)
      
      fig <- fig %>% add_pie(data = ic_types, labels = colnames(ic_types[i,5:2+length(type)][which(ic_types[i,5:2+length(type)] != 0)])
                             , values = as.double(ic_types[i,5:2+length(type)][which(ic_types[i,5:2+length(type)] != 0)]),
                             name = rownames(ic_types[i,]), domain = list(x = x, y = y),
                             showlegend = TRUE, text = NULL, textposition = "none")
    }
    
    fig <- fig %>% add_trace(type="image", source = raster2uri(raster::as.raster(flipImage(rotateFixed(data@images$slice1@image, 180), mode = "horizontal")))
      
    )
      
    fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                          yaxis = list(showgrid = FALSE, showticklabels=FALSE),
  #                        images = list(
  #                          source = raster2uri(raster::as.raster(flipImage(rotateFixed(data@images$slice1@image, 180), mode = "horizontal"))),
  #                          xref = 'paper',
  #                          yref =  'paper',
  #                          sizex = 1,
  #                          sizey = 1,
  #                         sizing = 'stretch',
  #                          opacity = 1,
  #                          layer= 'below',
  #                          x = 0,
  #                          y = 1,   
 #                           yanchor = 'top',
  #                          xanchor = 'left'
  #                        ),
                          scene = list(
                            aspectratio=list(x = 1, y = 0.5)
                          )
    )
    
    print(fig)
    return(fig)
    
    #######
    #fig <- plot_ly()
    
    #data <- Launch_analysis()
    
    #IC_C = names(data@misc[-1])
    
    #df = data.frame(matrix(nrow = length(data@misc[[IC_C[1]]]$IC_weight), ncol = 0))
    
    #for (i in IC_C) {
    #  df[i] = as.data.frame(data@misc[[i]]$IC_weight)
    #}
    
    #df[df<0] <- 0
    
    #rownames(df) <- rownames(as.data.frame(data@misc[[IC_C[1]]]$IC_weight))
    
    #df <- rowPercents(df, digits=1)
    
    #for(i in 1:length(data@misc[[IC_C[1]]]$IC_weight)) {
    #  fig <- fig %>% add_pie(data = df, labels = colnames(df[,1:(ncol(df)-2)]), values = df["AAACAGTGTTCCTGGG-1",1:(ncol(df)-2)],
    #                         name = "Cut", domain = list(x = c(0, 0.4), y = c(0.4, 1)))
    #}

    #fig <- fig %>% layout(xaxis=list(showgrid = FALSE, zeroline = FALSE, showticklabels=FALSE),
    #             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels=FALSE),
    #             images = list(
    #               plot_image()
    #             )
    #)
    #print(fig)
    #typeof(df["AAACAGTGTTCCTGGG-1",])
  }else{
    data <- Launch_analysis()
    
    IC_C = input[["IC_projection_IC_choice"]]
    
    if(input$select_color_IC_projection != "Range"){
      plot_ly(x = TissueCoordinates()[,"imagecol"], y = -TissueCoordinates()[,"imagerow"],
              marker = list(color = data@misc[[IC_C]]$IC_weight,
                            colorscale = input$select_color_IC_projection,
                            cmin = input$slider_IC_spatial_range[1], cmax=input$slider_IC_spatial_range[2]),
              type = 'scatter', mode = "markers",
              text = data@misc[[IC_C]]$IC_weight,
              customdata = names(data@misc[[IC_C]]$IC_weight),
              hovertemplate = paste("Cell : %{customdata}<br>",
                                    "Expression: %{text}",
                                    "<extra></extra>")
      ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                   yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                   showlegend = TRUE,
                   images = list(
                     plot_image()
                   )
      )
    } else {
      plot_ly(x = TissueCoordinates()[,"imagecol"], y = -TissueCoordinates()[,"imagerow"],
              marker = list(color = data@misc[[IC_C]]$IC_weight,
                            colors = colfunc(),
                            cmin = input$slider_IC_spatial_range[1], cmax=input$slider_IC_spatial_range[2]),
              type = 'scatter', mode = "markers",
              text = data@misc[[IC_C]]$IC_weight,
              customdata = names(data@misc[[IC_C]]$IC_weight),
              hovertemplate = paste("Cell : %{customdata}<br>",
                                    "Expression: %{text}",
                                    "<extra></extra>")
      ) %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                   yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                   showlegend = TRUE,
                   images = list(
                     plot_image()
                   )
      ) # add trace, show legend
    }
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
