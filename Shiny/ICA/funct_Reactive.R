########################################
# Color palette
########################################

colfunc <- reactive({
  color <- colorRampPalette(c("blue", "yellow","red"))
  return(color)
})

########################################
# reactive of the image to plot by plotly
########################################

TissueCoordinates <- reactive({
  c <- GetTissueCoordinates(Launch_analysis())
  return(c)
})

observeEvent(input$input_file$datapath, {
  plot_image()
})

plot_image <- reactive({
  data <- Launch_analysis()
  
  image <- list(
    source = raster2uri(raster::as.raster(data@images$slice1@image)),
    xref = 'x',
    yref =  'y',
    sizex = dim(data@images$slice1@image)[1],
    sizey = dim(data@images$slice1@image)[2],
    sizing = 'stretch',
    opacity = 1,
    layer= 'below',
    x = -10,
    y = -4,   
    yanchor = 'top',
    xanchor = 'left'
  )
  return(image)
})

plot_image_2 <- reactive({
  data <- Launch_analysis()
  image <- list(
    source = raster2uri(raster::as.raster(flipImage(rotateFixed((data@images$slice1@image), 180), mode = "horizontal"))),
    xref = 'paper',
    yref =  'paper',
    sizex = 1,
    sizey = 1,
    sizing = 'stretch',
    opacity = 1,
    layer= 'below',
    x = 0,
    y = 1,   
    yanchor = 'top',
    xanchor = 'left'
  )
  return(image)
})

##############################
# pieplot
##############################

current_plot_pie_plot <- reactive({
  data <- Launch_analysis()
  
  #type=c(48,13,18,26)
  type=input$All_IC_chosen_projection
  
  ic_types=data@reductions$ica@cell.embeddings[,type]
  
  ic_types<-apply(ic_types,2,function(x){x=ifelse(x<=0,0,x); return(x)})
  
  #We normalize by the sum
  sum_IC<-apply(ic_types,2,function(x){x=x/sum(x); return(x)})
  #we calculate the factor of size to reduce pie size where IC are low
  sum_IC=sqrt((rowSums(sum_IC)/max(rowSums(sum_IC))))
  
  #ic_types<-apply(ic_types,1,function(x){x/sum(x); return(x)})
  # We build the final object
  ic_types<-cbind(GetTissueCoordinates(data),ic_types) %>%  cbind(.,sum_IC)
  
  ic_types <- ic_types[rowSums(ic_types[,1:length(type)+2])>0,]
  
  #We build the plot
  
  #img<-grDevices::as.raster(data@images$slice1@image)
  
  ic_types$imagerow = max(ic_types$imagerow) - ic_types$imagerow + min(ic_types$imagerow)
  
  ####
  
  fig <- plot_ly()
  
  
  for (i in 1:nrow(ic_types)) {
    r=sum_IC
    
    t = colnames(ic_types[i,1:length(type)+2][which(ic_types[i,1:length(type)+2] != 0)])
    v = round(as.double(ic_types[i,1:length(type)+2][which(ic_types[i,1:length(type)+2] != 0)])/sum(as.double(ic_types[i,1:length(type)+2][which(ic_types[i,1:length(type)+2] != 0)]))*100,2)
    
    text_final = ""
    
    for(k in 1:length(t)) {
      text_final = paste(text_final,paste0(t[k]," : ",v[k],"%<br>"))
    }
    
    
    col_coordinates = (ic_types[i,"imagecol"])/dim(data@images$slice1@image)[1]
    row_coordinates = (ic_types[i,"imagerow"])/dim(data@images$slice1@image)[2]
    
    x = c(col_coordinates-(ic_types[i,"sum_IC"]/input$pieplot_size),col_coordinates+ic_types[i,"sum_IC"]/input$pieplot_size)
    y = c(row_coordinates-(ic_types[i,"sum_IC"]/input$pieplot_size),row_coordinates+ic_types[i,"sum_IC"]/input$pieplot_size)
    
    fig <- fig %>% add_trace(type = 'pie', data = ic_types, labels = colnames(ic_types[i,1:length(type)+2][which(ic_types[i,1:length(type)+2] != 0)])
                           , values = as.double(ic_types[i,1:length(type)+2][which(ic_types[i,1:length(type)+2] != 0)]),
                           name = rownames(ic_types[i,]), domain = list(x = x, y = y),
                           showlegend = TRUE, textposition = "none", textinfo = "none",
                           text = text_final,
                           hovertemplate = paste0("%{text}",
                                                 "<extra></extra>"))
  }
  
  fig <- fig %>% layout(xaxis=list(showgrid = FALSE, showticklabels=FALSE),
                        yaxis = list(showgrid = FALSE, showticklabels=FALSE),
                        grid = list(columns = dim(data@images$slice1@image)[1], rows = dim(data@images$slice1@image)[2]),
                        images = list(
                          source = raster2uri(raster::as.raster(data@images$slice1@image)),
                          xref = 'paper',
                          yref =  'paper',
                          sizex = 1,
                          sizey = 1,
                          sizing = 'stretch',
                          opacity = 1,
                          layer= 'below',
                          x = 0,
                          y = 1,   
                          yanchor = 'top',
                          xanchor = 'left'
                          )
                        )
  
  return(fig)
})

pie_plots <- reactiveValues(button_check = 1, pie_plot = NULL)

observeEvent(input$start_pieplot, {
  if (input$start_pieplot == pie_plots$button_check) {
    pie_plots$pie_plot = current_plot_pie_plot()
    pie_plots$button_check <- input$start_pieplot + 1
  }
})
