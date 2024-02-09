plotly_colorscale <- function(colorscale = "D"){
  
  if(input$select_color_visualisation_projection %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
    l = colorscale
  }else{
    #prepare colorscales
    l = list()
    se = seq(0, 1, (1/(ncol(values$data)-1)))
    col = viridis_pal(option = colorscale)(ncol(values$data))
    for(i in 1:length(se)){
      l[[i]] = c(se[i],col[i])
    }
  }
  return(l)
}